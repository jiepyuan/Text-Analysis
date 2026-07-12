library(rjags)
library(coda)
library(loo)
library(tidyverse)

script_dir <- dirname(
  rstudioapi::getActiveDocumentContext()$path
)

out_dir <- file.path(script_dir, "result")

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

message("Results will be saved to: ", out_dir)


prior_gcm_1 <- 0.184460806               
prior_gcm_2 <- 0.000680074               
prior_gcm_3 <- -0.02 
prior_gcm_4 <- 6.718749        
prior_gcm_5 <- 0.013587     

# (1) load and reshape the data -------------------------------------------

# 1. Convert to long format 
dat <- read.csv("gemma_separate_pos_avg_txt_emoji.csv")
dat$id <- 1:nrow(dat) 

select_dat <- dat %>% 
  select(id, t0:t14)


long_dat <- select_dat %>%
  pivot_longer(
    cols = starts_with("t"), 
    names_to = "time_raw", 
    values_to = "sentiment"
  ) %>%
  mutate(time = as.numeric(gsub("t", "", time_raw))) %>%
  filter(!is.na(sentiment)) 

# 2. Redefine the AR(1) Component
long_dat <- long_dat %>%
  group_by(id) %>%
  arrange(time) %>%
  mutate(
    prev_time = lag(time),
    actual_lag_val = lag(sentiment),
    has_lag = ifelse(!is.na(actual_lag_val) & (time == prev_time + 1), 1L, 0L),
    Y_lag2 = ifelse(has_lag == 1, actual_lag_val, 0)
  ) %>%
  filter(n() >= 3) %>%
  ungroup() %>%
  mutate(id_idx = as.numeric(as.factor(id)))


# 3. prepare the data

R_mat <- diag(2)

jags_data_growth <- list(
  Y       = long_dat$sentiment,
  time    = long_dat$time,       
  subject = long_dat$id_idx,
  N_obs   = nrow(long_dat),
  N_subj  = max(long_dat$id_idx),
  R       = R_mat,
  prior_gcm_4 = prior_gcm_4,
  prior_gcm_5 = prior_gcm_5
)


# 2. Compute the empirical Bayes initial precision matrix from the LME results
sigma_int   <- prior_gcm_1       
sigma_slope <- prior_gcm_2       
rho         <- prior_gcm_3

# Compute Cov(Intercept, Slope)
cov_int_slope <- rho * sigma_int * sigma_slope

# Construct the covariance matrix Sigma
Sigma_emp <- matrix(c(sigma_int^2, cov_int_slope, 
                      cov_int_slope, sigma_slope^2), nrow = 2)

# JAGS requires a precision matrix, which is the inverse of Sigma
Omega_init <- solve(Sigma_emp)


# (2) define the model ----------------------------------------------------


model_string_growth <- "
model {
  # --- Level 1: Observation Level ---
  for (i in 1:N_obs) {
    Y[i] ~ dnorm(mu[i], tau_epsilon)
    Y_pred[i] ~ dnorm(mu[i], tau_epsilon)

    # y_it = beta0i + beta1i * time_t + epsilon_it
    mu[i] <- beta0i[subject[i]] + beta1i[subject[i]] * time[i]
    
    sq_err[i] <- pow(Y[i] - Y_pred[i], 2)
    
    # Compute the pointwise log-likelihood
    log_lik[i] <- -0.5 * log(2 * 3.14159265359) + 0.5 * log(tau_epsilon) - 0.5 * tau_epsilon * pow(Y[i] - mu[i], 2)
  }
  
  mse <- sum(sq_err[1:N_obs]) / N_obs
  rmse <- sqrt(mse)

  # --- Level 2: Individual Level (Multivariate Normal) ---
  for (j in 1:N_subj) {
    # beta0i = gamma00 + u0i, beta1i = gamma10 + u1i
    beta_i[j, 1:2] ~ dmnorm(gamma_mean[1:2], Tau_u[1:2, 1:2])
    beta0i[j] <- beta_i[j, 1]
    beta1i[j] <- beta_i[j, 2]
    u0i[j] <- beta0i[j] - gamma00
    u1i[j] <- beta1i[j] - gamma10
  }

  # --- Priors ---
  
  # Population-level mean parameters: gamma00, gamma10
  gamma00 ~ dnorm(prior_gcm_4, 0.1)
  gamma10 ~ dnorm(prior_gcm_5, 0.1)
  gamma_mean[1] <- gamma00
  gamma_mean[2] <- gamma10
  
  # Precision matrix for the random effects (u0i, u1i)
  # JAGS dmnorm requires a precision matrix, i.e., the inverse of Sigma_u
  Tau_u[1:2, 1:2] ~ dwish(R[1:2, 1:2], 3)
  
  # Covariance parameters: sigma0^2, sigma1^2, sigma01
  Sigma_u[1:2, 1:2] <- inverse(Tau_u[,])
  sigma0_sq <- Sigma_u[1,1]
  sigma1_sq <- Sigma_u[2,2]
  sigma01 <- Sigma_u[1,2]
  rho_u <- sigma01 / sqrt(sigma0_sq * sigma1_sq)

  # Variance parameter for epsilon_it
  sigma_epsilon ~ dunif(0, 10)
  tau_epsilon <- 1 / pow(sigma_epsilon, 2)
}
"


# (3) estimate the parameters ---------------------------------------------

set.seed(20260224)

# Set each subject's initial values to the population means
alpha_init_1 <- matrix(rep(c(prior_gcm_4, 0.016420), max(long_dat$id_idx)), 
                       ncol = 2, byrow = TRUE)

alpha_init_2 <- matrix(rep(c(prior_gcm_4/2, 0.03), max(long_dat$id_idx)), 
                       ncol = 2, byrow = TRUE)

inits_growth_1 <- list(
  gamma00       = prior_gcm_4,
  gamma10       = prior_gcm_5 ,
  sigma_epsilon = 3.333,
  Tau_u         = Omega_init,
  beta_i        = alpha_init_1
)

inits_growth_2 <- list(
  gamma00       = prior_gcm_4/2,
  gamma10       = prior_gcm_5/2,
  sigma_epsilon = 2.8,
  Tau_u         = diag(c(1 / sigma_int^2, 1 / sigma_slope^2)),
  beta_i        = alpha_init_2
)

jags_mod_growth <- jags.model(
  file     = textConnection(model_string_growth),
  data     = jags_data_growth,
  inits    = list(inits_growth_1, inits_growth_2), 
  n.chains = 2,            
  n.adapt  = 5000          
)

# (4) sampling and monitoring ---------------------------------------------

# Burn-in
update(jags_mod_growth, n.iter = 10000)

# Parameters to monitor, including the variance components transformed in the model
params_growth <- c("gamma00", "gamma10", "sigma_epsilon", "sigma0_sq", 
                   "sigma1_sq", "sigma01", "rho_u", "rmse", "log_lik")

# Sampling
samples_growth <- coda.samples(
  model          = jags_mod_growth,
  variable.names = params_growth,
  n.iter         = 50000 
)

diag_params_growth <- varnames(samples_growth)
diag_params_growth <- diag_params_growth[!grepl("^log_lik\\[", diag_params_growth)]
diag_params_growth <- diag_params_growth[!grepl("^rmse$", diag_params_growth)]
samples_diag_growth <- samples_growth[, diag_params_growth]
gelman_results_growth <- gelman.diag(samples_diag_growth, multivariate = TRUE)
print(gelman_results_growth)

# Plot selected trace plots and save them directly in out_dir.
trace_params_growth <- c("gamma00", "gamma10", "sigma_epsilon", "sigma0_sq",
                         "sigma1_sq", "sigma01", "rho_u")

for (p in trace_params_growth) {
  safe_p <- gsub("\\[|\\]", "_", p)
  png(file.path(out_dir, paste0("traceplot_", safe_p, ".png")),
      width = 800, height = 600)
  traceplot(samples_diag_growth[, p], main = paste("Traceplot of", p))
  dev.off()
}

# (5) extract parameters --------------------------------------------------

# View the parameter summary and RMSE
summary_growth <- summary(samples_growth)
rmse_growth <- summary_growth$statistics["rmse", "Mean"]
cat("Growth Curve Model RMSE:", rmse_growth, "\n")


# (6) Compute LOO and WAIC ------------------------------------------------
# Extract the log-likelihood matrix
samples_matrix_growth <- as.matrix(samples_growth)
log_lik_cols_growth   <- grep("^log_lik\\[", colnames(samples_matrix_growth))
log_lik_matrix_growth <- samples_matrix_growth[, log_lik_cols_growth]

# Compute WAIC and LOO
waic_growth <- waic(log_lik_matrix_growth)
loo_growth  <- loo(log_lik_matrix_growth)

# Print the results
print(waic_growth)
print(loo_growth)

# Save the results
save(
  gelman_results_growth,
  waic_growth,
  loo_growth,
  rmse_growth,
  file = file.path(out_dir, "results_GCM.RData")
)

