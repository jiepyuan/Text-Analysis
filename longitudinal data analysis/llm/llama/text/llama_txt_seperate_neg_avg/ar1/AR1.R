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

prior_1_ar1 <- 3.03377074                                   
prior_2_ar1 <- -0.04048415           

# (1) load and reshape the data -------------------------------------------
# 1. Convert to long format 
dat <- read.csv("llama_separate_neg_avg_txt.csv")
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
    Y_lag = ifelse(has_lag == 1, actual_lag_val, 0)
  ) %>%
  filter(n() >= 3) %>%
  ungroup() %>%
  mutate(id_idx = as.numeric(as.factor(id)))


# 3. prepare the data

jags_data_ar1 <- list(
  Y       = long_dat$sentiment,
  Y_lag   = long_dat$Y_lag,      
  has_lag = long_dat$has_lag,
  subject = long_dat$id_idx,
  N_obs   = nrow(long_dat),
  N_subj  = max(long_dat$id_idx),
  prior_1_ar1 = prior_1_ar1,
  prior_2_ar1 = prior_2_ar1
)


# (2) define the model ----------------------------------------------------


model_string_ar1 <- "
model {
  # --- Level 1: Observation Level ---
  for (i in 1:N_obs) {
    Y[i] ~ dnorm(mu[i], tau_epsilon)
    
    # Posterior prediction for computing RMSE
    Y_pred[i] ~ dnorm(mu[i], tau_epsilon)

    # AR(1): y_it = alpha_i + phi * y_i,t-1 + epsilon_it
    mu[i] <- alpha[subject[i]] + phi * Y_lag[i] * has_lag[i]
    
    # Squared prediction error
    sq_err[i] <- pow(Y[i] - Y_pred[i], 2)
    
    # Compute the log-likelihood for WAIC and LOO
    log_lik[i] <- -0.5 * log(2 * 3.14159265359) + 0.5 * log(tau_epsilon) - 0.5 * tau_epsilon * pow(Y[i] - mu[i], 2)
  }
  
  mse <- sum(sq_err[1:N_obs]) / N_obs
  rmse <- sqrt(mse)

  # --- Level 2: Individual Level (alpha_i) ---
  for (j in 1:N_subj) {
    alpha[j] ~ dnorm(mu_alpha, tau_alpha)
  }

  # --- Priors (using the lme results as prior centers) ---
  
  # 1. Population mean prior for alpha_i
  mu_alpha ~ dnorm(prior_1_ar1, 0.1) 
  
  # 2. Autoregressive coefficient prior
  phi ~ dnorm(prior_2_ar1 , 0.1) 

  # 3. Random-effect standard deviation prior (uniform to allow a reasonable search range)
  sigma_epsilon ~ dunif(0, 10)
  sigma_epsilon_sq <- pow(sigma_epsilon, 2)
  tau_epsilon <- 1 / pow(sigma_epsilon, 2)

  sigma_alpha ~ dunif(0, 10)
  tau_alpha <- 1 / pow(sigma_alpha, 2)
}
"


# (3) estimate the parameters ---------------------------------------------

set.seed(20260224)

inits_ar1_1 <- list(
  mu_alpha      = prior_1_ar1,
  phi           = prior_2_ar1  ,
  sigma_alpha   = 1.691,
  sigma_epsilon = 3.334
)

inits_ar1_2 <- list(
  mu_alpha      = prior_1_ar1/2,
  phi           = prior_2_ar1/2,
  sigma_alpha   = 1.2,
  sigma_epsilon = 2.8
)

jags_mod_ar1 <- jags.model(
  file     = textConnection(model_string_ar1),
  data     = jags_data_ar1,
  inits    = list(inits_ar1_1, inits_ar1_2), 
  n.chains = 2,            
  n.adapt  = 5000          
)


# (4) sampling and monitoring ---------------------------------------------


update(jags_mod_ar1, n.iter = 10000) # Burn-in

# Monitor the model parameters: alpha_i, phi, and sigma_epsilon^2
# mu_alpha and sigma_alpha are the hierarchical prior parameters for alpha_i.
params_ar1 <- c("mu_alpha", "phi", "sigma_epsilon", "sigma_alpha", "rmse", "log_lik")

samples_ar1 <- coda.samples(
  model          = jags_mod_ar1,
  variable.names = params_ar1,
  n.iter         = 50000 # Adjust as needed; the AR1 model runs much faster than the new model
)

diag_params_ar1 <- varnames(samples_ar1)
diag_params_ar1 <- diag_params_ar1[!grepl("^log_lik\\[", diag_params_ar1)]
diag_params_ar1 <- diag_params_ar1[!grepl("^rmse$", diag_params_ar1)]
samples_diag_ar1 <- samples_ar1[, diag_params_ar1]
gelman_results_ar1 <- gelman.diag(samples_diag_ar1, multivariate = TRUE)
print(gelman_results_ar1)

# Plot selected trace plots and save them directly in out_dir.
trace_params_ar1 <- c("mu_alpha", "phi", "sigma_epsilon", "sigma_alpha")

for (p in trace_params_ar1) {
  safe_p <- gsub("\\[|\\]", "_", p)
  png(file.path(out_dir, paste0("traceplot_", safe_p, ".png")),
      width = 800, height = 600)
  traceplot(samples_diag_ar1[, p], main = paste("Traceplot of", p))
  dev.off()
}

# (5) extract parameters --------------------------------------------------

summary_ar1 <- summary(samples_ar1)
rmse_ar1 <- summary_ar1$statistics["rmse", "Mean"]
cat("AR(1) Model RMSE:", rmse_ar1, "\n")


# (6) Compute LOO and WAIC ------------------------------------------------

samples_matrix_ar1 <- as.matrix(samples_ar1)
log_lik_cols_ar1   <- grep("^log_lik\\[", colnames(samples_matrix_ar1))
log_lik_matrix_ar1 <- samples_matrix_ar1[, log_lik_cols_ar1]

waic_ar1 <- waic(log_lik_matrix_ar1)
loo_ar1  <- loo(log_lik_matrix_ar1)

print(waic_ar1)
print(loo_ar1)

save(
  gelman_results_ar1,
  waic_ar1,
  loo_ar1,
  rmse_ar1,
  file = file.path(out_dir, "results_AR1.RData")
)
