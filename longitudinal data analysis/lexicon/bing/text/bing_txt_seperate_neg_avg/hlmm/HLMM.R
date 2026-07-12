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

prior_hlmm_1 <- -2.069372    
prior_hlmm_2 <- 1.5822
prior_hlmm_3 <- 2*-0.02534729  

# (1) load and reshape the data -------------------------------------------

# 1. Convert to long format 
dat <- read.csv("bing_separate_neg_avg_txt.csv")
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

jags_data_hlmm <- list(
  Y       = long_dat$sentiment,
  time    = long_dat$time,        
  subject = long_dat$id_idx,
  N_obs   = nrow(long_dat),
  N_subj  = max(long_dat$id_idx),
  
  prior_hlmm_1 = prior_hlmm_1,
  prior_hlmm_2 = prior_hlmm_2,
  prior_hlmm_3 = prior_hlmm_3
)


# (2) define the model ----------------------------------------------------

model_string_hlmm <- "
model {
  # --- Level 1: Observation Level ---
  for (i in 1:N_obs) {
    # variance model: log(sigma_t^2) = gamma0 + gamma1 * time_t
    log_sigma2[i] <- gamma0 + gamma1 * time[i]
    tau_eps[i] <- exp(-log_sigma2[i])

    Y[i] ~ dnorm(mu[i], tau_eps[i])
    
    # posterior predictive, used for computing RMSE
    Y_pred[i] ~ dnorm(mu[i], tau_eps[i])

    # mean structure: y_it = beta0 + b_i + epsilon_it
    mu[i] <- beta0 + b[subject[i]]
    
    # posterior prediction error squared
    sq_err[i] <- pow(Y[i] - Y_pred[i], 2)
    
    # Calculate log-likelihood, note that we use the dynamic tau_eps[i] here
    log_lik[i] <- -0.5 * log(2 * 3.14159265359) + 0.5 * log(tau_eps[i]) - 0.5 * tau_eps[i] * pow(Y[i] - mu[i], 2)
  }
  
  mse <- sum(sq_err[1:N_obs]) / N_obs
  rmse <- sqrt(mse)

  # --- Level 2: Individual Level ---
  for (j in 1:N_subj) {
    b[j] ~ dnorm(0, prec_b)
  }

  # --- Priors (Empirical Bayes) ---
  
  # intercept prior
  beta0 ~ dnorm(prior_hlmm_1, 0.1) 
  
  # variance function parameters prior
  gamma0 ~ dnorm(log(pow(prior_hlmm_2, 2)), 0.1)
  gamma1 ~ dnorm(prior_hlmm_3, 0.1)
  
  # individual intercept standard deviation
  sigma_b ~ dunif(0, 10)
  prec_b <- 1 / pow(sigma_b, 2)
}
"

# (3) estimate the parameters ---------------------------------------------

set.seed(20260224)

inits_hlmm_1 <- list(
  beta0       = prior_hlmm_1,
  gamma0      = log(prior_hlmm_2^2),
  gamma1      = prior_hlmm_3,
  sigma_b     = 1.612
)

inits_hlmm_2 <- list(
  beta0       = prior_hlmm_1/2,
  gamma0      = log((prior_hlmm_2/2)^2),
  gamma1      = prior_hlmm_3/2,
  sigma_b     = 1.2
)

jags_mod_hlmm <- jags.model(
  file     = textConnection(model_string_hlmm),
  data     = jags_data_hlmm,
  inits    = list(inits_hlmm_1, inits_hlmm_2), 
  n.chains = 2,            
  n.adapt  = 5000          
)




# (4) sampling and monitoring ---------------------------------------------

# Burn-in
update(jags_mod_hlmm, n.iter = 10000)

# parameters we want to monitor
params_hlmm <- c("beta0", "gamma0", "gamma1", "sigma_b", "rmse", "log_lik")

# Sampling
samples_hlmm <- coda.samples(
  model          = jags_mod_hlmm,
  variable.names = params_hlmm,
  n.iter         = 50000 
)

diag_params <- varnames(samples_hlmm)
diag_params <- diag_params[!grepl("^log_lik\\[", diag_params)]
diag_params <- diag_params[!grepl("^rmse$", diag_params)]
samples_diag <- samples_hlmm[, diag_params]
gelman_results <- gelman.diag(samples_diag, multivariate = T)
print(gelman_results)

# Plot selected trace plots and save them directly in out_dir.
trace_params_hlmm <- c("beta0", "gamma0", "gamma1", "sigma_b")

for (p in trace_params_hlmm) {
  safe_p <- gsub("\\[|\\]", "_", p)
  png(file.path(out_dir, paste0("traceplot_", safe_p, ".png")),
      width = 800, height = 600)
  traceplot(samples_diag[, p], main = paste("Traceplot of", p))
  dev.off()
}

# (5) extract parameters --------------------------------------------------

# look up summary and RMSE
summary_hlmm <- summary(samples_hlmm)
rmse_hlmm <- summary_hlmm$statistics["rmse", "Mean"]
cat("HLMM Model RMSE:", rmse_hlmm, "\n")


# (6) Compute LOO and WAIC ------------------------------------------------

# extract Log-likelihood matrix
samples_matrix_hlmm <- as.matrix(samples_hlmm)
log_lik_cols_hlmm   <- grep("^log_lik\\[", colnames(samples_matrix_hlmm))
log_lik_matrix_hlmm <- samples_matrix_hlmm[, log_lik_cols_hlmm]

# calculate WAIC and LOO
waic_hlmm <- waic(log_lik_matrix_hlmm)
loo_hlmm  <- loo(log_lik_matrix_hlmm)

# print results
print(waic_hlmm)
print(loo_hlmm)

# save results
save(gelman_results, waic_hlmm, loo_hlmm, rmse_hlmm, file = file.path(out_dir, "results_HLMM.RData"))
