library(tidyr)
library(dplyr)
library(tibble)
library(coda)

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

prior_trace_1 <- 2.16996704                          
prior_trace_2 <- 0.00395139                
prior_trace_3 <- -0.05319454                     

# (0) load in data --------------------------------------------------------
dat <- read.csv("mistral_separate_neg_avg_txt_emoji.csv")
dat$id <- 1:nrow(dat) 

select_dat <- dat %>% 
  select(id, t0:t14)

# (1) reshape the data and define peaks ------------------------------------

# 1. Convert to long format 
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
    Y_lag2 = ifelse(has_lag == 1, actual_lag_val, 0),
    time_lag_safe = ifelse(has_lag == 1, prev_time, 0) # this is used to handle the missing data since when has_lag==0, time_lag_safe==0 to avoid the NA (missing values) influence the calculation of the likelihood
  ) %>%
  filter(n() >= 3) %>%
  ungroup() %>%
  mutate(id_idx = as.numeric(as.factor(id)))

# 3. Prepare the final data list for rjags
event_times <- c(1, 5, 8, 11, 13)

jags_data <- list(
  Y = long_dat$sentiment,
  Y_lag2 = long_dat$Y_lag2,      
  has_lag = long_dat$has_lag,
  time = long_dat$time,
  time_lag = long_dat$time_lag_safe, # this is used to handle the missing data
  subject = long_dat$id_idx,
  N_obs = nrow(long_dat),
  N_subj = max(long_dat$id_idx),
  t_event = event_times,
  N_events = length(event_times),
  prior_trace_1 = prior_trace_1,
  prior_trace_2 = prior_trace_2,
  prior_trace_3 = prior_trace_3
)


# (2) define the model ----------------------------------------------------

model_string <- "
model {
  # --- Level 1: Observation Level ---
  for (i in 1:N_obs) {
    Y[i] ~ dnorm(mu[i], tau_epsilon)

    trend[i] <- alpha[subject[i]] + beta[subject[i]] * time[i]
    trend_lag[i] <- alpha[subject[i]] + beta[subject[i]] * time_lag[i]

    R_lag[i] <- (Y_lag2[i] - trend_lag[i]) * has_lag[i]

    for (k in 1:N_events) {
      t_diff[i,k] <- (time[i] - t_event[k]) * step(time[i] - t_event[k])

      shock_effect[i,k] <- A[subject[i], k] *
        exp(-t_diff[i,k] / d_decay[subject[i]]) *
        step(time[i] - t_event[k])
    }

    mu[i] <- trend[i] + (phi * R_lag[i]) + sum(shock_effect[i, 1:N_events])

    sq_err[i] <- pow(Y[i] - mu[i], 2)

    log_lik[i] <- -0.5 * log(2 * 3.14159265359) +
                  0.5 * log(tau_epsilon) -
                  0.5 * tau_epsilon * pow(Y[i] - mu[i], 2)
  }

  mse <- sum(sq_err[1:N_obs]) / N_obs
  rmse <- sqrt(mse)

  # --- Level 2: Individual Level (Random Effects) ---
  for (j in 1:N_subj) {
    alpha[j] ~ dnorm(gamma_alpha, tau_alpha)
    beta[j] ~ dnorm(gamma_beta, tau_beta)

    # changed: log-scale decay random effect
    log_d_decay[j] ~ dnorm(gamma_log_d, tau_log_d)
    d_decay[j] <- exp(log_d_decay[j])

    for (k in 1:N_events) {
      A[j, k] ~ dnorm(gamma_A[k], tau_A[k])
    }
  }

  # --- Fixed Effects Priors ---
  gamma_alpha ~ dnorm(prior_trace_1 , 200)
  gamma_beta ~ dnorm(prior_trace_2, 200)
  phi ~ dnorm(prior_trace_3, 0.1) T(-1, 1)

  # changed: prior for mean decay on log scale
  gamma_log_d ~ dnorm(log(1.5), 4)

  for (k in 1:N_events) {
    gamma_A[k] ~ dnorm(0, 0.01)
    sigma_A[k] ~ dunif(0, 5)
    tau_A[k] <- 1 / pow(sigma_A[k], 2)
  }

  # --- Hyperpriors (Standard Deviations) ---
  sigma_epsilon ~ dunif(0, 10)
  tau_epsilon <- 1 / pow(sigma_epsilon, 2)

  sigma_alpha ~ dunif(0, 10)
  tau_alpha <- 1 / pow(sigma_alpha, 2)

  # changed: SD on log-decay scale
  sigma_log_d ~ dunif(0, 10)
  tau_log_d <- 1 / pow(sigma_log_d, 2)

  sigma_beta ~ dunif(0, 10)
  tau_beta <- 1 / pow(sigma_beta, 2)
}
"

# (3) estimate the parameters ---------------------------------------------

library(rjags)
set.seed(20260224)

# 1. Define a function to generate initial values for each chain

inits_function <- function() {
  list(
    gamma_alpha = rnorm(1, prior_trace_1 , 0.05),
    gamma_beta = rnorm(1, prior_trace_2  , 0.02),
    gamma_log_d = rnorm(1, log(1.5), 0.3),
    gamma_A = rnorm(5, 0, 0.5),
    phi = runif(1, -0.5, 0.5),
    sigma_A = runif(5, 0.5, 2),
    sigma_alpha = runif(1, 0.5, 2),
    sigma_beta = runif(1, 0.05, 0.3),
    sigma_log_d = runif(1, 0.2, 1),
    sigma_epsilon = runif(1, 2, 5)
  )
}

# 2. Create the JAGS Model Object
jags_mod <- jags.model(
  file = textConnection(model_string),
  data = jags_data,
  inits = inits_function,
  n.chains = 2,
  n.adapt = 50000
)

# (4) sampling and monitoring ---------------------------------------------

# 1. Burn-in Phase
update(jags_mod, n.iter = 50000)

# 2. The Sampling Phase
parameters_to_watch <- c(
  "gamma_alpha", "gamma_A", "gamma_beta", "gamma_log_d", "phi",
  "sigma_alpha", "sigma_A", "sigma_beta", "sigma_epsilon", "sigma_log_d",
  "log_lik", "rmse"
)


samples <- coda.samples(
  model = jags_mod,
  variable.names = parameters_to_watch,
  n.iter = 200000
)

# 3. Check Gelman-Rubin diagnostics for monitored nodes.
diag_params <- varnames(samples)
diag_params <- diag_params[!grepl("^log_lik\\[", diag_params)]
diag_params <- diag_params[!grepl("^rmse$", diag_params)]

samples_diag <- samples[, diag_params]

gelman_results <- gelman.diag(samples_diag, multivariate = FALSE)

print(gelman_results)

save(gelman_results,
     file = file.path(out_dir, "gelman_diagnostics.RData"))


# 4. Plot the convergence plot/trace plot
trace_dir <- file.path(out_dir, "traceplots")
if (!dir.exists(trace_dir)) {
  dir.create(trace_dir)
}

for (p in varnames(samples_diag)) {
  safe_p <- gsub("\\[|\\]", "_", p)
  png(file.path(trace_dir, paste0("traceplot_", safe_p, ".png")),
      width = 800, height = 600)
  traceplot(samples_diag[, p], main = paste("Traceplot of", p))
  dev.off()
}


# (5) extract parameters --------------------------------------------------

# 1. Summary statistics
summary_params <- varnames(samples)
summary_params <- summary_params[!grepl("^log_lik\\[", summary_params)]
samples_summary <- samples[, summary_params]
summary_results <- summary(samples_summary)

save(summary_results,
     file = file.path(out_dir, "summary_results.RData"))

# 2. Extract Table
final_table <- cbind(
  Mean     = summary_results$statistics[, "Mean"],
  SD       = summary_results$statistics[, "SD"],
  Lower_95 = summary_results$quantiles[, "2.5%"],
  Upper_95 = summary_results$quantiles[, "97.5%"]
)

# 3. View the final estimates
print(final_table)

write.csv(final_table,
          file = file.path(out_dir, "new_model_parameter_estimates.csv"),
          row.names = TRUE)



# (6) Calculate the loo and waic ------------------------------------------

library(loo)

# Convert mcmc.list to matrix
samples_matrix <- as.matrix(samples)

# Extract pointwise log-likelihood columns
log_lik_cols <- grep("^log_lik\\[", colnames(samples_matrix))
log_lik_matrix <- samples_matrix[, log_lik_cols, drop = FALSE]

# Safety check
stopifnot(ncol(log_lik_matrix) == jags_data$N_obs)

# Compute WAIC
model_waic <- waic(log_lik_matrix)
print(model_waic)

# Compute PSIS-LOO
model_loo <- loo(log_lik_matrix)
print(model_loo)

# Save results
save(model_waic, model_loo,
     file = file.path(out_dir, "new_model_evaluation_metrics.RData"))