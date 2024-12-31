rm(list = ls())

library(rstanarm)
library(bayesplot)
library(loo)
library(rstan)  # For monitor function

source("functions.R")

# directories
dir1 <- "../Data/intermediate/"
dir2 <- paste0(dir1, "final/")
dir3 <- "../Data/final/"
dir_output <- paste0(dir3, "emp_control/")

# options
testing <- FALSE
seed <- 123

mcmc_options <- list(
  test = list(chains = 4, iter = 1000),
  actual = list(chains = 4, iter = 3000)
)

# data
load(paste0(dir2, "03_final_data.Rdata"))
load(paste0(dir2, "03_additional_data.Rdata"))
load(paste0(dir2, "04_cluster_results_all.RData"))

if (!dir.exists(dir_output)) {
  dir.create(dir_output)
}

if (testing) {
  n_chains <- mcmc_options$test$chains
  n_iter <- mcmc_options$test$iter
} else {
  n_chains <- mcmc_options$actual$chains
  n_iter <- mcmc_options$actual$iter
}

# select data for modeling
modeling_data <- results_list$rnd_data_clutered

modeling_data <- modeling_data %>%
  rename(
    industry = isic4_dh,
    industry_alternative = isic4_alternative
  )

modeling_data$company_name <- as.factor(modeling_data$company_name)
modeling_data$isic4 <- as.factor(modeling_data$isic4)
modeling_data$industry <- as.factor(modeling_data$industry)
modeling_data$clusters <- as.factor(modeling_data$clusters)
modeling_data$industry_alternative <- as.factor(modeling_data$industry_alternative)
# modeling_data$industry_ctry_group_agg <- as.factor(modeling_data$industry_ctry_group_agg)
modeling_data$year <- as.numeric(modeling_data$year)

cat("Running Bayesian Linear Models (rstanarm)...\n")

# 
# cat("\n--- First set of models ---\n")
# 
# # --- First set of models
# if (!file.exists(paste0(dir_output, "first_models_done.txt"))) {
#   log_file_1 <- paste0(dir_output, "first_models_log.txt")
#   
#   std_out_conn <- file(log_file_1, open = "wt")
#   sink(std_out_conn, append = TRUE, split = TRUE)
#   
#   msg_conn <- file(log_file_1, open = "at") 
#   sink(msg_conn, append = TRUE, type = "message") 
#   
#   cat("\n--- Model nr. 1, company level intercepts ---\n")
#   stan_model_1 <- stan_lmer(
#     log_comp_shifted_op ~ lag(log_rd) + log_emp + (1 | company_name), 
#     data = modeling_data,
#     chains = n_chains, iter = n_iter, seed = seed, 
#     cores = 4 
#   )
#   
#   diag_1 <- check_model_diagnostics(stan_model_1)
#   cat("Model 1 Diagnostics:\n", diag_1$rhat_status, "\n", diag_1$neff_status, "\n")
#   
#   cat("\n--- Model nr. 2, company level intercepts and isic4 level slopes ---\n")
#   stan_model_2 <- stan_lmer(
#     log_comp_shifted_op ~ lag(log_rd) + log_emp + (1 | company_name) + (0 + lag(log_rd) | industry),
#     data = modeling_data,
#     chains = n_chains, iter = n_iter, seed = seed,
#     cores = 4
#   )
#   
#   diag_2 <- check_model_diagnostics(stan_model_2)
#   cat("Model 2 Diagnostics:\n", diag_2$rhat_status, "\n", diag_2$neff_status, "\n")
#   
#   cat("\n--- Model nr. 3, company level intercepts and cluster level slopes ---\n")
#   stan_model_3 <- stan_lmer(
#     log_comp_shifted_op ~ lag(log_rd) + log_emp + (1 | company_name) + (0 + lag(log_rd) | clusters),
#     data = modeling_data,
#     chains = n_chains, iter = n_iter, seed = seed,  # MCMC settings
#     cores = 4  # Use 4 cores for parallel processing
#   )
#   
#   diag_3 <- check_model_diagnostics(stan_model_3)
#   cat("Model 3 Diagnostics:\n", diag_3$rhat_status, "\n", diag_3$neff_status, "\n")
#   
#   diag <- list(stan_model_1 = diag_1, stan_model_2 = diag_2, stan_model_3 = diag_3)
#   save(stan_model_1, stan_model_2, stan_model_3, diag, file = paste0(dir_output, "first_models.Rdata"))
#   
#   writeLines("First models completed", con = paste0(dir_output, "first_models_done.txt"))
#   
#   sink(type = "message")
#   close(msg_conn)
#   sink()
#   close(std_out_conn)
#   
#   rm(list = setdiff(ls(), c("dir1", "dir2", "dir3", "dir_output", "n_chains", "n_iter", "seed", "mcmc_options", "modeling_data", "prior_mean", "testing", "check_model_diagnostics")))
#   gc() 
# }

cat("\n--- Second set of models ---\n")

# --- Second set of models
if (!file.exists(paste0(dir_output, "second_models_done.txt"))) {
  log_file_2 <- paste0(dir_output, "second_models_log.txt")
  
  std_out_conn_2 <- file(log_file_2, open = "wt")
  sink(std_out_conn_2, append = TRUE, split = TRUE)
  
  msg_conn_2 <- file(log_file_2, open = "at") 
  sink(msg_conn_2, append = TRUE, type = "message") 
  
  cat("\n--- Model nr. 1, company level intercepts and cluster X country level slopes ---\n")
  stan_model_2_1 <- stan_lmer(
    log_comp_shifted_op ~ log_emp + (1 | company_name) + (0 + lag(log_rd) | industry_alternative), 
    data = modeling_data,
    chains = n_chains, iter = n_iter, seed = seed, 
    cores = 4 
  )
  
  diag_1 <- check_model_diagnostics(stan_model_2_1)
  cat("Model 1 Diagnostics:\n", diag_1$rhat_status, "\n", diag_1$neff_status, "\n")
  
  cat("\n--- Model nr. 2, company level intercepts and isic4 level slopes ---\n")
  stan_model_2_2 <- stan_lmer(
    log_comp_shifted_op ~ log_emp + (1 | company_name) + (0 + lag(log_rd) | industry),
    data = modeling_data,
    chains = n_chains, iter = n_iter, seed = seed,
    cores = 4
  )
  
  diag_2 <- check_model_diagnostics(stan_model_2_2)
  cat("Model 2 Diagnostics:\n", diag_2$rhat_status, "\n", diag_2$neff_status, "\n")
  
  cat("\n--- Model nr. 3, company level intercepts and cluster level slopes ---\n")
  stan_model_2_3 <- stan_lmer(
    log_comp_shifted_op ~ log_emp + (1 | company_name) + (0 + lag(log_rd) | clusters),
    data = modeling_data,
    chains = n_chains, iter = n_iter, seed = seed, 
    cores = 4  
  )
  
  diag_3 <- check_model_diagnostics(stan_model_2_3)
  cat("Model 3 Diagnostics:\n", diag_3$rhat_status, "\n", diag_3$neff_status, "\n")
  
  diag <- list(stan_model_2_1 = diag_1, stan_model_2_2 = diag_2, stan_model_2_3 = diag_3)
  save(stan_model_2_1, stan_model_2_2, stan_model_2_3, diag, file = paste0(dir_output, "second_models.Rdata"))
  
  writeLines("Second models completed", con = paste0(dir_output, "second_models_done.txt"))
  
  sink(type = "message")
  close(msg_conn_2)
  sink()
  close(std_out_conn_2)
  
  rm(list = setdiff(ls(), c("dir1", "dir2", "dir3", "dir_output", "n_chains", "n_iter", "seed", "mcmc_options", "modeling_data", "prior_mean", "testing", "check_model_diagnostics")))
  gc()  
}
# 
# cat("\n--- Third set of models ---\n")
# 
# # --- Third set of models
# if (!file.exists(paste0(dir_output, "third_models_done.txt"))) {
#   log_file_3 <- paste0(dir_output, "third_models_log.txt")
#   
#   std_out_conn_3 <- file(log_file_3, open = "wt")
#   sink(std_out_conn_3, append = TRUE, split = TRUE)
#   
#   msg_conn_3 <- file(log_file_3, open = "at") 
#   sink(msg_conn_3, append = TRUE, type = "message") 
#   
#   cat("\n--- Model nr. 1, company level intercepts with inverse gaussian family and identity link ---\n")
#   stan_model_3_1 <- stan_glmer(
#     op_comp_shifted ~ lag(rd) + log_emp + (1 | company_name), 
#     data = modeling_data,
#     family = inverse.gaussian(link = "identity"),
#     chains = n_chains, iter = n_iter, seed = seed, 
#     cores = 4 
#   )
#   
#   diag_1 <- check_model_diagnostics(stan_model_3_1)
#   cat("Model 1 Diagnostics:\n", diag_1$rhat_status, "\n", diag_1$neff_status, "\n")
#   
#   cat("\n--- Model nr. 2, company level intercepts and isic4 level slopes with inverse gaussian family and identity link ---\n")
#   stan_model_3_2 <- stan_glmer(
#     op_comp_shifted ~ lag(rd) + log_emp + (1 | company_name) + (0 + lag(rd) | industry),
#     data = modeling_data,
#     family = inverse.gaussian(link = "identity"),
#     chains = n_chains, iter = n_iter, seed = seed,
#     cores = 4
#   )
#   
#   diag_2 <- check_model_diagnostics(stan_model_3_2)
#   cat("Model 2 Diagnostics:\n", diag_2$rhat_status, "\n", diag_2$neff_status, "\n")
#   
#   cat("\n--- Model nr. 3, company level intercepts and cluster level slopes with inverse gaussian family and identity link ---\n")
#   stan_model_3_3 <- stan_glmer(
#     op_comp_shifted ~ lag(rd) + log_emp + (1 | company_name) + (0 + lag(rd) | clusters),
#     data = modeling_data,
#     family = inverse.gaussian(link = "identity"),
#     chains = n_chains, iter = n_iter, seed = seed,
#     cores = 4
#   )
#   
#   diag_3 <- check_model_diagnostics(stan_model_3_3)
#   cat("Model 3 Diagnostics:\n", diag_3$rhat_status, "\n", diag_3$neff_status, "\n")
#   
#   diag <- list(stan_model_3_1 = diag_1, stan_model_3_2 = diag_2, stan_model_3_3 = diag_3)
#   save(stan_model_3_1, stan_model_3_2, stan_model_3_3, diag, file = paste0(dir_output, "third_models.Rdata"))
#   
#   writeLines("Third models completed", con = paste0(dir_output, "third_models_done.txt"))
#   
#   sink(type = "message")
#   close(msg_conn_3)
#   sink()
#   close(std_out_conn_3)
#   
#   rm(list = setdiff(ls(), c("dir1", "dir2", "dir3", "dir_output", "n_chains", "n_iter", "seed", "mcmc_options", "modeling_data", "prior_mean", "testing", "check_model_diagnostics")))
#   gc()  
# }

cat("\n--- Fourth set of models ---\n")

# --- Fourth set of models, using cumulative R&D as predictor
if (!file.exists(paste0(dir_output, "fourth_models_done.txt"))) {
  log_file_4 <- paste0(dir_output, "fourth_models_log.txt")
  
  std_out_conn_4 <- file(log_file_4, open = "wt")
  sink(std_out_conn_4, append = TRUE, split = TRUE)
  
  msg_conn_4 <- file(log_file_4, open = "at") 
  sink(msg_conn_4, append = TRUE, type = "message") 
  
  cat("\n--- Model nr. 1, company level intercepts and cluster X country level slopes ---\n")
  stan_model_4_1 <- stan_lmer(
    log_comp_shifted_op ~ log_emp + (1 | company_name) + (0 + lag(log_cum_rd) | industry_alternative), 
    data = modeling_data,
    chains = n_chains, iter = n_iter, seed = seed, 
    cores = 4 
  )
  
  diag_1 <- check_model_diagnostics(stan_model_4_1)
  cat("Model 1 Diagnostics:\n", diag_1$rhat_status, "\n", diag_1$neff_status, "\n")
  
  cat("\n--- Model nr. 2, company level intercepts and isic4 level slopes ---\n")
  stan_model_4_2 <- stan_lmer(
    log_comp_shifted_op ~ log_emp + (1 | company_name) + (0 + lag(log_cum_rd) | industry),
    data = modeling_data,
    chains = n_chains, iter = n_iter, seed = seed,
    cores = 4
  )
  
  diag_2 <- check_model_diagnostics(stan_model_4_2)
  cat("Model 2 Diagnostics:\n", diag_2$rhat_status, "\n", diag_2$neff_status, "\n")
  
  cat("\n--- Model nr. 3, company level intercepts and cluster level slopes ---\n")
  stan_model_4_3 <- stan_lmer(
    log_comp_shifted_op ~ log_emp + (1 | company_name) + (0 + lag(log_cum_rd) | clusters),
    data = modeling_data,
    chains = n_chains, iter = n_iter, seed = seed,  # MCMC settings
    cores = 4  # Use 4 cores for parallel processing
  )
  
  diag_3 <- check_model_diagnostics(stan_model_4_3)
  cat("Model 3 Diagnostics:\n", diag_3$rhat_status, "\n", diag_3$neff_status, "\n")
  
  diag <- list(stan_model_4_1 = diag_1, stan_model_4_2 = diag_2, stan_model_4_3 = diag_3)
  save(stan_model_4_1, stan_model_4_2, stan_model_4_3, diag, file = paste0(dir_output, "fourth_models.Rdata"))
  
  writeLines("Fourth models completed", con = paste0(dir_output, "fourth_models_done.txt"))
  
  sink(type = "message")
  close(msg_conn_4)
  sink()
  close(std_out_conn_4)
  
  rm(list = setdiff(ls(), c("dir1", "dir2", "dir3", "dir_output", "n_chains", "n_iter", "seed", "mcmc_options", "modeling_data", "prior_mean", "testing", "check_model_diagnostics")))
  gc()  
}
# 
# cat("\n--- Fifth set of models ---\n")
# 
# # --- Fifth set of models, using cumulative R&D as both a fixed effect and a random slope
# if (!file.exists(paste0(dir_output, "fifth_models_done.txt"))) {
#   log_file_5 <- paste0(dir_output, "fifth_models_log.txt")
#   
#   std_out_conn_5 <- file(log_file_5, open = "wt")
#   sink(std_out_conn_5, append = TRUE, split = TRUE)
#   
#   msg_conn_5 <- file(log_file_5, open = "at") 
#   sink(msg_conn_5, append = TRUE, type = "message") 
#   
#   cat("\n--- Model nr. 1, company level intercepts and cluster X country level slopes ---\n")
#   stan_model_5_1 <- stan_lmer(
#     log_comp_shifted_op ~ lag(log_cum_rd) + log_emp + (1 | company_name) + (0 + lag(log_cum_rd) | industry_alternative), 
#     data = modeling_data,
#     chains = n_chains, iter = n_iter, seed = seed, 
#     cores = 4 
#   )
#   
#   diag_1 <- check_model_diagnostics(stan_model_5_1)
#   cat("Model 1 Diagnostics:\n", diag_1$rhat_status, "\n", diag_1$neff_status, "\n")
#   
#   cat("\n--- Model nr. 2, company level intercepts and isic4 level slopes ---\n")
#   stan_model_5_2 <- stan_lmer(
#     log_comp_shifted_op ~ lag(log_cum_rd) + log_emp + (1 | company_name) + (0 + lag(log_cum_rd) | industry),
#     data = modeling_data,
#     chains = n_chains, iter = n_iter, seed = seed,
#     cores = 4
#   )
#   
#   diag_2 <- check_model_diagnostics(stan_model_5_2)
#   cat("Model 2 Diagnostics:\n", diag_2$rhat_status, "\n", diag_2$neff_status, "\n")
#   
#   cat("\n--- Model nr. 3, company level intercepts and cluster level slopes ---\n")
#   stan_model_5_3 <- stan_lmer(
#     log_comp_shifted_op ~ lag(log_cum_rd) + log_emp + (1 | company_name) + (0 + lag(log_cum_rd) | clusters),
#     data = modeling_data,
#     chains = n_chains, iter = n_iter, seed = seed,  # MCMC settings
#     cores = 4  # Use 4 cores for parallel processing
#   )
#   
#   diag_3 <- check_model_diagnostics(stan_model_5_3)
#   cat("Model 3 Diagnostics:\n", diag_3$rhat_status, "\n", diag_3$neff_status, "\n")
#   
#   diag <- list(stan_model_5_1 = diag_1, stan_model_5_2 = diag_2, stan_model_5_3 = diag_3)
#   save(stan_model_5_1, stan_model_5_2, stan_model_5_3, diag, file = paste0(dir_output, "fifth_models.Rdata"))
#   
#   writeLines("Fifth models completed", con = paste0(dir_output, "fifth_models_done.txt"))
#   
#   sink(type = "message")
#   close(msg_conn_5)
#   sink()
#   close(std_out_conn_5)
#   
#   rm(list = setdiff(ls(), c("dir1", "dir2", "dir3", "dir_output", "n_chains", "n_iter", "seed", "mcmc_options", "modeling_data", "prior_mean", "testing", "check_model_diagnostics")))
#   gc()  
# }
# 
