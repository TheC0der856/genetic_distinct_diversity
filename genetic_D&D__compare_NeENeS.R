

# Load the Ne table into R (Ne-Estimator)
Ne_results <- read.csv(paste("Results", describe_results_variable, "/NeEstimator/Ne_results.csv", sep = "", quote = ""))  
# Load the Ne table into R (Speed-Ne)
NeS_results <- read.csv(paste("Results", describe_results_variable, "/NeSpeed/Ne_results.csv", sep = "", quote = ""))  


############## SpeedNe ###############################
# calculate the size of the confidence intervals
NeS_results <- NeS_results %>%
  mutate(difference_percentile_CI = upper_97_5_interval_percentile - lower_97_5_interval_percentile)
NeS_results <- NeS_results %>%
  mutate(difference_parametric_CI = upper_97_5_interval_parametric - lower_97_5_interval_parametric)

# Calculate the mean of Ne
NeS <- median(NeS_results$Ne, na.rm = TRUE)

# Calculate the mean of CIs without Inf
CI_percentile_NeS <- median(NeS_results$difference_percentile_CI[!is.infinite(NeS_results$difference_percentile_CI)], na.rm= TRUE)
CI_parametric_NeS <- median(NeS_results$difference_parametric_CI[!is.infinite(NeS_results$difference_parametric_CI)], na.rm= TRUE)

# Calculate how many Inf in %
if ("Inf" %in% NeS_results$difference_parametric_CI) {
  inf_count_NeS_para <- sum(NeS_results$difference_parametric_CI == "Inf")
  perc_inf_para_NeS <- inf_count_NeS_para / nrow(NeS_results) * 100
} else {
  perc_inf_para_NeS <- 0
}
if ("Inf" %in% NeS_results$difference_percentile_CI) {
  inf_count_NeS_perc <- sum(NeS_results$difference_percentile_CI == "Inf")
  perc_inf_perc_NeS <- inf_count_NeS_perc / nrow(NeS_results) * 100
} else {
  perc_inf_perc_NeS <- 0
}

############# NeEstimator #############################

# Calculate the mean of Ne
NeE <- median(Ne_results$Ne, na.rm = TRUE)

# calculate the size of the confidence intervals
Ne_results <- Ne_results %>%
  mutate(difference_percentile_CI = upper_jackknife_confidence_interval - lower_jackknife_confidence_interval)
Ne_results <- Ne_results %>%
  mutate(difference_parametric_CI = upper_parametric_confidence_interval - lower_parametric_confidence_interval)

# Calculate the mean of CIs without Inf
CI_percentile_NeE <- median(Ne_results$difference_percentile_CI[!is.infinite(Ne_results$difference_percentile_CI)])
CI_parametric_NeE <- median(Ne_results$difference_parametric_CI[!is.infinite(Ne_results$difference_parametric_CI)])

# Calculate how many Inf in %
if ("Inf" %in% Ne_results$difference_parametric_CI) {
  inf_count_NeE_para <- table(Ne_results$difference_parametric_CI == "Inf")[["TRUE"]]
  perc_inf_para_NeE <- inf_count_NeE_para/ nrow(Ne_results) *100
} else {
  perc_inf_para_NeE <- 0
}
if ("Inf" %in% Ne_results$difference_percentile_CI) {
  inf_count_NeE_perc <- table(Ne_results$difference_percentile_CI == "Inf")[["TRUE"]]
  perc_inf_perc_NeE <- inf_count_NeE_perc/ nrow(Ne_results) *100
} else {
  perc_inf_perc_NeE <- 0
}


# display results
results <- c(NeE , NeS, CI_percentile_NeE, CI_percentile_NeS, perc_inf_perc_NeE, perc_inf_perc_NeS, CI_parametric_NeE, CI_parametric_NeS, perc_inf_para_NeE, perc_inf_para_NeS)
print(results)