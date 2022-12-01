#### T1.1: Function to summarise results summary ####

# Takes summary table and calculates error, uncertainty etc

# Input:

# results = summary table output from MCMCvis()

# true_parameters = table of input parameter values(cols = parameter, value)

################################################################################


summary_summary <- function(results,
                            true_parameters,
                            i){
  
# output rows: lambda, stable_juvenile, stable_adult,
#              recapture_juvenile, recapture_adult, 
#              reproduction_juvenile, reproduction_adult,
#              survival_juvenile, survival_adult
  
output <- data.frame(parameter = c("lambda", "stable_juvenile",
                                   "stable_adult", "recapture_juvenile", 
                                   "recapture_adult", "reproduction_juvenile",
                                   "reproduction_adult", "survival_juvenile", 
                                   "survival_adult"),
  true = c(filter(true_parameters, parameter == "lambda")$value,
              filter(true_parameters, parameter == "stable_juvenile")$value,
              filter(true_parameters, parameter == "stable_adult")$value,
              filter(true_parameters, parameter == "recapture_juvenile")$value,
              filter(true_parameters, parameter == "recapture_adult")$value,
              filter(true_parameters, parameter == "reproduction_juvenile")$value,
              filter(true_parameters, parameter == "reproduction_adult")$value,
              filter(true_parameters, parameter == "survival_juvenile")$value,
              filter(true_parameters, parameter == "survival_adult")$value),
     estimated = c(results["lambda", "mean"],
                   results["size_distribution[1]", "mean"],
                   results["size_distribution[2]", "mean"],
                   results["mean_p_juv", "mean"],
                   results["mean_p_adult", "mean"],
                   results["transition_matrix[1, 1]", "mean"],
                   results["transition_matrix[1, 2]", "mean"],
                   results["mean_phi_juv", "mean"],
                   results["mean_phi_adult", "mean"]),
     CI_lower = c(results["lambda", "2.5%"],
                  results["size_distribution[1]", "2.5%"],
                  results["size_distribution[2]", "2.5%"],
                  results["mean_p_juv", "2.5%"],
                  results["mean_p_adult", "2.5%"],
                  results["transition_matrix[1, 1]", "2.5%"],
                  results["transition_matrix[1, 2]", "2.5%"],
                  results["mean_phi_juv", "2.5%"],
                  results["mean_phi_adult", "2.5%"]),
     CI_upper = c(results["lambda", "97.5%"],
                  results["size_distribution[1]", "97.5%"],
                  results["size_distribution[2]", "97.5%"],
                  results["mean_p_juv", "97.5%"],
                  results["mean_p_adult", "97.5%"],
                  results["transition_matrix[1, 1]", "97.5%"],
                  results["transition_matrix[1, 2]", "97.5%"],
                  results["mean_phi_juv", "97.5%"],
                  results["mean_phi_adult", "97.5%"]))
  
# Then calculate error, CI width, and if parameter in CI
  
output <- mutate(output, 
                 error = estimated - true,
                 CI_width = CI_upper - CI_lower,
                 true_in_CI = NA,
                 scenario = i)

output$true_in_CI <- apply(output, 1, FUN = true_in_CI)

return(output)
  
  
}

# short extra function for calculating if true in CI

# input = row of data
# output = TRUE/FALSE

true_in_CI <- function(input){
  
  between(input[2], input[4], input[5])
  
}
