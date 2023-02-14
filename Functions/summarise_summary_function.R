#### T1.1: Function to summarise results summary ####

# Takes summary table and calculates error, uncertainty etc

# Input:

# results = summary table output from MCMCvis()

# true_parameters = table of input parameter values(cols = parameter, value)

################################################################################


summary_summary <- function(inputs,
                            stages,
                            scenario){
  
## first, import result and make summary
load(inputs$filename)
results <- MCMCsummary(model_result, round = 2)
  
# names in true_parameters match those in results
# reformat inputs to make true parameters
true_parameters <- inputs[,3:length(colnames(inputs))] %>% 
  pivot_longer(everything(), names_to = "parameter",
               values_to = "value")

if(length(which(true_parameters$parameter %in% rownames(results) == FALSE)) > 0){
  stop("parameter names not found in results")
}
  
# output rows: lambda,
#              recapture - all stages
#              reproduction - all stages
#              survival - all stages
  
# to get names for parameters use loop
  
parameter_names <- c("lambda")

for(j in stages){
  parameter_names <- c(parameter_names,
                       paste0("recapture_", j),
                       paste0("reproduction_", j),
                       paste0("survival_", j))
}

# need to use left_join to merge the true parameters and results
# first need to reduce number of columns in results and add parameter names
results_new <- results[, c("mean", "2.5%", "97.5%")] %>%
  mutate(parameter = rownames(results)) %>%
  filter(parameter %in% parameter_names)

output <- left_join(true_parameters, results_new)
  
colnames(output) <- c("parameter",
                      "true",
                      "estimated",
                      "CI_lower",
                      "CI_upper")
  
# Then calculate error, CI width, and if parameter in CI
  
output <- mutate(output, 
                 error = estimated - true,
                 CI_width = CI_upper - CI_lower,
                 true_in_CI = NA,
                 scenario = scenario)

output$true_in_CI <- apply(output, 1, FUN = true_in_CI)

return(output)
  
  
}

# short extra function for calculating if true in CI

# input = row of data
# output = TRUE/FALSE

true_in_CI <- function(input){
  
  between(input[2], input[4], input[5])
  
}
