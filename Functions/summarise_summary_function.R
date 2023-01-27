#### T1.1: Function to summarise results summary ####

# Takes summary table and calculates error, uncertainty etc

# Input:

# results = summary table output from MCMCvis()

# true_parameters = table of input parameter values(cols = parameter, value)

################################################################################


summary_summary <- function(results,
                            true_parameters,
                            stages,
                            i){
  
###### CHECK ######
# names in true_parameters match those in results

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
  
output <- data.frame(parameter = parameter_name,
  true = c(filter(true_parameters, parameter == parameter_name)$value),
  estimated = c(filter(results, rownames(results) == parameter_name)$mean),
  CI_lower = c(filter(results, rownames(results) == parameter_name)$"2.5%"),
  CI_upper = c(filter(results, rownames(results) == parameter_name)$"97.5%"))
  
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
