# FUNCTION to bootstrap data AND run analysis #

################################################################################

## INPUT = transition frequency table

## OUTPUT = summary of bootstrap results

#### load packages

library(tidyverse)

#### source scripts

source("./Functions/transition_frequency.R")
source("./Functions/make_matrix.R")

################################################################################

#### FUNCTION 1 : the actual resampling and calculation ####

bootstrap_tf <- function(frequency_table,
                         stages){
  
  # first - bootstrap the data
  new_frequency_table <- frequency_table[sample(1:nrow(frequency_table), 
                                                nrow(frequency_table),
                                                replace = TRUE),]
  # use it to create matrix
  boot_matrix <- make_matrix(new_frequency_table,
                             stages = stages)
  
  # calculate parameters and lambda
  
  output <- data.frame(lambda = as.numeric(eigen(boot_matrix)$values[1]))
  
  fates <- c(stages[-1], stages[length(stages)])
  
  for(i in stages){
    names <- c(paste0("survival_", i),
               paste0("reproduction_", i))
    output$s <- boot_matrix[fates[which(stages == i)],i]
    output$f <- boot_matrix[1,i]
    
    colnames(output) <- c(colnames(output)[1:(length(colnames(output))-2)],
                          names)
    
  }
  
  return(output)
  
}

####################

#### FUNCTION 2 : summary of bootstrap ####

bootstrap_summary <- function(frequency_table,
                              iterations,
                              stages){
  
  boot_results <- map(1:iterations, ~bootstrap_tf(frequency_table,
                                                  stages = stages)) %>%
    bind_rows()
  
  boot_summary <- boot_results %>% pivot_longer(everything(), names_to = "parameter",
                                                values_to = "value") %>%
    group_by(parameter) %>%
    summarise(estimate = mean(value),
              lower_ci = quantile(value, probs = 0.025),
              upper_ci = quantile(value, probs = 0.975))
  
  return(boot_summary)
  
}