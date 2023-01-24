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

bootstrap_tf <- function(frequency_table){
  
  # first - bootstrap the data
  new_frequency_table <- frequency_table[sample(1:nrow(frequency_table), 
                                                nrow(frequency_table),
                                                      replace = TRUE),]
  # use it to create matrix
  boot_matrix <- make_matrix(new_frequency_table)
  
  # calculate parameters and lambda
  lambda <- as.numeric(eigen(boot_matrix)$values[1])
  s_adult <- boot_matrix[2,2]
  s_juvenile <- boot_matrix[2,1]
  f_juvenile <- boot_matrix[1,2]
  f_adult <- boot_matrix[2,1]
  
  return(data.frame(lambda = lambda,
                    s_juvenile = s_juvenile,
                    s_adult = s_adult,
                    f_juvenile = f_juvenile,
                    f_adult = f_adult))
  
}

####################

#### FUNCTION 2 : summary of bootstrap ####

bootstrap_summary <- function(frequency_table,
                              iterations){
  
  boot_results <- rerun(iterations, bootstrap_tf(frequency_table)) %>%
    bind_rows()
  
  boot_summary <- boot_results %>% pivot_longer(everything(), names_to = "parameter",
                                values_to = "value") %>%
    group_by(parameter) %>%
    summarise(estimate = mean(value),
              lower_ci = quantile(value, probs = 0.025),
              upper_ci = quantile(value, probs = 0.975))
  
  return(boot_summary)
  
}