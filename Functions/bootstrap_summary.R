# FUNCTION to summarise bootstraps #

################################################################################

## INPUT = transition frequency table

## OUTPUT = summary of bootstrap results

#### load packages

library(tidyverse)

#### source scripts

source("./Functions/transition_frequency.R")
source("./Functions/make_matrix.R")
source("./Functions/bootstrap_tf.R")

####################################################################################################

#### FUNCTION : summary of bootstrap ####

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