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

#### FUNCTION: the actual resampling and calculation ####

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

