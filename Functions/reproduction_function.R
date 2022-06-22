# FUNCTION to apply reproduction to a population vector #

## Function takes a dataframe of individuals at t and applies reproduction and
# to give a new dataframe of 'offspring' numbers at t+1

## INPUT :
#
# - input_data =a dataframe with column names:
# ID (factor), Year (factor), Surv (0/1), Recap (0/1), Offspring (num), 
# Age (num), Trait (num), Group (factor)
#
# - parameters = a transition matrix of size max_age X max_age 
#
# - condition = variable that lambda varies by (only used if lambda = vector)
# 
# - inc_trait = TRUE or FALSE if you want to include a trait as well
#
# - obs_error = TRUE or FALSE whether there is observation error

## OUTPUT = a new dataframe with individuals that survived to the next year

#### FUNCTION ####
reproduction_function <- function(input_data, 
                              parameters = matrix(c(rep(1.6, 5),
                                                    0.5, 0, 0, 0, 0,
                                                    0, 0.5, 0, 0, 0,
                                                    0, 0, 0.5, 0, 0,
                                                    0, 0, 0, 0.5, 0), 
                                                  byrow = TRUE, 
                                                  ncol = 5),
                              max_age = 5, 
                              inc_trait = FALSE,
                              obs_error = FALSE) {
  
## Load packages

library(tidyverse)
  
## Only work on the focal year

old_data <- input_data %>% filter(Year < i)

input_data <- input_data %>% filter(Year == i)

## Fill in the offspring column of the input_data

# get a vector of fertility values for each individual based on age
# take first row of the parameters matrix and index by age column
lambdas <- parameters[1,input_data$Age]

# get offspring values using rpois using the lambda for each level of condition
input_data$Offspring <- rpois(n = length(input_data$Offspring), 
                                      lambda = lambdas)

## add observation error

if(obs_error == TRUE){
  
# resample the offspring counts with Poisson error
input_data$Offspring <- rpois(length(input_data$Offspring), 
                                     lambda = input_data$Offspring)
}

## Return new input_data file
return(rbind(old_data, input_data))
  
}  