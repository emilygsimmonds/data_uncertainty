# FUNCTION to apply reproduction to a population vector #

## Function takes a dataframe of individuals at t and applies reproduction and
# to give a new dataframe of 'offspring' numbers at t+1

## INPUT :
#
# - input_data =a dataframe with column names:
# ID (factor), Year (factor), Surv (0/1), Recap (0/1), Offspring (num), 
# Age (num), Trait (num), Group (factor)
#
# - lambda = fertility rate. Vector or single number
#
# - condition = variable that lambda varies by (only used if lambda = vector)
# 
# - inc_trait = TRUE or FALSE if you want to include a trait as well
#
# - obs_error = TRUE or FALSE whether there is observation error

## OUTPUT = a new dataframe with individuals that survived to the next year

#### FUNCTION ####
reproduction_function <- function(input_data, 
                              lambda = 1, 
                              condition = NULL, 
                              inc_trait = FALSE,
                              Obs_error = FALSE) {
  
## Load packages

library(tidyverse)
  
## Only work on the focal year

old_data <- input_data %>% filter(Year < i)

input_data <- input_data %>% filter(Year == i)

## Fill in the offspring column of the input_data

## check length of lambda to see if it varies or not then run offspring 

if(length(lambda) > 1){
  
# find the column that condition is set for
marker <- which(colnames(input_data) == condition)

# get offspring values using rpois using the lambda for each level of condition
input_data$Offspring <- rpois(n = length(input_data$Offspring), 
                                      lambda = lambda[input_data[,marker]])}else{
    input_data$Offspring <- rpois(n = length(input_data$Offspring), 
                              lambda = lambda)}

## add observation error

if(Obs_error == TRUE){
  
# resample the offspring counts with Poisson error
input_data$Offspring <- rpois(length(input_data$Offspring), 
                                     lambda = input_data$Offspring)
}

## Return new input_data file
return(rbind(old_data, input_data))
  
}  