# FUNCTION to apply survival to a population vector #

## Function takes a dataframe of individuals at t and applies survival and
# recapture probabilities to give a new dataframe of 'survivors' at t+1

## INPUT :
#
# - input_data =a dataframe with column names:
# ID (factor), Year (factor), Surv (0/1), Recap (0/1), Offspring (num), 
# Age (num), Trait (num)
#
# - parameters = a transition matrix of size max_age X max_age 
#
# - p = vector of recapture probabilities length of max_age
# 
# - max_age = maximum age species can get to
# 
# - inc_trait = TRUE or FALSE if you want to include a trait as well

## OUTPUT = a new dataframe with individuals that survived to the next year

#### FUNCTION ####
survival_function <- function(input_data, 
                              parameters = matrix(c(rep(1.6, 5),
                                                    0.5, 0, 0, 0, 0,
                                                    0, 0.5, 0, 0, 0,
                                                    0, 0, 0.5, 0, 0,
                                                    0, 0, 0, 0.5, 0), 
                                                  byrow = TRUE, 
                                                  ncol = 5),
                              p = rep(0.6, 5),
                              max_age = 5,
                              inc_trait = FALSE,
                              defined_seed = NULL) {
  
## Load packages
  
library(tidyverse)
  
## Only work on the focal year

old_data <- input_data %>% filter(Year < i)
  
input_data <- input_data %>% filter(Year == i)
  
## Fill in the survival column of the input_data

## Fill in survival values using probabilities in parameter matrix
  
# make a vector of probabilities based on age of individuals
# remove first row of parameter matrix as this is reproduction
# then add a final 0 as oldest individuals all die
# then index based on age
phi <- c(diag(parameters[-1,]),0)[input_data[ ,"Age"]]

# get survival values using rbinom using the phi vector
# need to ensure column Surv remains numeric
if(!is.null(defined_seed)){set.seed(seed)}
input_data$Surv <- as.numeric(rbinom(n = length(input_data$Surv), 
                           size = 1, 
                           prob = phi))

## Fill in recapture values using probabilities in parameter matrix

# make a vector of probabilities based on age of individuals
# use the vector of p
p <- p[input_data[ ,"Age"]]

# scale by survival - all that die are not recaptured

p[which(input_data$Surv == 0)] <- 0

# get recapture values using rbinom using the phi vector
# need to ensure column Recap remains numeric
if(!is.null(defined_seed)){set.seed(seed)}
input_data$Recap <- as.numeric(rbinom(n = length(input_data$Recap), 
                                     size = 1, 
                                     prob = p))
  
## Return new input_data file
return(rbind(old_data, input_data))
  
}  
