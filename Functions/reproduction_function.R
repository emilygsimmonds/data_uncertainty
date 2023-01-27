# FUNCTION to apply reproduction to a population vector #

################################################################################

## Function takes a dataframe of individuals at t and applies reproduction and
# to give a new dataframe of 'offspring' numbers at t+1

## INPUT :
#
# - input_data =a dataframe with column names:
# ID (num, Year (num), Surv (0/1), Offspring (num), 
# Age (num), Trait (num), Group (factor)
#
# - parameters = a transition matrix of size max_age X max_age 
#
# - condition = variable that lambda varies by (only used if lambda = vector)
# 
# - inc_trait = TRUE or FALSE if you want to include a trait as well
#
# - i = index to say which year is focal year
#
# - defined_seed = if repeatable analysis is desired, specify a numeric seed

# - obs_error = cannot do that here - do it at the end of the simulation


## OUTPUT = a new dataframe with individuals that survived to the next year

#### FUNCTION ####
reproduction_function <- function(input_data, 
                                  parameters = matrix(data = c(0.5, 0.5, 0.3, 0.7),
                                                      nrow = 2, byrow = TRUE), 
                                  stages,
                              inc_trait = FALSE,
                              defined_seed = NULL, i) {
  
## Load packages

library(tidyverse)
  
################################################################################

### INITIAL CHECKS ###

# i is specified and a number
  if(is.null(i)){stop("i must be supplied")}
  if(is.numeric(i) != TRUE){stop("i must be a number")}

# input data is correct format i.e. has the correct columns of correct format

# first - check that all columns expected are present
  if(is.null(input_data$ID)){stop("ID column missing")}
  if(is.null(input_data$Year)){stop("Year column missing")}
  if(is.null(input_data$Surv)){stop("Surv column missing")}
  if(is.null(input_data$Offspring)){stop("Offspring column missing")}
  if(is.null(input_data$Stage)){stop("Stage column missing")}
  if(is.null(input_data$Trait)){stop("Trait column missing")}

  # then check their format
  if(!is.numeric(input_data$Year)){stop("Year should be numeric")}
  if(!is.numeric(input_data$ID)){stop("ID should be numeric")}
  if(!is.numeric(input_data$Surv)){stop("Surv should be numeric")}
  if(!is.numeric(input_data$Offspring)){stop("Offspring should be numeric")}
  if(!is.character(input_data$Stage)){stop("Stage should be character")}
  if(!is.numeric(input_data$Trait)){stop("Trait should be numeric")}

# then check limits for Surv (0/1), Recap (0/1)
  if(length(which(input_data$Surv < 0)|which(input_data$Surv > 1)) > 0){
  stop("Surv must be 0 or 1")}

# parameters is a matrix of size max_age by max_age
  if(class(parameters)[1] != "matrix"){stop("parameters must be a matrix")}
  if(length(which(dim(parameters) != 
                  c(length(stages), 
                  length(stages))) == FALSE)>0){stop("parameters must have dim 
                                                  = stage x stage")}

# IF a seed is defined, it is a number
if(!is.null(defined_seed)){if(!is.numeric(defined_seed)){stop("seed 
must be a number")}}
  
################################################################################
  
## Only work on the focal year

old_data <- input_data %>% filter(Year < i)

input_data <- input_data %>% filter(Year == i)

## Fill in the offspring column of the input_data
# name parameter matrix rows and cols
rownames(parameters) <- stages
colnames(parameters) <- stages

# get a vector of fertility values for each individual based on age
# take first row of the parameters matrix and index by age column
lambdas <- parameters[1, input_data$Stage]

# get offspring values using rpois using the lambda for each level of condition
if(!is.null(defined_seed)){set.seed(defined_seed)}
input_data$Offspring <- rpois(n = length(input_data$Offspring), 
                                      lambda = lambdas)

## DO NOT ADD OBSERVATION ERROR HERE AS THIS SIMULATES THE "STATE"


## Return new input_data file
return(rbind(old_data, input_data))
  
}  