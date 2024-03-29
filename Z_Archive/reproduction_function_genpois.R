# FUNCTION to apply reproduction to a population vector #

################################################################################

## Function takes a dataframe of individuals at t and applies reproduction and
# to give a new dataframe of 'offspring' numbers at t+1

## INPUT :
#
# - input_data =a dataframe with column names:
# ID (num, Year (num), Surv (0/1), Offspring (num), Clutch_size (num), 
# Age (num), Trait (num), Group (factor)
#
# - parameters = a transition matrix of size max_age X max_age 
# - breeding_probability = probability of breeding in a give year
# - attempt_success = probability of some success of breeding (fledge)
# - mean_clutch_size = mean number of eggs or babies
# - young_survival = survival of newborns to t+1
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
                              parameters = matrix(c(rep(1.6, 5),
                                                    0.5, 0, 0, 0, 0,
                                                    0, 0.5, 0, 0, 0,
                                                    0, 0, 0.5, 0, 0,
                                                    0, 0, 0, 0.5, 0), 
                                                  byrow = TRUE, 
                                                  ncol = 5),
                              breeding_probability = c(0.85, rep(0.95, 4)),
                              attempt_success = c(0.8, rep(0.95, 4)),
                              mean_clutch_size = 6,
                              young_survival = 0.15,
                              max_age = 5, 
                              inc_trait = FALSE,
                              defined_seed = NULL, i) {
  
## Load packages

library(tidyverse)
library(HMMpa)
  
################################################################################

### INITIAL CHECKS ###

# max_age is specified and a number
 if(is.numeric(max_age) != TRUE){stop("max_age must be a number")}
  
# attempt_success and young_survival are specified and a number
 if(is.numeric(attempt_success) != TRUE){stop("attempt_success must be a number")}
 if(is.numeric(young_survival) != TRUE){stop("young_survival must be a number")}
  
# no age in input data exceeds max_age
 if(length(which(input_data$Age > max_age)) > 0){stop("cannot have individuals
                                                       older than max_age")}
# i is specified and a number
 if(is.null(i)){stop("i must be supplied")}
 if(is.numeric(i) != TRUE){stop("i must be a number")}

# input data is correct format i.e. has the correct columns of correct format

# first - check that all columns expected are present
  if(is.null(input_data$ID)){stop("ID column missing")}
  if(is.null(input_data$Year)){stop("Year column missing")}
  if(is.null(input_data$Surv)){stop("Surv column missing")}
  if(is.null(input_data$Offspring)){stop("Offspring column missing")}
  if(is.null(input_data$Age)){stop("Age column missing")}
  if(is.null(input_data$Trait)){stop("Trait column missing")}

  # then check their format
  if(!is.numeric(input_data$Year)){stop("Year should be numeric")}
  if(!is.numeric(input_data$ID)){stop("ID should be numeric")}
  if(!is.numeric(input_data$Surv)){stop("Surv should be numeric")}
  if(!is.numeric(input_data$Offspring)){stop("Offspring should be numeric")}
  if(!is.numeric(input_data$Age)){stop("Age should be numeric")}
  if(!is.numeric(input_data$Trait)){stop("Trait should be numeric")}

# then check limits for Surv (0/1), Recap (0/1) and Age (>0<max_age)
  if(length(which(input_data$Surv < 0)|which(input_data$Surv > 1)) > 0){
  stop("Surv must be 0 or 1")}
  if(length(which(input_data$Age < 1)|which(input_data$Age > max_age)) > 0){
  stop("Age must be between 1 and max_age")}

# parameters is a matrix of size max_age by max_age
  if(class(parameters)[1] != "matrix"){stop("parameters must be a matrix")}
  if(length(which(dim(parameters) != 
                  c(max_age, max_age)) == FALSE)>0){stop("parameters must have dim 
                                                  = max_age by max_age")}
  
# breeding probability is a vector of length max_age
  if(length(which(dim(breeding_probability) != 
                  max_age) == FALSE)>0){stop("breeding_probability must have length 
                                                  = max_age")}

# IF a seed is defined, it is a number
 if(!is.null(defined_seed)){if(!is.numeric(defined_seed)){stop("seed 
 must be a number")}}
  
################################################################################
  
## Only work on the focal year

old_data <- input_data %>% filter(Year < i)

input_data <- input_data %>% filter(Year == i)

## Fill in the offspring column of the input_data

# get a vector of fertility values for each individual based on age
# first assign breeding probabilities
breeding_probabilities <- breeding_probability[input_data$Age]

if(!is.null(defined_seed)){set.seed(defined_seed)}
breeding_chance  <- rbinom(length(input_data$Age), 1, 
                           breeding_probabilities)

# mean of the generalised poisson = lambda1/1-lambda2 so 6 here
# set up clutch size
if(!is.null(defined_seed)){set.seed(defined_seed)}
input_data$Clutch_size <- rgenpois(length(input_data$Age), 
                                   lambda1 = mean_clutch_size*1.5, 
                        lambda2 = -0.5)*breeding_chance # upper limit (-lambda1/lambda2)

# assign attempt success
attempt_success <- attempt_success[input_data$Age]

if(!is.null(defined_seed)){set.seed(defined_seed)}
attempt_failures <- rbinom(length(input_data$Age), 1, 
                         attempt_success) # high nest success or litter success 
# (i.e. low predation, low abandonment)

# get offspring values using rbinom for survival from fledge to 1 
if(!is.null(defined_seed)){set.seed(defined_seed)}
input_data$Offspring <- rbinom(n = length(input_data$Offspring), 
                               size = input_data$Clutch_size*attempt_failures,
                               prob = young_survival)

## DO NOT ADD OBSERVATION ERROR HERE AS THIS SIMULATES THE "STATE"


## Return new input_data file
return(rbind(old_data, input_data))
  
}  