# Script to run checks for function to make sure it is performing correctly #

################################################################################

## Source functions to test

source("./Functions/survival_function.R")
source("./Functions/reproduction_function.R")
source("./Functions/process_input_data.R")
source("./Functions/run_simulation.R")

## Source required packages

library(tidyverse)

#### Input error checks for all functions ####

## Set up inputs

input_data <- data.frame(ID = sample(1:100, 100, replace = FALSE),
                         Year = 1,
                         Surv = 1,
                         Recap = 1,
                         Offspring = rpois(100, 2),
                         Age = sample(1:3, 100, replace = TRUE),
                         Trait = rnorm(100, 20, 5))

max_age <- 3

# make sure Recap and Surv = 0 for all of max age

input_data[which(input_data$Age == max_age), c("Recap", "Surv")] <- 0

# set up parameters

parameters <- matrix(c(rep(1, 3), 1, 0, 0,
                                    0, 1, 0), 
                                  byrow = TRUE, 
                                  ncol = max_age)

# set up recapture probabilities

recapture <- rep(1, max_age)

## Test input data = wrong format

# ID numeric
input_data_test <- input_data %>% mutate(ID = is.factor(ID))

survival_function(input_data = input_data_test,
                  parameters = parameters,
                  p = recapture,
                  max_age = max_age, 
                  inc_trait = FALSE,
                  defined_seed = NULL, 
                  i = 2)
# CORRECT 29.06.22

reproduction_function(input_data = input_data_test,
                  parameters = parameters,
                  max_age = max_age, 
                  obs_error = FALSE,
                  inc_trait = FALSE,
                  defined_seed = NULL, 
                  i = 2)
# CORRECT 29.06.22


# ID not a factor
input_data_test <- input_data %>% mutate(ID = is.numeric(ID))

# ID not a factor
input_data_test <- input_data %>% mutate(ID = is.numeric(ID))

# ID not a factor
input_data_test <- input_data %>% mutate(ID = is.numeric(ID))

# ID not a factor
input_data_test <- input_data %>% mutate(ID = is.numeric(ID))

# ID not a factor
input_data_test <- input_data %>% mutate(ID = is.numeric(ID))

# input data column missing
# input parameters wrong format
# input max_age wrong
# input seed wrong
# if you enter input with age > max_age code fails


#### Survival function ####

# aim of function is to take formatted data for a focal year
# update the survival column based on age/stage specific survival from a matrix
# update recapture rate based on age/stage specific recapture probability

## Check code (make sure function gives correct output)

# if survival/recapture parameters all set to 1 survival/recap column = all 1s

# if survival/recapture parameters all set to 0 survival/recap column = all 0s

# if reproduction set to 0, makes no difference

# set seed and make sure output matches test columns for survival and recapture
# (try a few different parameter matrices)

#### Reproduction function ####

# aim of function is to take formatted data for a focal year
# update the offspring column based on a age/stage specific reproductive rate

## Check code (make sure function gives correct output)

# if survival set to 0, makes no difference

# set seed and make sure output matches test column for offspring production
# (try a few different parameter matrices)

# check that offspring column output changes when you add obs_error = TRUE
# make sure it matches test column with same seed

#### Process input data function ####

# aim of function is to take formatted data for all years
# reduce to focal year, update by removing dead individuals ready for the next
# year and adding in new individuals

## Should add error checks for input data

## Check code

# check all individuals with survival = 0 do get removed but survival = 1 remain

# check all individuals with recap = 0 but survival = 1 are retained

# check number of new offspring is correct

# check ID generation is unique

#### Run simulation function ####

## Code check

# repeat all code checks above but using this wrapper function

################################################################################

# Try with a couple of different i

# When testing survival probabilities etc make sure to code the
# test NOT using the input data vector

# Also test for all same age so easier to see
