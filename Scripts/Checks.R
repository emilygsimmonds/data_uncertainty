# Script to run checks for function to make sure it is performing correctly #

################################################################################

## Source functions to test ####

source("./Functions/survival_function.R")
source("./Functions/reproduction_function.R")
source("./Functions/process_input_data.R")
source("./Functions/run_simulation.R")

## Source required packages ####

library(tidyverse)

#### INPUT ERROR CHECKS FOR ALL FUNCTIONS ####

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

## Test input data = wrong format ####

# set up function to change each column to a factor in turn

# first = list of column names 
column_names <- as.list(1:length(input_data[1,]))

# then use map to run for all columns

map(.x = column_names, safely(~{
  # change format of focal column
  input_data[,.x] <- as.factor(input_data[,.x])
  # then run survival function 'safely'
  survival_function(input_data = input_data,
                           parameters = parameters,
                           p = recapture,
                           max_age = max_age, 
                           inc_trait = FALSE,
                           defined_seed = NULL, 
                           i = 2)})) # CORRECT 29.06.22

map(.x = column_names, safely(~{
  # change format of focal column
  input_data[,.x] <- as.factor(input_data[,.x])
  # then run reproduction function 'safely'
  reproduction_function(input_data = input_data,
                    parameters = parameters,
                    max_age = max_age, 
                    inc_trait = FALSE,
                    defined_seed = NULL, 
                    i = 2)})) # CORRECT 29.06.22

## Test input data = missing columns ####

# run in map across all columns using list from previous test
map(.x = column_names, safely(~{
  # remove focal column
  input_data <- input_data[,-.x]
  # then run survival function 'safely'
  survival_function(input_data = input_data,
                    parameters = parameters,
                    p = recapture,
                    max_age = max_age, 
                    inc_trait = FALSE,
                    defined_seed = NULL, 
                    i = 2)})) # CORRECT 29.06.22

map(.x = column_names, safely(~{
  # remove focal column
  input_data <- input_data[,-.x]
  # then run reproduction function 'safely'
  reproduction_function(input_data = input_data,
                        parameters = parameters,
                        max_age = max_age, 
                        inc_trait = FALSE,
                        defined_seed = NULL, 
                        i = 2)})) # CORRECT 29.06.22

## Test input data = input parameters wrong format ####

# wrong dimensions of parameter matrix

parameters_test1 <- matrix(c(rep(1, 4), 
                             1, 0, 0, 0,
                             0, 1, 0, 0,
                             0, 0, 1, 0), 
                     byrow = TRUE, 
                     ncol = max_age+1)

parameters_test2 <- matrix(c(rep(1, 2), 1, 0), 
                           byrow = TRUE, 
                           ncol = max_age-1)

# test matrix = too big
survival_function(input_data = input_data, 
                  parameters = parameters_test1,
                  max_age = max_age, 
                  i = 2) # CORRECT 29.06.22

reproduction_function(input_data = input_data,
                      parameters = parameters_test1,
                      max_age = max_age,
                      inc_trait = inc_trait,
                      obs_error = obs_error,
                      defined_seed = defined_seed, i = 2) # CORRECT 29.06.22

# test matrix too small
survival_function(input_data = input_data, 
                  parameters = parameters_test2,
                  max_age = max_age, 
                  i = 2) # CORRECT 29.06.22

reproduction_function(input_data = input_data,
                      parameters = parameters_test2,
                      max_age = max_age,
                      inc_trait = inc_trait,
                      obs_error = obs_error,
                      defined_seed = defined_seed, i = 2) # CORRECT 29.06.22


# test if not a matrix
survival_function(input_data = input_data, 
                  parameters = as.numeric(parameters),
                  max_age = max_age, 
                  i = 2)  # CORRECT 29.06.22

reproduction_function(input_data = input_data,
                      parameters = as.numeric(parameters),
                      max_age = max_age,
                      inc_trait = inc_trait,
                      obs_error = obs_error,
                      defined_seed = defined_seed, i = 2) # CORRECT 29.06.22

# input max_age wrong
survival_function(input_data = input_data, 
                  parameters = parameters,
                  max_age = as.factor(max_age), 
                  i = 2)  # CORRECT 29.06.22

reproduction_function(input_data = input_data,
                      parameters = parameters,
                      max_age = as.factor(max_age),
                      inc_trait = inc_trait,
                      obs_error = obs_error,
                      defined_seed = defined_seed, i = 2) # CORRECT 29.06.22

# input seed wrong

survival_function(input_data = input_data, 
                  parameters = parameters,
                  p = rep(1, max_age),
                  max_age = max_age,
                  defined_seed = "l",
                  i = 2)  # CORRECT 29.06.22

reproduction_function(input_data = input_data,
                      parameters = parameters,
                      max_age = max_age,
                      inc_trait = inc_trait,
                      obs_error = obs_error,
                      defined_seed = "l", i = 2) # CORRECT 29.06.22

# if you enter input with age > max_age code fails

input_data$Age[10] <- 7

survival_function(input_data = input_data, 
                  parameters = parameters,
                  p = rep(1, max_age),
                  max_age = max_age,
                  i = 2)  # CORRECT 29.06.22

reproduction_function(input_data = input_data,
                      parameters = parameters,
                      max_age = max_age,
                      inc_trait = inc_trait,
                      obs_error = obs_error, i = 2) # CORRECT 29.06.22

################################################################################

#### FUNCTION SPECIFIC CHECKS ####

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

################################################################################

#### Survival function ####

# aim of function is to take formatted data for a focal year
# update the survival column based on age/stage specific survival from a matrix
# update recapture rate based on age/stage specific recapture probability

## Check code (make sure function gives correct output) ####

## TEST: surv/recap parameters all set to 1 survival/recap column = all 1s ####

# make sure all ages are at least 1 below max age
max_age <- 4

parameters_test <- matrix(c(rep(1, 4), 
                       1, 0, 0, 0, 
                       0, 1, 0, 0,
                       0, 0, 1, 0), 
                     byrow = TRUE, 
                     ncol = max_age)

recapture_test <- rep(1,max_age)

test <- survival_function(input_data = input_data,
                  parameters = parameters_test,
                  p = recapture_test,
                  max_age = max_age, 
                  inc_trait = FALSE,
                  defined_seed = NULL, 
                  i = 1)

summary(test$Surv)
summary(test$Recap)

# CORRECT 05.07.22

## TEST: surv/recap parameters all set to 0 survival/recap column = all 0s ####


# make sure all ages are at least 1 below max age
max_age <- 4

parameters_test <- matrix(c(rep(0, 4), 
                            0, 0, 0, 0, 
                            0, 0, 0, 0,
                            0, 0, 0, 0), 
                          byrow = TRUE, 
                          ncol = max_age)

recapture_test <- rep(0,max_age)

test <- survival_function(input_data = input_data,
                          parameters = parameters_test,
                          p = recapture_test,
                          max_age = max_age, 
                          inc_trait = FALSE,
                          defined_seed = NULL, 
                          i = 1)

summary(test$Surv)
summary(test$Recap)

# CORRECT 05.07.22

## TEST: if reproduction set to 0, makes no difference ####

# tested in code above! CORRECT 05.07.22

## TEST: set seed and make sure output matches test ####
# (try a few different parameter matrices)

# set up a parameter matrix to check
# set up parameters
parameters1 <- matrix(c(rep(1, 3), 0.5, 0, 0,
                       0, 0.5, 0), 
                     byrow = TRUE, 
                     ncol = max_age)
parameters2 <- matrix(c(rep(1, 3), 0.75, 0, 0,
                        0, 0.75, 0), 
                      byrow = TRUE, 
                      ncol = max_age)
parameters3 <- matrix(c(rep(1, 3), 0.3, 0, 0,
                        0, 0.3, 0), 
                      byrow = TRUE, 
                      ncol = max_age)

# set up recapture probabilities
recapture1 <- rep(0.5, max_age)
recapture2 <- rep(0.3, max_age)
recapture3 <- rep(0.8, max_age)

# set up test vectors
# MUST scale recap by survival
set.seed(1)
test_surv1 <- as.numeric(rbinom(n = length(input_data$Surv), 
                                size = 1, 
              prob = c(diag(parameters1[-1,]),0)[input_data[ ,"Age"]]))
set.seed(1)
test_recap1 <- as.numeric(rbinom(n = length(input_data$Surv), 
                                size = 1, 
                                prob = c(recapture1[input_data[ ,"Age"]])))
test_recap1[which(test_surv1 == 0)] <- 0

# 2
set.seed(2)
test_surv2 <- as.numeric(rbinom(n = length(input_data$Surv), 
                                size = 1, 
                      prob = c(diag(parameters2[-1,]),0)[input_data[ ,"Age"]]))
set.seed(2)
test_recap2 <- as.numeric(rbinom(n = length(input_data$Surv), 
                                 size = 1, 
                      prob = c(recapture2[input_data[ ,"Age"]])))
test_recap2[which(test_surv2 == 0)] <- 0
# 3
set.seed(3)
test_surv3 <- as.numeric(rbinom(n = length(input_data$Surv), 
                                size = 1, 
                      prob = c(diag(parameters3[-1,]),0)[input_data[ ,"Age"]]))
set.seed(3)
test_recap3 <- as.numeric(rbinom(n = length(input_data$Surv), 
                                 size = 1, 
                                 prob = c(recapture3[input_data[ ,"Age"]])))
test_recap3[which(test_surv3 == 0)] <- 0

# run survival function
test1 <- survival_function(input_data = input_data,
                          parameters = parameters1,
                          p = recapture1,
                          max_age = max_age, 
                          inc_trait = FALSE,
                          defined_seed = 1, 
                          i = 1)
test2 <- survival_function(input_data = input_data,
                           parameters = parameters2,
                           p = recapture2,
                           max_age = max_age, 
                           inc_trait = FALSE,
                           defined_seed = 2, 
                           i = 1)
test3 <- survival_function(input_data = input_data,
                           parameters = parameters3,
                           p = recapture3,
                           max_age = max_age, 
                           inc_trait = FALSE,
                           defined_seed = 3, 
                           i = 1)

# check - should = 0 if the same

test1$Surv - test_surv1
test2$Surv - test_surv2
test3$Surv - test_surv3 # ALL CORRECT 05.07.22

test1$Recap - test_recap1
test2$Recap - test_recap2
test3$Recap - test_recap3 # ALL CORRECT 05.07.22

################################################################################

#### Reproduction function ####

# aim of function is to take formatted data for a focal year
# update the offspring column based on a age/stage specific reproductive rate

## Check code (make sure function gives correct output) ####

## TEST: if survival set to 0, makes no difference ####

parameters_test <- matrix(c(rep(1, 3), 
                            0, 0, 0, 
                            0, 0, 0), 
                          byrow = TRUE, 
                          ncol = max_age)

reproduction_function(input_data = input_data,
                      parameters = parameters,
                      max_age = max_age, 
                      inc_trait = FALSE,
                      defined_seed = 1, 
                      i = 1)$Offspring - 
  reproduction_function(input_data = input_data,
                      parameters = parameters_test,
                      max_age = max_age, 
                      inc_trait = FALSE,
                      defined_seed = 1, 
                      i = 1)$Offspring # CORRECT 05.07.22

## TEST: set seed and make sure output matches test ####
# (try a few different parameter matrices)

# set up a parameter matrix to check
# set up parameters
parameters1 <- matrix(c(0.5, 0.5, 0.5, 
                        1, 0, 0,
                        0, 1, 0), 
                      byrow = TRUE, 
                      ncol = max_age)
parameters2 <- matrix(c(0.5, 1, 1, 
                        1, 0, 0,
                        0, 1, 0), 
                      byrow = TRUE, 
                      ncol = max_age)
parameters3 <- matrix(c(2, 2, 0.5, 
                        1, 0, 0,
                        0, 1, 0), 
                      byrow = TRUE, 
                      ncol = max_age)

# run test vectors
set.seed(1)
test_repro1 <- rpois(n = length(input_data$Offspring), 
                              lambda = parameters1[1, input_data$Age])
set.seed(2)
test_repro2 <- rpois(n = length(input_data$Offspring), 
                     lambda = parameters2[1, input_data$Age])
set.seed(3)
test_repro3 <- rpois(n = length(input_data$Offspring), 
                     lambda = parameters3[1, input_data$Age])

# then run function and subtract test vector from Offspring column
test1 <- reproduction_function(input_data = input_data,
                      parameters = parameters1,
                      max_age = max_age, 
                      inc_trait = FALSE,
                      defined_seed = 1, 
                      i = 1)$Offspring - test_repro1
test2 <- reproduction_function(input_data = input_data,
                               parameters = parameters2,
                               max_age = max_age, 
                               inc_trait = FALSE,
                               defined_seed = 2, 
                               i = 1)$Offspring - test_repro2
test3 <- reproduction_function(input_data = input_data,
                               parameters = parameters3,
                               max_age = max_age, 
                               inc_trait = FALSE,
                               defined_seed = 3, 
                               i = 1)$Offspring - test_repro3

# CHECK

test1
test2
test3 # ALL CORRECT 05.07.22

## TEST: check that offspring column output changes when obs_error = TRUE ####
# make sure it matches test column with same seed

reproduction_function(input_data = input_data,
                      parameters = parameters,
                      max_age = max_age, 
                      inc_trait = FALSE,
                      defined_seed = 1, 
                      i = 1)$Offspring - 
  reproduction_function(input_data = input_data,
                        parameters = parameters,
                        max_age = max_age, 
                        inc_trait = FALSE,
                        defined_seed = 1, 
                        i = 1, obs_error = TRUE)$Offspring # CORRECT 05.07.22


################################################################################
#### Process input data function ####

# aim of function is to take formatted data for all years
# reduce to focal year, update by removing dead individuals ready for the next
# year and adding in new individuals

## Should add error checks for input data

## Check code ####

## Set up inputs
# make sure length is short so easy to check 
input_data <- data.frame(ID = sample(1:10, 10, replace = FALSE),
                         Year = 1,
                         Surv = 1,
                         Recap = 1,
                         Offspring = rpois(10, 2),
                         Age = sample(1:3, 10, replace = TRUE),
                         Trait = rnorm(10, 20, 5))

max_age <- 3

# make sure Recap and Surv = 0 for all of max age

input_data[which(input_data$Age == max_age), c("Recap", "Surv")] <- 0

# set up parameters

parameters <- matrix(c(rep(1, 3), 1, 0, 0,
                       0, 1, 0), 
                     byrow = TRUE, 
                     ncol = max_age)

# set up recapture probabilities needs to be < 1

recapture <- rep(0.5, max_age)

## TEST: all individuals with Surv = 0 get removed + Surv = 1 remain ####
# check which individuals should be removed
removals <- input_data$ID[which(input_data$Surv == 0)]
survivors <- input_data$ID[which(input_data$Surv == 1)]

# run function
output <- process_input_data(input_data, i=2, IDs=1:1000)

# check IDs in Year 2
output <- output %>% filter(Year == 2)
output$ID # CORRECT 06.07.22

## TEST: all individuals with Recap = 0 but Surv = 1 are retained ####
# set up input data so some have some where Surv = 1 but Recap = 0
# also try vice versa
input_data$Surv <- c(rep(0,5), rep(1,5))
input_data$Recap<- c(rep(0:1, times = 5))

# increase max age so none should have died from being old
max_age <- 4

# run function
output <- process_input_data(input_data, i = 2, IDs = 1:100)

# CHECK: vector of IDs that should be retained in Year = 2
retained1 <- input_data$ID[which(input_data$Surv == 1 & input_data$Recap == 0)]
# vector of IDs that should not make it to Year = 2
removed2 <- input_data$ID[which(input_data$Surv == 0 & input_data$Recap == 1)]
output$ID[output$Year == 2] # CORRECT 06.07.22

## CHECK: number of new offspring is correct ##
# calculate expected number of offspring
expected_offspring <- sum(input_data$Offspring)

# then run function and add up new individuals
output <- process_input_data(input_data, i = 2, IDs = 1:100)
output <- output %>% filter(Year == 2)
output_offspring <- output$ID[!output$ID %in% input_data$ID]

# should be 0 if same
length(output_offspring)-expected_offspring # CORRECT 06.07.22

################################################################################

## TEST RUNNING MULTIPLE ####

source("./Functions/run_simulation.R")

# set up input data

input_data <- data.frame(ID = sample(1:100, 100, replace = FALSE),
                         Year = 1,
                         Surv = rbinom(100, 1, prob = 0.6),
                         Recap = 1,
                         Offspring = rpois(100, 1),
                         Age = sample(1:3, 100, replace = TRUE),
                         Trait = rnorm(100, 20, 5))
# set up max age

max_age = 3

# make sure Recap and Surv = 0 for all of max age

input_data[which(input_data$Age == max_age), c("Recap", "Surv")] <- 0

# set up parameters 

parameters = matrix(c(0.5, rep(1, 2),
                      0.3, 0, 0,
                      0, 0.6, 0), 
                    byrow = TRUE, 
                    ncol = max_age)

# set up recapture probabilities

recapture <- rep(0.7, max_age)

# set up IDs

IDs <- 101:10000000

#### TEST ####

output_data <- run_simulation(input_data_old = input_data, 
                              parameters = parameters, 
                              p = recapture, 
                              max_age = max_age,
                              inc_trait = FALSE,
                              obs_error = FALSE,
                              start_i = 2, end_i = 25, IDs = IDs) 

output_data %>% group_by(Year) %>% summarise(count = n(),
                                             repro = sum(Offspring))

#### Check if getting duplicated individuals ####

duplicates <- output_data %>% group_by(ID,Year) %>% summarise(count = n())

summary(duplicates$count)


