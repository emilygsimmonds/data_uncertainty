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
                         Offspring = rpois(100, 2),
                         Stage = sample(c("juvenile", "adult"), 100, 
                                        replace = TRUE),
                         Trait = rnorm(100, 20, 5))

# set up parameters

parameters <- matrix(c(rep(1, 2),
                                    0.9, 0.9), 
                                  byrow = TRUE, 
                                  ncol = 2)

stages <- c("juvenile", "adult")

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
                           inc_trait = FALSE,
                           defined_seed = NULL, 
                           i = 2)})) # CORRECT 29.06.22 + 09.22

map(.x = column_names, safely(~{
  # change format of focal column
  input_data[,.x] <- as.factor(input_data[,.x])
  # then run reproduction function 'safely'
  reproduction_function(input_data = input_data,
                    parameters = parameters,
                    inc_trait = FALSE,
                    defined_seed = NULL, 
                    i = 2)})) # CORRECT 29.06.22+ 09.22

## Test input data = missing columns ####

# run in map across all columns using list from previous test
map(.x = column_names, safely(~{
  # remove focal column
  input_data <- input_data[,-.x]
  # then run survival function 'safely'
  survival_function(input_data = input_data,
                    parameters = parameters,
                    inc_trait = FALSE,
                    defined_seed = NULL, 
                    i = 2)})) # CORRECT 29.06.22 + 09.22

map(.x = column_names, safely(~{
  # remove focal column
  input_data <- input_data[,-.x]
  # then run reproduction function 'safely'
  reproduction_function(input_data = input_data,
                        parameters = parameters,
                        inc_trait = FALSE,
                        defined_seed = NULL, 
                        i = 2)})) # CORRECT 29.06.22 + 09.22

## Test input data = input parameters wrong format ####

# wrong dimensions of parameter matrix

parameters_test1 <- matrix(c(rep(1, 4), 
                             1, 0, 0, 0,
                             0, 1, 0, 0,
                             0, 0, 1, 0), 
                     byrow = TRUE, 
                     ncol = 4)

# test matrix = too big
survival_function(input_data = input_data, 
                  parameters = parameters_test1,
                  stages = stages, 
                  i = 2) # CORRECT 29.06.22 + 09.22

reproduction_function(input_data = input_data,
                      parameters = parameters_test1,
                      inc_trait = inc_trait,
                      defined_seed = defined_seed, i = 2) # CORRECT 29.06.22 + 09.22

# test if not a matrix
survival_function(input_data = input_data, 
                  parameters = as.numeric(parameters),
                  i = 2)  # CORRECT 29.06.22 + 09.22

reproduction_function(input_data = input_data,
                      parameters = as.numeric(parameters),
                      inc_trait = inc_trait,
                      defined_seed = defined_seed, i = 2) # CORRECT 29.06.22 + 09.22

# input seed wrong

survival_function(input_data = input_data, 
                  parameters = parameters,
                  defined_seed = "l",
                  i = 2)  # CORRECT 29.06.22

reproduction_function(input_data = input_data,
                      parameters = parameters,
                      inc_trait = inc_trait,
                      defined_seed = "l", i = 2) # CORRECT 29.06.22

################################################################################

#### FUNCTION SPECIFIC CHECKS ####

## Set up inputs

input_data <- data.frame(ID = sample(1:100, 100, replace = FALSE),
                         Year = 1,
                         Surv = 1,
                         Offspring = rpois(100, 2),
                         Stage = sample(c("juvenile", "adult"), 100, 
                                        replace = TRUE),
                         Trait = rnorm(100, 20, 5))

# set up parameters


parameters <- matrix(c(rep(1, 2),
                       0.9, 0.9), 
                     byrow = TRUE, 
                     ncol = 2)

stages <- c("juvenile", "adult")


################################################################################

#### Survival function ####

# aim of function is to take formatted data for a focal year
# update the survival column based on age/stage specific survival from a matrix

## Check code (make sure function gives correct output) ####

## TEST: surv parameters all set to 1 survival column = all 1s ####

parameters_test <- matrix(c(rep(1, 4)), 
                     byrow = TRUE, 
                     ncol = 2)

test <- survival_function(input_data = input_data,
                  parameters = parameters_test,
                  inc_trait = FALSE,
                  defined_seed = NULL, 
                  i = 1)

summary(test$Surv)

# CORRECT 05.07.22 + 09.22

## TEST: surv parameters all set to 0 survival column = all 0s ####

parameters_test <- matrix(c(rep(0, 4)), 
                          byrow = TRUE, 
                          ncol = 2)

test <- survival_function(input_data = input_data,
                          parameters = parameters_test,
                          inc_trait = FALSE,
                          defined_seed = NULL, 
                          i = 1)

summary(test$Surv)

# CORRECT 05.07.22 + 09.22

## TEST: surv parameters all set to 0.5 survival column = get 50% 0s ####

parameters_test <- matrix(c(rep(0, 2),
                            rep(0.5, 2)), 
                          byrow = TRUE, 
                          ncol = 2)

test <- survival_function(input_data = input_data,
                          parameters = parameters_test,
                          inc_trait = FALSE,
                          defined_seed = NULL, 
                          i = 1)

summary(test$Surv) # CORRECT 23.01.23

## TEST: if reproduction set to 0, makes no difference ####

# tested in code above! CORRECT 05.07.22

## TEST: set seed and make sure output matches test ####
# (try a few different parameter matrices)

# set up a parameter matrix to check
# set up parameters

parameters1 <- matrix(c(rep(1, 2), 0.4, 0.5), 
                     byrow = TRUE, 
                     ncol = 2)
parameters2 <- matrix(c(rep(1, 2), 0.65, 0.75), 
                      byrow = TRUE, 
                      ncol = 2)
parameters3 <- matrix(c(rep(1, 2), 0.2, 0.3), 
                      byrow = TRUE, 
                      ncol = 2)

rownames(parameters1) <- rownames(parameters2) <- rownames(parameters3) <-  stages
colnames(parameters1) <- colnames(parameters2) <- colnames(parameters3) <- stages

# set up test vectors
# MUST scale recap by survival
set.seed(1)
test_surv1 <- as.numeric(rbinom(n = length(input_data$Surv), 
                                size = 1, 
              prob = as.numeric(parameters1[-1,][input_data[,"Stage"]])))

# 2
set.seed(2)
test_surv2 <- as.numeric(rbinom(n = length(input_data$Surv), 
                                size = 1, 
                                prob = as.numeric(parameters2[-1,][input_data[,"Stage"]])))
# 3
set.seed(3)
test_surv3 <- as.numeric(rbinom(n = length(input_data$Surv), 
                                size = 1, 
                                prob = as.numeric(parameters3[-1,][input_data[,"Stage"]])))

# run survival function
test1 <- survival_function(input_data = input_data,
                          parameters = parameters1,
                          inc_trait = FALSE,
                          defined_seed = 1, 
                          i = 1)
test2 <- survival_function(input_data = input_data,
                           parameters = parameters2,
                           inc_trait = FALSE,
                           defined_seed = 2, 
                           i = 1)
test3 <- survival_function(input_data = input_data,
                           parameters = parameters3,
                           inc_trait = FALSE,
                           defined_seed = 3, 
                           i = 1)

# check - should = 0 if the same

test1$Surv - test_surv1
test2$Surv - test_surv2
test3$Surv - test_surv3 # ALL CORRECT 05.07.22 + 09.22 + 01.23

# now check that is right survival rate

sum(test_surv1[which(input_data$Stage == "juvenile")])/length(which(input_data$Stage == "juvenile"))
sum(test_surv2[which(input_data$Stage == "juvenile")])/length(which(input_data$Stage == "juvenile"))
sum(test_surv3[which(input_data$Stage == "juvenile")])/length(which(input_data$Stage == "juvenile"))

sum(test_surv1[which(input_data$Stage == "adult")])/length(which(input_data$Stage == "adult"))
sum(test_surv2[which(input_data$Stage == "adult")])/length(which(input_data$Stage == "adult"))
sum(test_surv3[which(input_data$Stage == "adult")])/length(which(input_data$Stage == "adult"))

################################################################################

#### Reproduction function ####

# aim of function is to take formatted data for a focal year
# update the offspring column based on a age/stage specific reproductive rate

## Check code (make sure function gives correct output) ####

## TEST: if survival set to 0, makes no difference ####

parameters_test <- matrix(c(rep(1, 2), 
                            0, 0), 
                          byrow = TRUE, 
                          ncol = 2)

reproduction_function(input_data = input_data,
                      parameters = parameters,
                      inc_trait = FALSE,
                      defined_seed = 1, 
                      i = 1)$Offspring - 
  reproduction_function(input_data = input_data,
                      parameters = parameters_test,
                      inc_trait = FALSE,
                      defined_seed = 1, 
                      i = 1)$Offspring # CORRECT 05.07.22 + 09.22

## TEST: set seed and make sure output matches test ####
# (try a few different parameter matrices)

# set up a parameter matrix to check
# set up parameters
parameters1 <- matrix(c(0.5, 0.5, 1, 1), 
                      byrow = TRUE, 
                      ncol = 2)
parameters2 <- matrix(c(0.5, 1, 1, 
                        1), 
                      byrow = TRUE, 
                      ncol = 2)
parameters3 <- matrix(c(2, 2, 0.5, 
                        1), 
                      byrow = TRUE, 
                      ncol = 2)

rownames(parameters1) <- rownames(parameters2) <- rownames(parameters3) <-  stages
colnames(parameters1) <- colnames(parameters2) <- colnames(parameters3) <- stages

# run test vectors
set.seed(1)
test_repro1 <- rpois(n = length(input_data$Offspring), 
                              lambda = parameters1[1, input_data$Stage])
set.seed(2)
test_repro2 <- rpois(n = length(input_data$Offspring), 
                     lambda = parameters2[1, input_data$Stage])
set.seed(3)
test_repro3 <- rpois(n = length(input_data$Offspring), 
                     lambda = parameters3[1, input_data$Stage])

# then run function and subtract test vector from Offspring column
test1 <- reproduction_function(input_data = input_data,
                      parameters = parameters1,
                      inc_trait = FALSE,
                      defined_seed = 1, 
                      i = 1)$Offspring - test_repro1
test2 <- reproduction_function(input_data = input_data,
                               parameters = parameters2,
                               inc_trait = FALSE,
                               defined_seed = 2, 
                               i = 1)$Offspring - test_repro2
test3 <- reproduction_function(input_data = input_data,
                               parameters = parameters3,
                               inc_trait = FALSE,
                               defined_seed = 3, 
                               i = 1)$Offspring - test_repro3

# CHECK

test1
test2
test3 # ALL CORRECT 05.07.22 + 09.22


################################################################################

#### Process input data function ####

# aim of function is to take formatted data for all years
# reduce to focal year, update by removing dead individuals ready for the next
# year and adding in new individuals

## Should add error checks for input data

## Check code ####

## Set up inputs

input_data <- data.frame(ID = sample(1:100, 100, replace = FALSE),
                         Year = 1,
                         Surv = sample(c(0,1), size = 100, replace = TRUE, 
                                       prob = c(0.9,0.9)),
                         Offspring = rpois(100, 2),
                         Stage = sample(c("juvenile", "adult"), 100, 
                                        replace = TRUE),
                         Trait = rnorm(100, 20, 5))

# set up parameters


parameters <- matrix(c(rep(1, 2),
                       0.9, 0.9), 
                     byrow = TRUE, 
                     ncol = 2)

stages <- c("juvenile", "adult")


## TEST: all individuals with Surv = 0 get removed + Surv = 1 remain ####
# check which individuals should be removed
removals <- input_data$ID[which(input_data$Surv == 0)]
survivors <- input_data$ID[which(input_data$Surv == 1)]

# run function
output <- process_input_data(input_data, i=2, IDs=1:1000)

# check IDs in Year 2
output <- output %>% filter(Year == 2)
output$ID %in% removals
output$ID[output$Stage=="adult"] %in% survivors

# CORRECT 06.07.22 + 09.22

## CHECK: number of new offspring is correct ##
# calculate expected number of offspring
expected_offspring <- sum(input_data$Offspring)

# then run function and add up new individuals
output <- process_input_data(input_data, i = 2, IDs = 1:1000)
output <- output %>% filter(Year == 2)
output_offspring <- output$ID[!output$ID %in% input_data$ID]

# should be 0 if same
length(output_offspring)-expected_offspring # CORRECT 06.07.22 + 09.22

################################################################################

## TEST RUNNING MULTIPLE ####

source("./Functions/run_simulation.R")

## Set up inputs

Stage = sample(c("juvenile", "adult"), 100, 
               replace = TRUE)

phi = c(0.7, 0.9)
names(phi) <- c("juvenile", "adult")
phi = phi[Stage]

input_data <- data.frame(ID = sample(1:100, 100, replace = FALSE),
                         Year = 1,
                         Surv = rbinom(100, 1, phi),
                         Offspring = rpois(100, 1),
                         Stage = Stage,
                         Trait = rnorm(100, 20, 5))

# set up parameters


parameters <- matrix(c(rep(1, 2),
                       0.7, 0.9), 
                     byrow = TRUE, 
                     ncol = 2)

stages <- c("juvenile", "adult")

# set up IDs

IDs <- 101:10000000

#### TEST ####

output_data <- run_simulation_state(input_data_old = input_data, 
                              parameters = parameters, 
                              inc_trait = FALSE,
                              start_i = 2, end_i = 5, IDs = IDs) 

output_data %>% group_by(Year, Stage) %>% summarise(count = n(),
                                             repro = sum(Offspring),
                                             surv = sum(Surv)/count)

# check that each year has the survival level expected 0.7 for juv 0.9 adults

# and then try some different parameters and make sure it changes
parameters <- matrix(c(rep(1, 2),
                       0.3, 0.2), 
                     byrow = TRUE, 
                     ncol = 2)

phi = c(0.3, 0.2)
names(phi) <- c("juvenile", "adult")
phi = phi[Stage]

input_data <- data.frame(ID = sample(1:100, 100, replace = FALSE),
                         Year = 1,
                         Surv = rbinom(100, 1, phi),
                         Offspring = rpois(100, 1),
                         Stage = Stage,
                         Trait = rnorm(100, 20, 5))

output_data <- run_simulation_state(input_data_old = input_data, 
                                    parameters = parameters, 
                                    inc_trait = FALSE,
                                    start_i = 2, end_i = 5, IDs = IDs) 

output_data %>% group_by(Year, Stage) %>% summarise(count = n(),
                                                    repro = sum(Offspring),
                                                    surv = sum(Surv)/count)

# SEEMS CORRECT 24.01.23

#### Check if getting duplicated individuals ####

duplicates <- output_data %>% group_by(ID,Year) %>% summarise(count = n())

summary(duplicates$count)

#### TEST: observation process ####

source("./Functions/run_observation_process.R")

non_perfect_recap <- run_observation_process(output_data, 
                                             p = c(1, recapture),
                                             phi = c(0.3, 0.5),
                                             fecundity_error = FALSE,
                                             seed = 2,
                                             stages = c("juvenile",
                                                        "adult"))

count_error_too <- run_observation_process(output_data, 
                                           p = c(1, recapture),
                                           phi = c(0.3, 0.5),
                                           fecundity_error = TRUE,
                                           seed = 2,
                                           stages = c("juvenile",
                                                      "adult"))

# number of individuals is reduced do they all have recap = 1
summary(non_perfect_recap) # YES

# are all juveniles still there
length(which(output_data$Age == 1)) - length(which(non_perfect_recap$Age == 1)) # YES

# how many individuals removed?
3019/3254 # 92 % remain

# are fecundity counts different?
count_error_too$Offspring - count_error_too$Offspring_obs



#### TEST: missing not at random ####

source("./Functions/run_simulation.R")
source("./Functions/create_scenario_data_not_random.R")

## Set up inputs

phi = c(0.7, 0.9)
names(phi) <- c("juvenile", "adult")
recapture <- c(1,1)
missing <- c(0.7,0.7)

# set up parameters

parameters <- matrix(c(rep(1, 2),
                       0.7, 0.9), 
                     byrow = TRUE, 
                     ncol = 2)

stages <- c("juvenile", "adult")

# set up IDs

output_random <- run_observation_process(output_data,
                                             p = recapture*missing,
                                             fecundity_error = FALSE,
                                             phi = phi,
                                             stages = stages,
                                             random = TRUE)

output_not_random <- run_observation_process(output_data,
                                             p = recapture*missing,
                                             fecundity_error = FALSE,
                                             phi = phi,
                                             stages = stages,
                                             random = FALSE)

# number of individuals is reduced do they all have recap = 1
summary(output_not_random) # YES

# how many individuals removed?
length(output_not_random[,1])/length(output_data[,1]) # 68 % remain, seems good
length(output_random[,1])/length(output_data[,1]) # 70% remain, seems good

# check that number of offspring differs between the two
summary(output_not_random$Offspring) #(median = 1, mean = 1.147)
summary(output_random$Offspring) #(median = 1, mean = 0.8606) # small difference
