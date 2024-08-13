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
                           i = 2)})) # CORRECT 29.06.22 + 09.22 + 08.24

map(.x = column_names, safely(~{
  # change format of focal column
  input_data[,.x] <- as.factor(input_data[,.x])
  # then run reproduction function 'safely'
  reproduction_function(input_data = input_data,
                    parameters = parameters,
                    inc_trait = FALSE,
                    defined_seed = NULL, 
                    i = 2)})) # CORRECT 29.06.22+ 09.22 + 08.24

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
                    i = 2)})) # CORRECT 29.06.22 + 09.22 + 08.24

map(.x = column_names, safely(~{
  # remove focal column
  input_data <- input_data[,-.x]
  # then run reproduction function 'safely'
  reproduction_function(input_data = input_data,
                        parameters = parameters,
                        inc_trait = FALSE,
                        defined_seed = NULL, 
                        i = 2)})) # CORRECT 29.06.22 + 09.22 + 08.24

## Test input data = input parameters wrong format ####

# wrong dimensions of parameter matrix

parameters_test1 <- matrix(c(rep(1, 4), 
                             1, 0, 0, 0,
                             0, 1, 0, 0,
                             0, 0, 1, 0), 
                     byrow = TRUE, 
                     ncol = 4)

# test matrix = too big - ERROR SHOULD BE RELATED TO DIM
survival_function(input_data = input_data, 
                  parameters = parameters_test1,
                  stages = stages, 
                  i = 2) # CORRECT 29.06.22 + 09.22 + 08.24

reproduction_function(input_data = input_data,
                      parameters = parameters_test1,
                      inc_trait = inc_trait,
                      defined_seed = defined_seed, 
                      stages = stages,
                      i = 2) # CORRECT 29.06.22 + 09.22 + 08.24

# test if not a matrix: ERROR = MUST BE MATRIX
survival_function(input_data = input_data, 
                  parameters = as.numeric(parameters),
                  i = 2)  # CORRECT 29.06.22 + 09.22 + 08.24

reproduction_function(input_data = input_data,
                      parameters = as.numeric(parameters),
                      inc_trait = inc_trait,
                      defined_seed = defined_seed, 
                      stages = stages,
                      i = 2) # CORRECT 29.06.22 + 09.22 + 08.24

# input seed wrong: WILL ASK TO BE A NUMBER
survival_function(input_data = input_data, 
                  parameters = parameters,
                  defined_seed = "l",
                  i = 2)  # CORRECT 29.06.22 + 08.24

reproduction_function(input_data = input_data,
                      parameters = parameters,
                      inc_trait = inc_trait,
                      stages = stages,
                      defined_seed = "l", i = 2) # CORRECT 29.06.22 + 08.24

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

# CORRECT 05.07.22 + 09.22 + 08.24

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

# CORRECT 05.07.22 + 09.22 + 08.24

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

summary(test$Surv) # CORRECT 23.01.23 + 08.24 
# (may need to try a few times as stochastic)

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

rownames(parameters1) <- rownames(parameters2) <- 
  rownames(parameters3) <-  stages
colnames(parameters1) <- colnames(parameters2) <- 
  colnames(parameters3) <- stages

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
                                prob = as.numeric(parameters2[-1,]
                                                  [input_data[,"Stage"]])))
# 3
set.seed(3)
test_surv3 <- as.numeric(rbinom(n = length(input_data$Surv), 
                                size = 1, 
                                prob = as.numeric(parameters3[-1,]
                                                  [input_data[,"Stage"]])))

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
test3$Surv - test_surv3 # ALL CORRECT 05.07.22 + 09.22 + 01.23 + 08.24

# now check that is right survival rate: 

# 0.4
sum(test_surv1[which(input_data$Stage == "juvenile")])/
  length(which(input_data$Stage == "juvenile"))
# 0.65
sum(test_surv2[which(input_data$Stage == "juvenile")])/
  length(which(input_data$Stage == "juvenile"))
# 0.2
sum(test_surv3[which(input_data$Stage == "juvenile")])/
  length(which(input_data$Stage == "juvenile"))

# 0.5
sum(test_surv1[which(input_data$Stage == "adult")])/
  length(which(input_data$Stage == "adult"))
# 0.75
sum(test_surv2[which(input_data$Stage == "adult")])/
  length(which(input_data$Stage == "adult"))
# 0.3
sum(test_surv3[which(input_data$Stage == "adult")])/
  length(which(input_data$Stage == "adult"))

# CORRECT 08.24

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

# Should output all 0
reproduction_function(input_data = input_data,
                      parameters = parameters,
                      inc_trait = FALSE,
                      defined_seed = 1, 
                      stages = stages,
                      i = 1)$Offspring - 
  reproduction_function(input_data = input_data,
                      parameters = parameters_test,
                      inc_trait = FALSE,
                      defined_seed = 1,
                      stages = stages,
                      i = 1)$Offspring # CORRECT 05.07.22 + 09.22 + 08.24

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

rownames(parameters1) <- rownames(parameters2) <- 
  rownames(parameters3) <-  stages
colnames(parameters1) <- colnames(parameters2) <- 
  colnames(parameters3) <- stages

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
                      stages = stages,
                      i = 1)$Offspring - test_repro1
test2 <- reproduction_function(input_data = input_data,
                               parameters = parameters2,
                               inc_trait = FALSE,
                               defined_seed = 2, 
                               stages = stages,
                               i = 1)$Offspring - test_repro2
test3 <- reproduction_function(input_data = input_data,
                               parameters = parameters3,
                               inc_trait = FALSE,
                               defined_seed = 3, 
                               stages = stages,
                               i = 1)$Offspring - test_repro3

# CHECK : SHOULD ALL = 0

test1
test2
test3 # ALL CORRECT 05.07.22 + 09.22 + 08.24


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
output <- process_input_data(input_data, i=2, IDs=1:1000,
                             stages = stages)

# check IDs in Year 2
output <- output %>% filter(Year == 2)
output$ID %in% removals # SHOULD ALL BE FALSE
output$ID[output$Stage=="adult"] %in% survivors # SHOULD ALL BE TRUE

# CORRECT 06.07.22 + 09.22 + 08.24

## CHECK: number of new offspring is correct ##
# calculate expected number of offspring
expected_offspring <- sum(input_data$Offspring)

# then run function and add up new individuals
output <- process_input_data(input_data, i = 2, IDs = 1:1000, stages = stages)
output <- output %>% filter(Year == 2)
output_offspring <- output$ID[!output$ID %in% input_data$ID]

# should be 0 if same
length(output_offspring)-expected_offspring # CORRECT 06.07.22 + 09.22 + 08.24

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
                              start_i = 2, end_i = 5, IDs = IDs,
                              stages = stages) 

output_data %>% group_by(Year, Stage) %>% summarise(count = n(),
                                             repro = sum(Offspring),
                                             surv = sum(Surv)/count)

# check that each year has the survival level expected 
# approx 0.7 for juv 0.9 adults
# CORRECT 08.24

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
                                    start_i = 2, end_i = 5, IDs = IDs,
                                    stages = stages) 

output_data %>% group_by(Year, Stage) %>% summarise(count = n(),
                                                    repro = sum(Offspring),
                                                    surv = sum(Surv)/count)

# SEEMS CORRECT 24.01.23 + 08.24

#### Check if getting duplicated individuals ####

duplicates <- output_data %>% group_by(ID,Year) %>% summarise(count = n())

summary(duplicates$count) # WANT ALL TO BE 1 CORRECT 08.24

#### TEST: observation process ####

source("./Functions/run_observation_process.R")

# set recapture rates
recapture <- 0.7

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
summary(non_perfect_recap) # YES CORRECT: 08.24

# are all juveniles still there: SHOULD = 0
length(which(output_data$Age == 1)) - 
  length(which(non_perfect_recap$Age == 1)) # YES CORRECT 08.24

# how many individuals removed?
length(non_perfect_recap$ID)/length(output_data$ID) # 92 % remain

# are fecundity counts different? SHOULD NOT = 0
count_error_too$Offspring - count_error_too$Offspring_obs # CORRECT 08.24


#### TEST: missing not at random ####

source("./Functions/run_simulation.R")
source("./Functions/create_scenario_data_not_random.R")

## Set up inputs

phi = c(0.7, 0.9)
names(phi) <- c("juvenile", "adult")
recapture <- c(1,1)
missing <- c(0.7,0.7)
split <- 0.5
bias <- "high"
offset <- 0.2

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

# RUNNING WITH CHECK ON SO CAN SEE RECAPTURE RATES
output_not_random_high <- run_observation_process(output_data,
                                             p = recapture*missing,
                                             fecundity_error = FALSE,
                                             phi = phi,
                                             stages = stages,
                                             split = split,
                                             bias = "high",
                                             offset = offset,
                                             random = FALSE,
                                             do_checks = TRUE)

output_not_random_low <- run_observation_process(output_data,
                                                  p = recapture*missing,
                                                  fecundity_error = FALSE,
                                                  phi = phi,
                                                  stages = stages,
                                                  split = split,
                                                  bias = "low",
                                                  offset = offset,
                                                  random = FALSE,
                                                  do_checks = TRUE)

# number of individuals is reduced do they all have recap = 1
summary(output_not_random_high) # YES
summary(output_not_random_low) # YES
summary(output_random) # YES
# ALL CORRECT 08.24

# how many individuals removed? WANT APPROX 70%
length(output_not_random_high[,1])/length(output_data[,1]) # 62.8 % remain 08.24
length(output_not_random_low[,1])/length(output_data[,1]) # 62.6 % remain 08.24
length(output_random[,1])/length(output_data[,1]) # 69.8% remain 08.24

# check that number of offspring differs 
# HIGH should be higher, LOW should be lower than randoms
summary(output_not_random_high$Offspring) 
summary(output_random$Offspring) 
summary(output_not_random_low$Offspring)
# WORKS FOR MEAN BUT NOT MAX OR MEDIAN 08.24

# EXTRA CHECK are there more individuals missing from correct groups

# hard to check this as individuals appear in multiple years and can belong
# to different groups in different years so doing by ID is not exact mapping
# INSTEAD - PUT CHECKING INTO THE FUNCTION ITSELF - SEE ABOVE
# THAT IS CORRECT AS OF 08.24
