# Script to run checks for 3x3 and 5x5 matrices #

################################################################################

## Source functions to test ####

source("./Functions/survival_function.R")
source("./Functions/reproduction_function.R")
source("./Functions/process_input_data.R")
source("./Functions/run_simulation.R")

## Source required packages ####

library(tidyverse)
library(mpmsim)

#### SET UP PARAMETERS AND INPUTS ####

## Set up inputs

input_data3 <- data.frame(ID = sample(1:100, 100, replace = FALSE),
                         Year = 1,
                         Surv = 1,
                         Offspring = rpois(100, 2),
                         Stage = sample(c("juvenile", "subadult", "adult"), 100, 
                                        replace = TRUE),
                         Trait = rnorm(100, 20, 5))

input_data5 <- data.frame(ID = sample(1:100, 100, replace = FALSE),
                          Year = 1,
                          Surv = 1,
                          Offspring = rpois(100, 2),
                          Stage = sample(c("juvenile", "subadult", "adult1",
                                           "adult2", "adult3"), 100, 
                                         replace = TRUE),
                          Trait = rnorm(100, 20, 5))

# set up parameters

parameters3 <- matrix(c(0,0.5,1,
                        0.3,0,0,
                        0,0.4,0.7), nrow = 3, byrow = TRUE)

parameters5 <- matrix(c(0,0,0.5,0.7,1,
                        0.3,0,0,0,0,
                        0,0.4,0,0,0,
                        0,0,0.5,0,0,
                        0,0,0,0.6,0.7), nrow = 5, byrow = TRUE)


stages3 <- c("juvenile", "subadult", "adult")
stages5 <-c("juvenile", "subadult", "adult1", "adult2", "adult3")


################################################################################

#### Survival function ####

set.seed(10)
test3 <- rbinom(100, 1, prob = c(0.3,0.4,0.7)[match(input_data3$Stage, stages3)])

survival3 <- survival_function(input_data = input_data3,
                          parameters = parameters3,
                          inc_trait = FALSE,
                          defined_seed = 10, 
                          stages = stages3,
                          i = 1) 

survival3$Surv - test3

survival3 %>%
  group_by(Stage) %>% summarise(surv_count = sum(Surv)/n()) # seems ok 26.01.23

set.seed(10)
test5 <- rbinom(100, 1, 
                prob = c(0.3,0.4,0.5,0.6,0.7)[match(input_data5$Stage, stages5)])

survival5 <- survival_function(input_data = input_data5,
                               parameters = parameters5,
                               inc_trait = 10,
                               defined_seed = NULL, 
                               stages = stages5,
                               i = 1) 

survival5$Surv - test5

survival5 %>%
  group_by(Stage) %>% summarise(surv_count = sum(Surv)/n()) # seems ok 26.01.23

################################################################################

#### Reproduction function ####

set.seed(10)
test3 <- rpois(n = length(input_data3$Offspring), 
                     lambda = parameters3[1, match(input_data3$Stage, stages3)])
set.seed(10)
test5 <- rpois(n = length(input_data5$Offspring), 
                     lambda = parameters5[1, match(input_data5$Stage, stages5)])

# then run function and subtract test vector from Offspring column
reproduction3 <- reproduction_function(input_data = input_data3,
                               parameters = parameters3,
                               inc_trait = FALSE,
                               defined_seed = 10, 
                               stages = stages3,
                               i = 1)

reproduction3$Offspring - test3

reproduction3 %>%
  group_by(Stage) %>% summarise(repro_count = sum(Offspring)/n()) # seems ok 26.01.23

reproduction5 <- reproduction_function(input_data = input_data5,
                               parameters = parameters5,
                               inc_trait = FALSE,
                               defined_seed = 10, 
                               stages = stages5,
                               i = 1)

reproduction5$Offspring - test5

reproduction5 %>%
  group_by(Stage) %>% summarise(repro_count = sum(Offspring)/n()) # seems ok 26.01.23

################################################################################

#### Process input data function ####

input_data3$Surv <- survival3$Surv

removals3 <- input_data3$ID[which(input_data3$Surv == 0)]
survivors3 <- input_data3$ID[which(input_data3$Surv == 1)]

input_data5$Surv <- survival5$Surv

removals5 <- input_data5$ID[which(input_data5$Surv == 0)]
survivors5 <- input_data5$ID[which(input_data5$Surv == 1)]

# run function
output3 <- process_input_data(input_data3, i=2, IDs=1:1000,
                              stages = stages3)

output5 <- process_input_data(input_data5, i=2, IDs=1:1000,
                              stages = stages5)

# check IDs in Year 2
output3 <- output3 %>% filter(Year == 2)
output3$ID %in% removals3
output3$ID[output3$Stage != "juvenile"] %in% survivors3

output5 <- output5 %>% filter(Year == 2)
output5$ID %in% removals5
output5$ID[output5$Stage != "juvenile"] %in% survivors5

# CORRECT 06.07.22 + 09.22

## CHECK: number of new offspring is correct ##
# calculate expected number of offspring
expected_offspring3 <- sum(input_data3$Offspring)
expected_offspring5 <- sum(input_data5$Offspring)

# then run function and add up new individuals
output_offspring3 <- output3$ID[!output3$ID %in% input_data3$ID]
output_offspring5 <- output5$ID[!output5$ID %in% input_data5$ID]

# should be 0 if same
length(output_offspring3)-expected_offspring3 # CORRECT 06.07.22 + 09.22
length(output_offspring5)-expected_offspring5

################################################################################

## Load libraries ####

library(nimble)
library(nimbleEcology)

## Source functions to test ####

source("./Functions/make_input_data_function.R")

## Source model code ####

source("./Scripts/T1.1_Model_hmm_33.R")

#### RUN TEST SIMULATION ####

# set up IDs

IDs <- 101:10000000

output_data3 <- run_simulation_state(input_data_old = input_data3, 
                                    parameters = parameters3, 
                                    inc_trait = FALSE,
                                    stages = stages3,
                                    start_i = 2, end_i = 6, IDs = IDs) %>%
  filter(Year > 1) # remove first year as parameters slightly different

output_data3 %>% group_by(Year, Stage) %>% summarise(count = n(),
                                                    repro = sum(Offspring)/count,
                                                    surv = sum(Surv)/count)

output_data5 <- run_simulation_state(input_data_old = input_data5, 
                                     parameters = parameters5, 
                                     inc_trait = FALSE,
                                     stages = stages5,
                                     start_i = 2, end_i = 6, IDs = IDs) %>%
  filter(Year > 1)

summary5 <- output_data5 %>% group_by(Year, Stage) %>% summarise(count = n(),
                                                     repro = sum(Offspring)/count,
                                                     surv = sum(Surv)/count)

################################################################################

#### Test observation process ####

source("./Functions/run_observation_process.R")

non_perfect_recap3 <- run_observation_process(state_data = output_data3, 
                                             p = c(1, 0.8, 0.8),
                                             phi = c(0.3, 0.4, 0.7),
                                             fecundity_error = FALSE,
                                             seed = 2,
                                             stages = stages3)

count_error_too3 <- run_observation_process(output_data, 
                                           p = c(1, 0.8, 0.8),
                                           phi = c(0.3, 0.4, 0.7),
                                           fecundity_error = TRUE,
                                           seed = 2,
                                           stages = stages3)

# number of individuals is reduced do they all have recap = 1
summary(non_perfect_recap3) # YES

# are all juveniles still there
length(which(output_data3$Age == 1)) - length(which(non_perfect_recap3$Age == 1)) # YES

# how many individuals removed?
length(unique(non_perfect_recap3$ID))/
  length(unique(output_data3$ID))
   # 98 % remain

# check individuals by stage - 70-77% BUT GOES > 80 when using diff seed
length(non_perfect_recap3$ID[which(non_perfect_recap3$Stage == "subadult")])/
  length(output_data3$ID[which(output_data3$Stage == "subadult")])
length(non_perfect_recap3$ID[which(non_perfect_recap3$Stage == "adult")])/
  length(output_data3$ID[which(output_data3$Stage == "adult")])

length(unique(count_error_too3$ID))/
  length(unique(output_data3$ID))
# 98 % remain

# are fecundity counts different?
count_error_too3$Offspring - count_error_too3$Offspring_obs

###############################################################################

#### Check missing scenarios ####

baseline_observations <- readRDS("./Data files/3x3/3baseline_simulation_observationsmat48.RDS")
.x <- baseline_observations
repro_stages <- c("subadult", "adult")

# juvenile missing
marker1 <- which(.x$Stage == repro_stages[1])
set.seed(1)
marker2 <- sample(marker1, round(length(marker1)*0.2))
.x <- .x %>% mutate(Offspring_obs = Offspring)
.x$Offspring_obs[marker2] <- 0

# compare offspring of lowest repro class
sum(.x$Offspring_obs[which(.x$Stage == "subadult")])/
  sum(baseline_observations$Offspring[which(baseline_observations$Stage == "subadult")]) #CORRECT

sum(.x$Offspring_obs[which(.x$Stage == "adult")])/
  sum(baseline_observations$Offspring[which(baseline_observations$Stage == "adult")])

# adult missing
.x <- baseline_observations
marker1 <- which(.x$Stage %in% repro_stages[-1])
set.seed(1)
marker2 <- sample(marker1, round(length(marker1)*0.2))
.x <- .x %>% mutate(Offspring_obs = Offspring)
.x$Offspring_obs[marker2] <- 0

# compare offspring of higher repro class
sum(.x$Offspring_obs[which(.x$Stage == "adult")])/
  sum(baseline_observations$Offspring[which(baseline_observations$Stage == "adult")]) #CORRECT

sum(.x$Offspring_obs[which(.x$Stage == "subadult")])/
  sum(baseline_observations$Offspring[which(baseline_observations$Stage == "subadult")]) #CORRECT


###############################################################################


non_perfect_recap5 <- run_observation_process(state_data = output_data5, 
                                              p = c(1, 0.8, 0.8, 
                                                    0.8, 0.8),
                                              phi = c(0.3, 0.4, 0.5, 0.6,
                                                      0.7),
                                              fecundity_error = FALSE,
                                              seed = 2,
                                              stages = stages5)

count_error_too5 <- run_observation_process(state_data = output_data5, 
                                            p = c(1, 0.8, 0.8, 
                                                  0.8, 0.8),
                                            phi = c(0.3, 0.4, 0.5, 0.6,
                                                    0.7),
                                            fecundity_error = TRUE,
                                            seed = 2,
                                            stages = stages5)

# number of individuals is reduced do they all have recap = 1
summary(non_perfect_recap5) # YES

# are all juveniles still there
length(which(output_data5$Age == 1)) - length(which(non_perfect_recap5$Age == 1)) # YES

# how many individuals removed?
length(unique(non_perfect_recap5$ID))/
  length(unique(output_data5$ID))
# 98 % remain

# check individuals by stage - 70-77%
length(non_perfect_recap5$ID[which(non_perfect_recap5$Stage == "subadult")])/
  length(output_data5$ID[which(output_data5$Stage == "subadult")])
length(non_perfect_recap5$ID[which(non_perfect_recap5$Stage == "adult3")])/
  length(output_data5$ID[which(output_data5$Stage == "adult3")])

length(unique(count_error_too5$ID))/
  length(unique(output_data5$ID))
# 98 % remain

# are fecundity counts different?
count_error_too5$Offspring - count_error_too5$Offspring_obs

################################################################################

#### CHECK MODEL IN WRAPPER ####

source("./Functions/make_input_data_function.R")
library(nimbleEcology)

# check eigenvalues of input parameters
eigen(parameters3)
eigen(parameters5)

source("./Scripts/T1.1_Model_hmm_33.R")

model_inputs3 <- make_input_data(output_data3, n_occasions = 5,
                                 stages = stages3)

output_results3 <- nimbleMCMC(code = Model_SS_hmm, 
                             data = model_inputs3$data_input,
                             constants = model_inputs3$constants,
                             inits = model_inputs3$inits,
                             monitors = model_inputs3$parameters_to_save,
                             niter = 500,
                             nburnin = 50,
                             nchains = 2)

MCMCsummary(output_results3, round = 2)
parameters3

source("./Scripts/T1.1_Model_hmm_55.R")

model_inputs5 <- make_input_data(output_data5, n_occasions = 5,
                                 stages = stages5)

output_results5 <- nimbleMCMC(code = Model_SS_hmm, 
                              data = model_inputs5$data_input,
                              constants = model_inputs5$constants,
                              inits = model_inputs5$inits,
                              monitors = model_inputs5$parameters_to_save,
                              niter = 500,
                              nburnin = 50,
                              nchains = 2)

MCMCsummary(output_results5)
parameters5

#### CHECK DIRECT ESTIMATE MODEL ####

library(popbio)

# source functions
source("./Functions/make_matrix.R")
source("./Functions/transition_frequency.R")
source("./Functions/bootstrap_tf.R")

#### construct a transition frequency table

# for the state
tf_table_state <- create_transition_frequency_table(census_data = output_data3,
                                                    max_year = max(output_data3$Year),
                                                    stages = stages3) # seems to work :)
# make population matrix
make_matrix(tf_table_state, stages = stages3)

#### run bootstrap to get CIs for vital rates and lambda

# for state
boot_results <- bootstrap_summary(tf_table_state, 
                                  iterations = 2000,
                                  stages = stages3)

# for the state
tf_table_state <- create_transition_frequency_table(census_data = output_data5,
                                                    max_year = max(output_data5$Year),
                                                    stages = stages5) # seems to work :)
# make population matrix
make_matrix(tf_table_state, stages = stages5)

#### run bootstrap to get CIs for vital rates and lambda

# for state
boot_results <- bootstrap_summary(tf_table_state, 
                                  iterations = 2000,
                                  stages = stages5)


