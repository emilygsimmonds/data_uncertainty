# Script to run checks for 3x3 #

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

# set up parameters

parameters3 <- matrix(c(0,0.5,1,
                        0.3,0,0,
                        0,0.4,0.7), nrow = 3, byrow = TRUE)

stages3 <- c("juvenile", "subadult", "adult")


################################################################################

#### Survival function ####

# check if the survival function matches manual calculation of survival provs
set.seed(10)
# manual test
test3 <- rbinom(100, 1, prob = c(0.3,0.4,0.7)[match(input_data3$Stage, 
                                                    stages3)])

# function
survival3 <- survival_function(input_data = input_data3,
                          parameters = parameters3,
                          inc_trait = FALSE,
                          defined_seed = 10, 
                          stages = stages3,
                          i = 1) 

# SHOULD = 0 
survival3$Surv - test3 # CORRECT 08.24

# STAGES SHOULD = APPROX: adult 0.7, subadult 0.4, juvenile 0.3
survival3 %>%
  group_by(Stage) %>% summarise(surv_count = sum(Surv)/n()) 
# CORRECT 26.01.23 + 08.24

################################################################################

#### Reproduction function ####

# check if reproduction function matches manual offspring number assignment
set.seed(10)
# manual
test3 <- rpois(n = length(input_data3$Offspring), 
                     lambda = parameters3[1, match(input_data3$Stage, stages3)])

# then run function
reproduction3 <- reproduction_function(input_data = input_data3,
                               parameters = parameters3,
                               inc_trait = FALSE,
                               defined_seed = 10, 
                               stages = stages3,
                               i = 1)

# SHOULD = 0
reproduction3$Offspring - test3
# CORRECT 08.24

# STAGES SHOULD BE APPROX: juv = 0, subadult = 0.5, adult  =1
reproduction3 %>%
  group_by(Stage) %>% summarise(repro_count = sum(Offspring)/n()) 
# CORRECT 26.01.23 + 08.24

################################################################################

#### Process input data function ####

# aim of function is to take formatted data for all years
# reduce to focal year, update by removing dead individuals ready for the next
# year and adding in new individuals

## TEST: all individuals with Surv = 0 get removed + Surv = 1 remain ####
# check which individuals should be removed

input_data3$Surv <- survival3$Surv

removals3 <- input_data3$ID[which(input_data3$Surv == 0)]
survivors3 <- input_data3$ID[which(input_data3$Surv == 1)]

# run function
output3 <- process_input_data(input_data3, i=2, IDs=1:1000,
                              stages = stages3)
# check IDs in Year 2
output3 <- output3 %>% filter(Year == 2)
output3$ID %in% removals3 # CHECK ALL REMOVED
output3$ID[output3$Stage != "juvenile"] %in% survivors3 # CHECK JUVENILES REMAIN

# CORRECT 06.07.22 + 09.22 + 08.24

## CHECK: number of new offspring is correct ##
# calculate expected number of offspring
expected_offspring3 <- sum(input_data3$Offspring)

# then run function and add up new individuals
output_offspring3 <- output3$ID[!output3$ID %in% input_data3$ID]

# should be 0 if same
length(output_offspring3)-expected_offspring3 # CORRECT 06.07.22 + 09.22 + 08.24

################################################################################

## Load libraries ####

library(nimble)
library(nimbleEcology)

## Source functions to test ####

source("./Functions/make_input_data_function.R")
source("./Functions/run_simulation.R")
source("./Functions/create_scenario_data_not_random.R")

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

# check that each year has the survival level expected 
# approx 0.3 for juv, 0.4 sub adults, 0.7 adults
# check reproduction ok too 0 juv, 0.5 subadult, 1 adult
# CORRECT 08.24

# THEN try different parameters

parameters3b <- matrix(c(0,1,0.5,
                        0.7,0,0,
                        0,0.9,0.5), nrow = 3, byrow = TRUE)

output_data3b <- run_simulation_state(input_data_old = input_data3, 
                                     parameters = parameters3b, 
                                     inc_trait = FALSE,
                                     stages = stages3,
                                     start_i = 2, end_i = 6, IDs = IDs) %>%
  filter(Year > 1) # remove first year as parameters slightly different

output_data3b %>% group_by(Year, Stage) %>% summarise(count = n(),
                                                     repro = sum(Offspring)/count,
                                                     surv = sum(Surv)/count)

# CORRECT 08.24

################################################################################

#### Test observation process ####

source("./Functions/run_observation_process.R")

non_perfect_recap3 <- run_observation_process(state_data = output_data3, 
                                             p = c(1, 0.7, 0.7),
                                             phi = c(0.3, 0.4, 0.7),
                                             fecundity_error = FALSE,
                                             seed = 2,
                                             stages = stages3)

count_error_too3 <- run_observation_process(output_data3, 
                                           p = c(1, 0.7, 0.7),
                                           phi = c(0.3, 0.4, 0.7),
                                           fecundity_error = TRUE,
                                           seed = 2,
                                           stages = stages3)

# number of individuals is reduced do they all have recap = 1
summary(non_perfect_recap3) # YES CORRECT 08.24

# are all juveniles still there SHOULD BE 0
length(which(output_data3$Age == 1)) - 
  length(which(non_perfect_recap3$Age == 1)) # YES CORRECT 08.24

# how many individuals removed?
length(unique(non_perfect_recap3$ID))/
  length(unique(output_data3$ID))
   # 98 % remain

# check individuals by stage SHOULD BE 70% 
length(non_perfect_recap3$ID[which(non_perfect_recap3$Stage == "subadult")])/
  length(output_data3$ID[which(output_data3$Stage == "subadult")])
length(non_perfect_recap3$ID[which(non_perfect_recap3$Stage == "adult")])/
  length(output_data3$ID[which(output_data3$Stage == "adult")])

# CORRECT 08.24

# are fecundity counts different? NOT 0
count_error_too3$Offspring - count_error_too3$Offspring_obs # CORRECT 08.24

###############################################################################

#### TEST: missing not at random ####

## Set up inputs

phi = c(0.3, 0.4, 0.7)
names(phi) <- c("juvenile","subadult", "adult")
recapture <- c(1,1,1)
missing <- c(0.7,0.7,0.7)
offset <- 0.2
split <- 0.5

# set up IDs

output_random <- run_observation_process(output_data3,
                                         p = recapture*missing,
                                         fecundity_error = FALSE,
                                         phi = phi,
                                         stages = stages3,
                                         random = TRUE)

# RUNNING WITH CHECK ON SO CAN SEE RECAPTURE RATES
output_not_random_high <- run_observation_process(output_data3,
                                                  p = recapture*missing,
                                                  fecundity_error = FALSE,
                                                  phi = phi,
                                                  stages = stages3,
                                                  repo_stages = c("subadult", 
                                                                  "adult"),
                                                  split = split,
                                                  bias = "high",
                                                  offset = offset,
                                                  random = FALSE,
                                                  do_checks = TRUE)

output_not_random_low <- run_observation_process(output_data3,
                                                 p = recapture*missing,
                                                 fecundity_error = FALSE,
                                                 phi = phi,
                                                 stages = stages3,
                                                 repo_stages = c("subadult", 
                                                                 "adult"),
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
length(output_not_random_high[,1])/length(output_data3[,1]) # 69.3 % remain 08.24
length(output_not_random_low[,1])/length(output_data3[,1]) # 69.3 % remain 08.24
length(output_random[,1])/length(output_data3[,1]) # 70% remain 08.24

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

################################################################################

#### CHECK MODEL IN WRAPPER ####

source("./Functions/make_input_data_function.R")
library(nimbleEcology)
library(MCMCvis)

# check eigenvalues of input parameters
eigen(parameters3)

source("./Scripts/T1.1_Model_hmm_33.R")

model_inputs3 <- make_input_data(output_data3, n_occasions = 5,
                                 stages = stages3,
                                 reproduction_data = output_not_random_high)

# double check that reproduction_data is different
model_inputs3$data_input$offspring_obs_a - 
  filter(output_data3, Stage == "adult")$Offspring 
# CORRECT AS NOT EVEN SAME LENGTH - 08.24

output_results3 <- nimbleMCMC(code = Model_SS_hmm, 
                             data = model_inputs3$data_input,
                             constants = model_inputs3$constants,
                             inits = model_inputs3$inits,
                             monitors = model_inputs3$parameters_to_save,
                             niter = 5000,
                             nburnin = 500,
                             nchains = 2)

MCMCsummary(output_results3, round = 2)
parameters3

# MODEL WORKING AND ESTIMATING CORRECT PARAMETERS 08.24

#### CHECK DIRECT ESTIMATE MODEL ####

library(popbio)

# source functions
source("./Functions/make_matrix.R")
source("./Functions/transition_frequency.R")
source("./Functions/bootstrap_tf.R")
source("./Functions/bootstrap_summary.R")


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


