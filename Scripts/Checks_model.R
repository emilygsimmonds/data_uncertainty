# Script to run checks for model code to make sure it is performing correctly #

################################################################################

## Source functions to test ####

source("./Functions/make_input_data_function.R")
source("./Functions/run_simulation.R")

## Source required packages ####

library(tidyverse)
library(nimble)
library(nimbleEcology)
library(MCMCvis)

## Source model code ####

source("./Scripts/T1.1_Model_hmm_22.R")

## Make test dataset ####

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

phi = c(0.7, 0.9)
names(phi) <- c("juvenile", "adult")

# set up IDs

IDs <- 101:10000000

#### MAKE TEST DATA ####

test_data <- run_simulation_state(input_data_old = input_data, 
                                    parameters = parameters, 
                                    inc_trait = FALSE,
                                    start_i = 2, end_i = 5, IDs = IDs,
                                    stages = stages) 

#### CHECK OBSERVATION AND TRANSITION MATRICES ####

## Set up parameter values
mean_phi_adult <- 0.9
mean_phi_juv <- 0.9
mean_p <- 0.99
fecundity <- 1

## Define the matrices using same code as the model

# vector of initial state probabilities (juv surv, adult surv, death)
initial_survival <- rep(NA, 3)
initial_survival[1] <- 0.5
initial_survival[2] <- 0.5
initial_survival[3] <- 0

# matrix of transitions from juv, to adult, to dead (STATE)
# the probabilities should be the probability of being 1, 2 or 3
# STATES = alive juv 1, alive adult 2, dead 3
transition <- matrix(NA, nrow = 3, ncol = 3)
transition[1, 1] <- 0 # Pr(juv alive t -> juv alive t+1)
transition[1, 2] <- mean_phi_juv # Pr(juv alive t -> adult alive t+1)
transition[1, 3] <- 1-mean_phi_juv # Pr(juv alive t -> dead t+1)
transition[2, 1] <- 0 # Pr(adult alive t -> juv alive t+1)
transition[2, 2] <- mean_phi_adult # Pr(adult alive t -> adult alive t+1)
transition[2, 3] <- 1 - mean_phi_adult # Pr(adult alive t -> dead t+1)
transition[3, 1] <- 0 # Pr(dead t -> juvenile alive t+1)
transition[3, 2] <- 0 # Pr(dead t -> adult alive t+1)
transition[3, 3] <- 1 # Pr(dead t -> dead t+1)

# column 1 = observed juv, column 2 = observed adult, column 3 not detected
observations <- matrix(NA, nrow = 3, ncol = 3)
observations[1, 1] <- mean_p # Pr(juv alive t and detected t)
observations[1, 2] <- 0 # Pr(juv alive t and detected as adult t)
observations[1, 3] <- 1 - mean_p # Pr(juv alive t but not detected t)
observations[2, 1] <- 0 # Pr(adult alive t but detected as juv t)
observations[2, 2] <- mean_p # Pr(adult alive t and detected t)
observations[2, 3] <- 1 - mean_p # Pr(adult alive t and not detected t)
observations[3, 1] <- 0 # Pr(dead t and detected as juvenile t)
observations[3, 2] <- 0 # Pr(dead t and detected as adult t)
observations[3, 3] <- 1 # Pr(dead t and not detected t)

#### TEST: do matrices produce the same as manual? ####

# what is prob of juvenile being observed as adult in t+1
manual_juv_to_ad <- mean_phi_juv * mean_p
matrix_juv_to_ad <- transition[1,2] * observations[2,2]

# SHOULD BE 0 - correct 08.24
manual_juv_to_ad - matrix_juv_to_ad

# what is prob of adult being observed as adult in t+1
manual_ad_to_ad <- mean_phi_adult * mean_p
matrix_ad_to_ad <- transition[2,2] * observations[2,2]

# SHOULD BE 0 - correct 08.24
manual_ad_to_ad - matrix_ad_to_ad

# what is prob of juvenile dead being observed as dead in t+1
manual_juv_to_dead <- (1-mean_phi_juv) 
matrix_juv_to_dead <- transition[1,3] * observations[3,3]

# SHOULD BE 0 - correct 08.24
manual_juv_to_dead - matrix_juv_to_dead

#### CHECK CREATION OF INPUT DATA ####

# True values: phi_j = 0.9, phi_a = 0.9, p = 1, lambda = 1

## Set up data
model_inputs <- make_input_data(test_data,
                              n_occasions = 5,
                              fecundity_error = FALSE,
                              stages = c("juvenile", "adult"))



#### CHECK MODEL IN WRAPPER ####

n_iter <- 50000
n_burnin <- 500
n_chains <- 2

output_results <- nimbleMCMC(code = Model_SS_hmm, 
             data = model_inputs$data_input,
             constants = model_inputs$constants,
             inits = model_inputs$inits,
             monitors = model_inputs$parameters_to_save,
             niter = n_iter,
             nburnin = n_burnin,
             nchains = n_chains)

# True values: phi_j = 0.9, phi_a = 0.9, p = 1, lambda = 1

MCMCsummary(output_results, round = 2) # CORRECT 08.24

#### CHECK DIRECT ESTIMATE MODEL ####

library(popbio)

# source functions
source("./Functions/make_input_data_function.R")
source("./Functions/transition_frequency.R")
source("./Functions/bootstrap_summary.R")

# use above test data

#### construct a transition frequency table

# for the state
tf_table_state <- create_transition_frequency_table(test_data,
                                                    max_year = max(test_data$Year),
                                                    stages = stages) 

tf_table_state
# seems to work :) 08.24

# make population matrix
make_matrix(tf_table_state) # PRODUCES ROUGHLY OK ESTIMATES surv too high 08.24

#### run bootstrap to get CIs for vital rates and lambda

# for state
boot_results <- bootstrap_summary(tf_table_state, 
                                  iterations = 2000,
                                  stages = stages)

boot_results # works but survival still too high 08.24
