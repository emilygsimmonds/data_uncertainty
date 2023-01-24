# Script to run checks for model code to make sure it is performing correctly #

################################################################################

## Source functions to test ####

source("./Functions/make_input_data_function.R")

## Source model code ####

source("./Scripts/T1.1_Model_SS_hmm.R")

## Source required packages ####

library(tidyverse)
library(nimble)
library(nimbleEcology)

## Load test dataset ##

load('./Data files/test.RData')

#### CHECK OBSERVATION AND TRANSITION MATRICES ####

## Set up parameter values
mean_phi_adult <- 0.6
mean_phi_juv <- 0.3
mean_p <- 0.8
fecundity <- 1.6

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

# what is prob of adult being observed as adult in t+1
manual_ad_to_ad <- mean_phi_adult * mean_p
matrix_ad_to_ad <- transition[2,2] * observations[2,2]

# what is prob of juvenile dead being observed as dead in t+1
manual_juv_to_dead <- (1-mean_phi_juv) 
matrix_juv_to_dead <- transition[1,3] * observations[3,3]

#### CHECK MODEL RUN ####

# True values: phi_j = 0.3, phi_a = 0.6, p = 0.8, lambda = 1

## Set up data
input_data <- output_data

# re-code raw data so that adult = 2, juvenile = 1
output_data <- input_data %>%
  filter(Recap == 1) %>% # only keep those that were recaptured
  mutate(Surv = case_when(Age > 1 ~ 2, 
                          TRUE ~ Recap), 
         Age = case_when(Age == 1 ~ 1,
                         Age > 1 ~ 2)) # change age to just juv (1) and adult (2)

## Define constants, data and inits

## DATA

# need to make a capture history and save out age and offspring columns
offspring_obs <- output_data$Offspring
age <- output_data$Age

# capture history
capture_history <- output_data %>%
  # spread out data. The fill = 3 fills in 3s when combo was not observed (dead/non detected)
  pivot_wider(id_cols = ID, names_from = Year, values_from = Surv,
              values_fill = 3) %>%
  as.matrix()

# store ready for model
data_input <- list(surv_obs = capture_history[,2:26],
                   age = age,
                   offspring_obs = offspring_obs)

## INITS

# set up initial values for survival states (1 on first occasion and 2 after)
surv_state_init <- capture_history[,2:26]
# create a vector of first occasions (THIS WILL BE A CONSTANT)
first <- apply(surv_state_init, 1, function(x) which(x<3)[1])
for(i in 1:nrow(surv_state_init)){
  if(first[i] < 23)
    surv_state_init[i, (first[i]+1):23] <- 2}

# for hmm remove any individuals first seen in the final capture occasion
data_input$surv_obs <- data_input$surv_obs[-which(first > 23), ]

# initial values for MPM parameters
lambda <- 1
transition_matrix <- nimMatrix(c(1,0.5,
                                 1,0.5), 
                               nrow = 2)
size_distribution <- c(0.5, 0.5)

inits_hmm <- list(mean_phi_juv = runif(1, 0, 1),
                  mean_phi_adult = runif(1, 0, 1),
                  mean_p = runif(1, 0, 1),
                  alpha = rnorm(1, 0, 0.1),
                  beta_age = rnorm(1, 0, 0.1),
                  offspring_state = offspring_obs,
                  fecundity_rate = rep(1, length(offspring_obs)))

## CONSTANTS

constants <- list(N = nrow(data_input$surv_obs), 
                  occasions = 25, 
                  first = first[-which(first > 23)],
                  O_N = length(offspring_obs))

## Define parameters to track

parameters_to_save <- c("mean_phi_adult",
                        "mean_phi_juv",
                        "mean_p",
                        "alpha",
                        "beta_age",
                        "transition_matrix",
                        "lambda", 
                        "size_distribution"
)

#### TEST: run model ####

n_iter <- 5000
n_burnin <- 50
n_chains <- 2

start_time <- Sys.time()

mcmc.output <- nimbleMCMC(code = Model_SS_hmm,
                          constants = constants,
                          data = data_input,
                          inits = inits_hmm,
                          monitors = parameters_to_save,
                          niter = n_iter,
                          nburnin = n_burnin,
                          nchains = n_chains)

end_time <- Sys.time()
end_time - start_time

MCMCsummary(mcmc.output, round = 2)

#### CHECK MODEL IN WRAPPER ####

parameters_to_save <- c("mean_phi_adult",
                        "mean_phi_juv",
                        "mean_p_juv",
                        "mean_p_adult",
                        "alpha",
                        "beta_age",
                        "transition_matrix",
                        "lambda", 
                        "size_distribution"
)

n_iter <- 50000
n_burnin <- 500
n_chains <- 2

load("./Data files/baseline_simulation_statemat1.RData")

# True values: phi_j = 0.3, phi_a = 0.4, p = 0.8, lambda = 0.7

model_inputs <- make_input_data(baseline_state[[1]], n_occasions = 5)

output_results <- nimbleMCMC(code = Model_SS_hmm, 
             data = model_inputs$data_input,
             constants = model_inputs$constants,
             inits = model_inputs$inits,
             monitors = parameters_to_save,
             niter = n_iter,
             nburnin = n_burnin,
             nchains = n_chains)

MCMCsummary(output_results, round = 2)

#### CHECK DIRECT ESTIMATE MODEL ####

library(popbio)

# source functions
source("./Functions/make_input_data_function.R")
source("./Functions/transition_frequency.R")

# load data - test on the state
load("./Data files/baseline_simulation_statemat1.RData")

#### construct a transition frequency table

# for the state
tf_table_state <- create_transition_frequency_table(baseline_state[[1]],
                                                    max_year = max(baseline_state[[1]]$Year)) # seems to work :)
# make population matrix
make_matrix(tf_table_state)

#### run bootstrap to get CIs for vital rates and lambda



