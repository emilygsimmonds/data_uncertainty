# T1.1: Script to run the nimble model #

################################################################################

#### Set up ####

# load packages

library(tidyverse)
library(nimble)
library(nimbleEcology)
library(MCMCvis)

# source model

source("./Scripts/T1.1_Model_SS_hmm.R")
source("./Functions/make_input_data_function.R")

# load data

load("./Data files/test.RData")

input_data <- output_data

# re-code raw data so that adult = 2, juvenile = 1
output_data <- input_data %>%
  filter(Recap == 1) %>% # only keep those that were recaptured
  mutate(Surv = case_when(Age > 1 ~ 2, 
                          TRUE ~ Recap), 
         Age = case_when(Age == 1 ~ 1,
                         Age > 1 ~ 2)) # change age to just juv (1) and adult (2)

################################################################################

#### Define constants, data and inits ####

## DATA

# need to make a capture history and save out age and offspring columns
offspring_obs <- output_data$Offspring[which(output_data$Year < 11)]
age <- output_data$Age[which(output_data$Year < 11)]

# capture history
capture_history <- output_data %>%
  # spread out data. The fill = 3 fills in 3s when combo was not observed (dead/non detected)
  pivot_wider(id_cols = ID, names_from = Year, values_from = Surv,
              values_fill = 3) %>%
  as.matrix()

# remove any individuals never seen
capture_history <- capture_history[-which(rowSums(capture_history[,2:11])==30), ]

# store ready for model
data_input <- list(surv_obs = capture_history[,2:11],
                   age = age,
                   offspring_obs = offspring_obs)

## INITS

# set up initial values for survival states (1 on first occasion and 2 after)
surv_state_init <- capture_history[,2:11]
# create a vector of first occasions (THIS WILL BE A CONSTANT)
first <- apply(surv_state_init, 1, function(x) which(x<3)[1])
for(i in 1:nrow(surv_state_init)){
  if(first[i] < 9)
    surv_state_init[i, (first[i]+1):9] <- 2}

# initial values for MPM parameters
lambda <- 1
transition_matrix <- nimMatrix(c(1,0.5,
                                 1,0.5), 
                               nrow = 2)
size_distribution <- c(0.5, 0.5)

inits <- list(mean_phi_juv = runif(1, 0, 1),
              mean_phi_adult = runif(1, 0, 1),
              mean_p = runif(1, 0, 1),
              alpha = rnorm(1, 0, 0.1),
              beta_age = rnorm(1, 0, 0.1),
              surv_state = surv_state_init,
              offspring_state = offspring_obs,
              fecundity_rate = rep(1, length(offspring_obs)),
              lambda = lambda,
              transition_matrix = transition_matrix,
              size_distribution = size_distribution)

## CONSTANTS

# number of occasions (Occasions) and number of individuals (N)
constants <- list(N = nrow(capture_history), 
                  occasions = 9, 
                  first = first)

#### Define parameters to track ####

parameters_to_save <- c("mean_phi_adult",
                        "mean_phi_juv",
                        "mean_p",
                        "alpha",
                        "beta_age",
                        #"mean_fecundity_juv",
                        #"length_fecundity"
                        "transition_matrix",
                        "lambda", 
                        "size_distribution"
)

#### Run set up ####

n_iter <- 500
n_burnin <- 10
n_chains <- 2

#### Run model ####

Rmodel <- nimbleModel(code = Model_SS_raw,
                      constants = constants,
                      data = data_input,
                      inits = inits)

nimEigen(Rmodel$transition_matrix)

mcmc.output <- nimbleMCMC(code = Model_SS_raw,
                          constants = constants,
                          data = data_input,
                          inits = inits,
                          monitors = parameters_to_save,
                          niter = n_iter,
                          nburnin = n_burnin,
                          nchains = n_chains)

# for hmm remove any individuals first seen in the final capture occasion

data_input$surv_obs<- data_input$surv_obs[-which(first > 7), ]

# number of occasions (Occasions) and number of individuals (N)
constants <- list(N = nrow(data_input$surv_obs), 
                  occasions = 9, 
                  first = first[-which(first > 7)])

inits_hmm <- list(mean_phi_juv = runif(1, 0, 1),
              mean_phi_adult = runif(1, 0, 1),
              mean_p = runif(1, 0, 1),
              alpha = rnorm(1, 0, 0.1),
              beta_age = rnorm(1, 0, 0.1),
              offspring_state = offspring_obs,
              fecundity_rate = rep(1, length(offspring_obs)))

parameters_to_save <- c("mean_phi_adult",
                        "mean_phi_juv",
                        "mean_p",
                        "alpha",
                        "beta_age",
                        #"mean_fecundity_juv",
                        #"length_fecundity"
                        #"transition_matrix"
                        "lambda", 
                        "size_distribution"
)

mcmc.output <- nimbleMCMC(code = Model_SS_hmm,
                          constants = constants,
                          data = data_input,
                          inits = inits_hmm,
                          monitors = parameters_to_save,
                          niter = n_iter,
                          nburnin = n_burnin,
                          nchains = n_chains)

#### Check results ####

MCMCsummary(mcmc.output, round = 2)

#### TESTING WITH make_input_data_function ####

model_inputs <- make_input_data(filter(output_data, Year < 11), 
                                n_occasions = 10)

nimbleMCMC(code = Model_SS_hmm, 
           data = model_inputs[[1]],
           constants = model_inputs[[2]],
           inits = model_inputs[[3]],
           monitors = parameters_to_save,
           niter = n_iter,
           nburnin = n_burnin,
           nchains = n_chains)
