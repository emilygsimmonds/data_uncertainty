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

# source functions
source("./Functions/make_input_data_function.R")

# load data

load("./Data files/test.RData")

load("./Data files/baseline_simulation_observations.RData")

#### Define parameters to track ####

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

#### Edit data ####

# take all of the simulated datasets and edit into model inputs
model_inputs <- map(.x = baseline_observations, 
                    .f = make_input_data, 
                    n_occasions = 10,
                    fecundity_error = FALSE)

model_inputs <- make_input_data(observation,
                                n_occasions = 10,
                                fecundity_error = FALSE)

#### Run set up ####

n_iter <- 50000
n_burnin <- 500
n_chains <- 2

#### Compile models ####

# Don't compile first as constants will change each time anyway

#### Run models ####

start_time <- Sys.time()

output_baseline <- map(.x = model_inputs[2], ~{
  nimbleMCMC(code = Model_SS_hmm, 
          data = .x$data_input,
          constants = .x$constants,
          inits = .x$inits,
          monitors = parameters_to_save,
          niter = n_iter,
          nburnin = n_burnin,
          nchains = n_chains)
})

end_time <- Sys.time()
end_time - start_time

MCMCsummary(output_baseline[[1]],  round = 2)

#### Check results ####

test <- nimbleMCMC(code = Model_SS_hmm, 
           data = model_inputs$data_input,
           constants = model_inputs$constants,
           inits = model_inputs$inits,
           monitors = parameters_to_save,
           niter = n_iter,
           nburnin = n_burnin,
           nchains = n_chains)

MCMCsummary(test,  round = 2)

#### 03.08.22 - COMES OUT WITH REALLY BAD ANSWERS. NEED TO CHECK - look at priors etc

#### 09.22 - better answers - was missing the first entry when using model so
# number of juveniles was practically 0. Now fixed.

#### 12.09.22 - all very good

#### run all baseline p_juv = 1 ####

start_time <- Sys.time()

output_baseline_all <- map(.x = model_inputs, ~{
  nimbleMCMC(code = Model_SS_hmm, 
             data = .x$data_input,
             constants = .x$constants,
             inits = .x$inits,
             monitors = parameters_to_save,
             niter = n_iter,
             nburnin = n_burnin,
             nchains = n_chains)
})

end_time <- Sys.time()
end_time - start_time

