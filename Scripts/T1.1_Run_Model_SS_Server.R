# T1.1: Script to run the nimble model #

## T1.1_Run_Model_SS_Server ## Server version of the code

################################################################################

# This script is a wrapper to run the nimble model to estimate parameters
# from the different simulated datasets. 

################################################################################

#### Set up ####

# load packages

library(tidyverse)
library(nimble)
library(nimbleEcology)
library(MCMCvis)
library(furrr)
library(magrittr)

# source model
source("./Scripts/T1.1_Model_SS_hmm.R")

# source functions
source("./Functions/make_input_data_function.R")

# load data

load("./Data files/baseline_simulation_observations.RData")

################################################################################

#### Edit data ####

# take all of the simulated datasets and edit into model inputs
model_inputs <- map(.x = baseline_observations, 
                    .f = make_input_data, 
                    n_occasions = 10,
                    fecundity_error = FALSE)

#### Run set up ####

n_iter <- 50000
n_burnin <- 500
n_chains <- 2


#### run all baseline p_juv = 1 ####

y <- as.list(1:100)

plan(multisession, workers = 2)

start_time <- Sys.time()

output_baseline_all <- map2(.x = model_inputs,
                            .y = y, ~{
  model_result <- nimbleMCMC(code = Model_SS_hmm, 
             data = .x$data_input,
             constants = .x$constants,
             inits = .x$inits,
             monitors = .x$parameters_to_save,
             niter = n_iter,
             nburnin = n_burnin,
             nchains = n_chains)
  # save out result at each pass
  save(model_result, file = paste0("baseline_SS_result", .y, ".RData"))
}, .progress = TRUE)

end_time <- Sys.time()
end_time - start_time


