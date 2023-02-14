## T1.1_Run_Model_SS_Server ## Server version of the code

################################################################################

#### Set up ####

# load packages

library(tidyverse, lib = "/cluster/home/emilygs/myRpackages/")
library(nimble, lib = "/cluster/home/emilygs/myRpackages/")
library(nimbleEcology, lib = "/cluster/home/emilygs/myRpackages/")
library(future, lib = "/cluster/home/emilygs/myRpackages/")
library(furrr, lib = "/cluster/home/emilygs/myRpackages/")
library(magrittr, lib = "/cluster/home/emilygs/myRpackages/")

# source model
source("./Scripts/T1.1_Model_SS_hmm.R")

# source functions
source("./Functions/make_input_data_function.R")

# load data

#load("./Data files/baseline_simulation_observations.RData")
#load("./Data files/juvenile_missing_simulation.RData")

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
#model_inputs <- map(
#  .x = baseline_observations,
#  #.x = juvenile_missing_reproduction, 
#                    .f = make_input_data, 
#                    n_occasions =5,
#                    fecundity_error = FALSE)
#                    # fecundity_error = TRUE)

#### Run set up ####

n_iter <- 50000
n_burnin <- 500
n_chains <- 2


#### run all baseline p_juv = 1 ####

y <- as.list(1:100)

#plan(multisession, workers = 4)

#output_all <- map2(.x = model_inputs,
#                            .y = y, ~{
#  model_result <- nimbleMCMC(code = Model_SS_hmm, 
#             data = .x$data_input,
#             constants = .x$constants,
#             inits = .x$inits,
#             monitors = parameters_to_save,
#             niter = n_iter,
#             nburnin = n_burnin,
#             nchains = n_chains)
#  # save out result at each pass
#  save(model_result, 
#       file = paste0("/cluster/work/emilygs/result2/baseline_SS_result",
#       #file = paste0("/cluster/work/emilygs/juvenile_missing_SS_result", 
#                     .y, ".RData"))
#}, .progress = TRUE)

load("./Data files/baseline_simulation_observationsmax.RData")

model_inputs <- map(
  .x = baseline_observations,
  #.x = juvenile_missing_reproduction, 
  .f = make_input_data, 
  n_occasions =5,
  fecundity_error = FALSE)
# fecundity_error = TRUE)

plan(multisession, workers = 4)

output_all <- map2(.x = model_inputs[78:100],
                   .y = y[78:100], ~{
                     model_result <- nimbleMCMC(code = Model_SS_hmm, 
                                                data = .x$data_input,
                                                constants = .x$constants,
                                                inits = .x$inits,
                                                monitors = parameters_to_save,
                                                niter = n_iter,
                                                nburnin = n_burnin,
                                                nchains = n_chains)
                     # save out result at each pass
                     save(model_result, 
                          file = paste0("/cluster/work/emilygs/result2/baseline_max_result",
                                        #file = paste0("/cluster/work/emilygs/juvenile_missing_SS_result", 
                                        .y, ".RData"))
                   }, .progress = TRUE)

#load("./Data files/baseline_simulation_observationsmax0.RData")
#
#model_inputs <- map(
#  .x = baseline_observations,
#  #.x = juvenile_missing_reproduction, 
#  .f = make_input_data, 
#  n_occasions =5,
#  fecundity_error = FALSE)
## fecundity_error = TRUE)
#
#plan(multisession, workers = 4)
#
#output_all <- map2(.x = model_inputs,
#                   .y = y, ~{
#                     model_result <- nimbleMCMC(code = Model_SS_hmm, 
#                                                data = .x$data_input,
#                                                constants = .x$constants,
#                                                inits = .x$inits,
#                                                monitors = parameters_to_save,
#                                                niter = n_iter,
#                                                nburnin = n_burnin,
#                                                nchains = n_chains)
#                     # save out result at each pass
#                     save(model_result, 
#                          file = paste0("/cluster/work/emilygs/result2/baseline_max0_result",
#                                        #file = paste0("/cluster/work/emilygs/juvenile_missing_SS_result", 
#                                        .y, ".RData"))
#                   }, .progress = TRUE)
#
#load("./Data files/baseline_simulation_observationsmin.RData")
#
#model_inputs <- map(
#  .x = baseline_observations,
#  #.x = juvenile_missing_reproduction, 
#  .f = make_input_data, 
#  n_occasions =5,
#  fecundity_error = FALSE)
## fecundity_error = TRUE)
#
#plan(multisession, workers = 4)
#
#output_all <- map2(.x = model_inputs,
#                   .y = y, ~{
#                     model_result <- nimbleMCMC(code = Model_SS_hmm, 
#                                                data = .x$data_input,
#                                                constants = .x$constants,
#                                                inits = .x$inits,
#                                                monitors = parameters_to_save,
#                                                niter = n_iter,
#                                                nburnin = n_burnin,
#                                                nchains = n_chains)
#                     # save out result at each pass
#                     save(model_result, 
#                          file = paste0("/cluster/work/emilygs/result2/baseline_min_result",
#                                        #file = paste0("/cluster/work/emilygs/juvenile_missing_SS_result", 
#                                        .y, ".RData"))
#                   }, .progress = TRUE)
#
#
#
#
#