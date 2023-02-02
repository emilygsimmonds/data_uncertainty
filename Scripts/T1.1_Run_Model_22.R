# T1.1: Script to run the nimble model #

## T1.1_Run_Model_22 ##

################################################################################

# This script is a wrapper to run the nimble model to estimate parameters
# from the different simulated datasets. 

################################################################################

#### Set up ####

# load packages

library(tidyverse)
library(nimble)
library(nimbleEcology)
library(furrr)
library(magrittr)

#library(tidyverse, lib = "/cluster/home/emilygs/myRpackages/")
#library(nimble, lib = "/cluster/home/emilygs/myRpackages/")
#library(nimbleEcology, lib = "/cluster/home/emilygs/myRpackages/")
#library(future, lib = "/cluster/home/emilygs/myRpackages/")
#library(furrr, lib = "/cluster/home/emilygs/myRpackages/")
#library(magrittr, lib = "/cluster/home/emilygs/myRpackages/")

# source model
source("./Scripts/T1.1_Model_hmm_22.R")

# source functions
source("./Functions/make_input_data_function.R")
source("./Functions/run_model.R")

# load data

# get list of all baseline data files
filenames <- list.files("./Data files/2x2", 
                        pattern = "baseline_simulation_observations",
                        full.names = TRUE)

################################################################################

#### Edit data ####

# take all of the simulated datasets and edit into model inputs
model_inputs <- map(.x = filenames, ~{
  load(.x)
  map(.x = baseline_observations,
                    .f = make_input_data, 
                    n_occasions = 5,
                    fecundity_error = FALSE,
                    stages = c("juvenile", "adult"))})

#### Run set up ####

n_iter <- 50000
n_burnin <- 500
n_chains <- 2

#### run all baseline p_juv = 1 ####

y <- as.list(1:100) # list of individual scenarios
z <- as.list("mat1", "mat2", "mat3", "mat4", "mat5")

plan(multisession, workers = 4)

# run as future_pmap
future_pmap(list(inputs = unlist(model_inputs, recursive = FALSE)[1:3],
                 niter = as.list(rep(50000, 3)),
                 nburnin = as.list(rep(500, 3)),
                 scenario = as.list(1:3),
                 mat_num = as.list(rep("mat1", 3)),
                 location = as.list(rep("./Data files/2x2/Baseline/baseline_result_", 3))), 
                 run_model, .options = furrr_options(seed = TRUE,
                                                packages = c("nimble",
                                                             "nimbleEcology")))


################################################################################
################################################################################

##### Random error #####
#
## take all of the simulated datasets and now edit to use offspring observed with error
#model_inputs <- map(.x = filenames, ~{
#  load(.x)
#  map(.x = baseline_observations,
#      .f = make_input_data, 
#      n_occasions = 5,
#      fecundity_error = TRUE,
#      stages = c("juvenile", "adult"))})
#
#plan(multisession, workers = 4)
#
## first map across 5 matrices
#map2(.x = model_inputs,
#     .y = z, ~{
#       z <- .y # index of matrix
#       map2(.x = .x,
#            .y = y, ~{ 
#              model_result <- nimbleMCMC(code = Model_SS_hmm, 
#                                         data = .x$data_input,
#                                         constants = .x$constants,
#                                         inits = .x$inits,
#                                         monitors = .x$parameters_to_save,
#                                         niter = n_iter,
#                                         nburnin = n_burnin,
#                                         nchains = n_chains)
#              
#              # save out result at each pass
#              save(model_result, file = paste0("/cluster/work/emilygs/DU/R_error/R_error_result_", .y, z, ".RData"))
#              return(NULL)})
#       return(NULL)
#     }, .progress = TRUE)
#