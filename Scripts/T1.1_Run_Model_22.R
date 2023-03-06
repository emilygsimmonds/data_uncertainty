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
source("./Scripts/T1.1_Model_hmm_33.R")

# source functions
#source("./Functions/make_input_data_function.R")
source("./Functions/run_model.R")

# load data

# get list of all baseline data files
filenames <- list.files("./Data files/3x3", 
                        pattern = "baseline_simulation_observations",
                        full.names = TRUE)

################################################################################

#### Edit data ####

#### Run set up ####

n_iter <- 50000
n_burnin <- 500
n_chains <- 2

#### run all baseline p_juv = 1 ####

y <- as.list(1:100) # list of individual scenarios
z <- c("mat1", "mat2", "mat3", "mat4", "mat5")

plan(multisession, workers = 8)

filenames <- c("./Data files/3x3/3juvenile_missing_simulationmat141.RDS")

# save inputs as a dataframe for pmap and remove any models that have already run
inputs <- data.frame(filename = filenames,
     niter = rep(50000, 1),
     nburnin = rep(500, 1),
     scenario = c(41),
     mat_num = c("mat1"),
     location = c("./Data files/3x3/a_J_missing/j_missing_result_"),
     num_stages = rep(3, 1),
     fecundity_error = c(TRUE))


# remove files that have already been run
filenames2 <- list.files("/cluster/work/emilygs/DU/2x2/Baseline/", 
                                      pattern = "baseline_result",
                                      full.names = FALSE)

# make a dataframe of those run
already_run <- data.frame(scenario = parse_number(str_sub(filenames2, 1, -11)),
                          matrix_number = str_sub(filenames2, -10, -7))

# now remove them
marker <- map2(.x = as.list(already_run$matrix_number),
              .y = as.list(already_run$scenario), ~{
    marker <- which(inputs$scenario == .y &
                      inputs$mat_num == .x)
    return(marker)
    })

inputs <- inputs[-unlist(marker),]

# run as future_pmap
future_pmap(inputs, 
            run_model, .options = furrr_options(seed = TRUE,
                                                packages = c("nimble",
                                                             "nimbleEcology",
                                                             "tidyverse")))


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