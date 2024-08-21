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
#source("./Functions/make_input_data_function.R")
source("./Functions/run_model_22.R")

# load data

# get list of all survival data file - random missing scenario
filenames <- list.files("./Data files/2x2", 
                        pattern = "random_missing",
                        full.names = TRUE)

# get list of all reproduction process data (not_random)
filenames0 <- list.files("./Data files/2x2_not_random", 
                        pattern = "not_random",
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

# save inputs as a dataframe for pmap and remove any models that have already run
inputs <- data.frame(filename = unlist(rep(filenames,2)),
                     niter = rep(50000, 1000),
                     nburnin = rep(500, 1000),
                     scenario = c(paste0(rep(1:100, 5),"high"),
                                  paste0(rep(1:100,5), "low")),
                     mat_num = rep(rep(z, each = 100),2),
                     location = rep("/cluster/work/emilygs/DU/2x2_not_random/not_random_result_", 100),
                     num_stages = rep(2, 1000),
                     fecundity_error = rep(FALSE,1000),
                     reproduction_data = unlist(filenames0))

# remove files that have already been run
filenames2 <- list.files("/cluster/work/emilygs/DU/2x2_not_random", 
                         pattern = "not_random_result_",
                         full.names = FALSE)

# make a dataframe of those run
if(length(filenames2 > 0)){
#already_run <- data.frame(scenario = parse_number(str_sub(filenames2, 19, -11)),
#                          matrix_number = str_sub(filenames2, -10, -7))
# changed due to not random file names
already_run <- data.frame(scenario = str_sub(filenames2, 19, -11),
                          matrix_number = str_sub(filenames2, -10, -7))
  
# now remove them
marker <- map2(.x = as.list(already_run$matrix_number),
               .y = as.list(already_run$scenario), ~{
                 marker <- which(inputs$scenario == .y &
                                   inputs$mat_num == .x)
                 return(marker)
               })

inputs <- inputs[-unlist(marker),]}

# run as future_pmap
future_pmap(inputs, 
            run_model, .options = furrr_options(seed = TRUE,
                                                packages = c("nimble",
                                                             "nimbleEcology",
                                                             "tidyverse")))


##############################################################################
