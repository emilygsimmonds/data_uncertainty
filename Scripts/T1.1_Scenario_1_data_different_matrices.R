# T1.1: Data simulation for scenario 1: bias and error in fecundity #

################################################################################
#
# Features of the population:
# - female-based (reproduction is the number of juv. females)
# - does NOT include density-dependence
#
# Inputs:
# - Input_data = a dataframe with column names: ID (factor), Year (factor), 
# Surv (0/1), Recap (0/1), Clutch_size (num), Offspring (num), Age (num), 
# Trait (num)
#
# - parameters = matrix of parameter values (transition matrix) inc phi, f
#
# - max_age = maximum age species can get to
# 
# - inc_trait = TRUE or FALSE if you want to include a trait as well
#
#
################################################################################

#### Set up ####

# load packages

library(tidyverse)

# load any data

# source necessary functions
source("./Functions/run_simulation.R")
source("./Functions/run_observation_process.R")
source("./Functions/create_scenario_data.R")

#### Create matrices ####

min_no0 <- matrix(c(0.02,0.11,0.49,0.68), byrow = TRUE,
                  nrow = 2)

max_no0 <- matrix(c(5.41,5.41,0.78,0.78), byrow = TRUE,
                  nrow = 2)

max_0 <- matrix(c(0,100,0.15,0.1), byrow = TRUE,
                  nrow = 2)

#### Create simulated data ####

create_scenario_data(parameters = min_no0,
                     name = "min", recapture_a = 0.8, recapture_j = 1,
                     max_age = 2)

create_scenario_data(parameters = max_no0,
                     name = "max", recapture_a = 0.8, recapture_j = 1,
                     max_age = 2)

create_scenario_data(parameters = max_0,
                     name = "max0", recapture_a = 0.8, recapture_j = 1,
                     max_age = 2)

