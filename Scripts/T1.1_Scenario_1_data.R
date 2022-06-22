# T1.1: Data simulation for scenario 1: bias and error in fecundity #

## MAKE SURE ALL SIMULATIONS ARE 25 YEARS - can then reduce for Scenario 3

################################################################################
#
# Features of the population:
# - female-based (reproduction is the number of juv. females)
# - does NOT include density-dependence
#
# Inputs:
# - Input_data = a dataframe with column names: ID (factor), Year (factor), 
# Surv (0/1), Recap (0/1), Offspring (num), Age (num), Trait (num)
#
# - parameters = matrix of parameter values (transition matrix) inc phi, f
#
# - max_age = maximum age species can get to
# 
# - inc_trait = TRUE or FALSE if you want to include a trait as well
#
# - obs_error = TRUE or FALSE whether there is observation error
#
#
################################################################################

#### Set up ####

# load packages

library(tidyverse)

# load any data

# source necessary functions
source("./Functions/run_simulation.R")

#### Create simulated data ####

# set up input data

input_data <- data.frame(ID = sample(1:1000, 100, replace = FALSE),
                         Year = 1,
                         Surv = 1,
                         Recap = 1,
                         Offspring = rpois(100, 2),
                         Age = sample(1:5, 100, replace = TRUE),
                         Trait = rnorm(100, 20, 5))

# set up recapture probability 

recapture <- 0.5

# set up survival

survival <- c(0.2, 0.5, 0.5)

# set up fertility

fertility <- c(1, 2, 2)

#### TEST ####

for(i in 2:10){
output_data <- run_simulation(input_data_old = input_data, 
               lambda = fertility, 
               phi = survival, 
               p = recapture, 
               condition_surv = "Age", 
               condition_repro = "Age",
               max_age = 3,
               inc_trait = FALSE,
               Obs_error = FALSE,
               i = i) 
input_data <- output_data
}

#### Simulation 1: missing reproductive events (at random) ####

#### Simulation 2: missing reproductive events (not at random - bias) ####

#### Simulation 3: count error in offspring numbers (random) ####

#### Simulation 4: count error in offspring (biased) ####

