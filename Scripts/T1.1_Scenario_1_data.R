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

input_data <- data.frame(ID = sample(1:100, 100, replace = FALSE),
                         Year = 1,
                         Surv = 1,
                         Recap = 1,
                         Offspring = rpois(100, 2),
                         Age = sample(1:5, 100, replace = TRUE),
                         Trait = rnorm(100, 20, 5))
# set up max age

max_age = 5

# make sure Recap and Surv = 0 for all of max age

input_data[which(input_data$Age == max_age), c("Recap", "Surv")] <- 0

# set up parameters 

parameters = matrix(c(1.6, rep(2, 4),
                      0.5, 0, 0, 0, 0,
                      0, 0.7, 0, 0, 0,
                      0, 0, 0.7, 0, 0,
                      0, 0, 0, 0.7, 0), 
                    byrow = TRUE, 
                    ncol = max_age)

# set up recapture probabilities

recapture <- rep(0.8, max_age)

# set up IDs

IDs <- 101:1000000

#### TEST ####

for(i in 2:10){
output_data <- run_simulation(input_data_old = input_data, 
               parameters = parameters, 
               p = recapture, 
               max_age = max_age,
               inc_trait = FALSE,
               obs_error = FALSE,
               i = i, IDs = IDs) 
input_data <- output_data
}

#### Simulation 1: missing reproductive events (at random) ####

#### Simulation 2: missing reproductive events (not at random - bias) ####

#### Simulation 3: count error in offspring numbers (random) ####

#### Simulation 4: count error in offspring (biased) ####

