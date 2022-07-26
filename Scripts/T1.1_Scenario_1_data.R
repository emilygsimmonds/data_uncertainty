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

input_data <- data.frame(ID = sample(1:200, 200, replace = FALSE),
                         Year = 1,
                         Surv = rbinom(200, 1, prob = 0.6),
                         Recap = rbinom(200, 1, prob = 0.8),
                         Offspring = rpois(200, 1),
                         Age = sample(1:5, 200, replace = TRUE),
                         Trait = rnorm(200, 20, 5))
# set up max age

max_age = 5

# make sure Recap and Surv = 0 for all of max age

input_data[which(input_data$Age == max_age), c("Recap", "Surv")] <- 0

# set up parameters 

parameters = matrix(c(rep(1, 5),
                      0.3, 0, 0, 0, 0,
                      0, 0.6, 0, 0, 0,
                      0, 0, 0.6, 0, 0,
                      0, 0, 0, 0.6, 0), 
                    byrow = TRUE, 
                    ncol = max_age)

# set up recapture probabilities

recapture <- rep(0.8, max_age)

# set up IDs

IDs <- 101:200000000

#### TEST ####

output_data <- run_simulation(input_data_old = input_data, 
                              parameters = parameters, 
                              p = recapture, 
                              max_age = max_age,
                              inc_trait = FALSE,
                              obs_error = FALSE,
                              start_i = 2, end_i = 25, IDs = IDs) 

save(output_data, file = "test.RData")

output_data %>% group_by(Year) %>% summarise(count = n(),
                                             repro = sum(Offspring))


#### Simulation 1: missing reproductive events (at random) ####

# run normal set of simulations then edit
baseline <- rerun(100, run_simulation(input_data_old = input_data, 
                              parameters = parameters, 
                              p = recapture, 
                              max_age = max_age,
                              inc_trait = FALSE,
                              obs_error = FALSE,
                              defined_seed = 1,
                              start_i = 2, end_i = 25, IDs = IDs))

# save
save(baseline, file = "baseline_simulation.RData")

# randomly add 0s to the offspring column 10%

random_missing_reproduction <- map(.x = baseline, ~{
  set.seed(1)
  marker <- sample(1:length(.x$Offspring), length(.x$Offspring)/10)
  .x$Offspring[marker] <- 0
  return(.x)
})

# check that random missing is different to baseline
baseline[[1]]$Offspring - random_missing_reproduction[[1]]$Offspring 
# YES are different

# save
save(random_missing_reproduction, file = "random_missing_simulation.RData")

#### Simulation 2: missing reproductive events (not at random - bias) ####
# miss juveniles

# add 0s to juveniles in the offspring column 50%
juvenile_missing_reproduction <- map(.x = baseline, ~{
  marker1 <- which(.x$Age == 1)
  set.seed(1)
  marker2 <- sample(marker1, length(marker1)/50)
  .x$Offspring[marker2] <- 0
  return(.x)
})

baseline[[1]]$Offspring - juvenile_missing_reproduction[[1]]$Offspring

# save
save(juvenile_missing_reproduction, file = "juvenile_missing_simulation.RData")

# miss adults
adult_missing_reproduction <- map(.x = baseline, ~{
  marker1 <- which(.x$Age > 1)
  set.seed(1)
  marker2 <- sample(marker1, length(marker1)/50)
  .x$Offspring[marker2] <- 0
  return(.x)
})

adult_missing_reproduction[[1]]$Offspring - 
  juvenile_missing_reproduction[[1]]$Offspring

# save
save(adult_missing_reproduction, file = "adult_missing_simulation.RData")

#### Simulation 3: count error in offspring numbers (random) ####

# run set of simulations with obs error
obs_error_simulation <- rerun(100, run_simulation(input_data_old = input_data, 
                                      parameters = parameters, 
                                      p = recapture, 
                                      max_age = max_age,
                                      inc_trait = FALSE,
                                      obs_error = TRUE,
                                      defined_seed = 1,
                                      start_i = 2, end_i = 25, IDs = IDs))

# save
save(obs_error_simulation, file = "obs_error_simulation.RData")


