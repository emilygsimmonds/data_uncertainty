# T1.1: Data simulation for scenario 1: bias and error in fecundity #

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
#
################################################################################

#### Set up ####

# load packages

library(tidyverse)

# load any data

# source necessary functions
source("./Functions/run_simulation.R")
source("./Functions/run_observation_process.R")

#### Create simulated data ####

# set up input data

input_data <- data.frame(ID = sample(1:200, 200, replace = FALSE),
                         Year = 1,
                         Surv = rbinom(200, 1, prob = 0.4),
                         Offspring = rpois(200, 1),
                         Age = sample(1:5, 200, replace = TRUE),
                         Trait = rnorm(200, 20, 5))
# set up max age

max_age = 5

# make sure Surv = 0 for all of max age

input_data[which(input_data$Age == max_age), c("Surv")] <- 0

# set up parameters - 
# began close to Riecke paper then upped repro to give slightly growing pop

parameters = matrix(c(rep(0.75, 5),
                      0.3, 0, 0, 0, 0,
                      0, 0.5, 0, 0, 0,
                      0, 0, 0.5, 0, 0,
                      0, 0, 0, 0.5, 0), 
                    byrow = TRUE, 
                    ncol = max_age) # made sure that lambda is approx 1!!

# set up recapture probabilities

recapture <- 0.8

# set up IDs

IDs <- 101:200000000

#### TEST ####

# run state simulation
output_data <- run_simulation_state(input_data_old = input_data, 
                              parameters = parameters, 
                              max_age = max_age,
                              inc_trait = FALSE,
                              start_i = 2, end_i = 10, IDs = IDs) 

# then observation process
observation <- run_observation_process(output_data, 
                                       p = recapture,
                                       juv_recapture = TRUE,
                                       fecundity_error = FALSE)

save(output_data, file = "./Data files/test.RData")

x <- output_data %>% group_by(Year) %>% summarise(count = n(),
                                             repro = sum(Offspring))


#### Simulation 1: missing reproductive events (at random) ####

### BASELINE INCLUDES COLUMN OF OBSERVATION ERROR IN FECUNDITY 

seeds <- as.list(c(1:100))

# run normal set of simulations then edit
baseline <- map(.x = seeds, ~{
  state <- run_simulation_state(defined_seed = .x,
                       input_data_old = input_data, 
                       parameters = parameters, 
                       max_age = max_age,
                       inc_trait = FALSE,
                       start_i = 2, end_i = 10, IDs = IDs)
  observations <- run_observation_process(state,
                                          p = recapture,
                                          juv_recapture = TRUE,
                                          fecundity_error = TRUE)
  }) 

# save
save(baseline, file = "./Data files/baseline_simulation.RData")

# randomly add 0s to the offspring column 10%

random_missing_reproduction <- map(.x = baseline, ~{
  set.seed(1)
  marker <- sample(1:length(.x$Offspring), length(.x$Offspring)/10)
  .x$Offspring[marker] <- 0
  return(.x)
})

# check that random missing is different to baseline
length(which(baseline[[1]]$Offspring - 
               random_missing_reproduction[[1]]$Offspring != 0))
# YES are different

# save
save(random_missing_reproduction, 
     file = "./Data files/random_missing_simulation.RData")

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

length(which(baseline[[1]]$Offspring - 
               juvenile_missing_reproduction[[1]]$Offspring != 0))

# save
save(juvenile_missing_reproduction, 
     file = "./Data files/juvenile_missing_simulation.RData")

# miss adults
adult_missing_reproduction <- map(.x = baseline, ~{
  marker1 <- which(.x$Age > 1)
  set.seed(1)
  marker2 <- sample(marker1, length(marker1)/50)
  .x$Offspring[marker2] <- 0
  return(.x)
})

length(which(adult_missing_reproduction[[1]]$Offspring - 
  juvenile_missing_reproduction[[1]]$Offspring != 0))

# save
save(adult_missing_reproduction, 
     file = "./Data files/adult_missing_simulation.RData")

#### Simulation 3: count error in offspring numbers (random) ####

# already included in baseline simulation just need to choose which
# column to put in model


