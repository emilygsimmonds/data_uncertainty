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

#### Create simulated data ####

# set up max age

max_age = 5

# set up parameters - 
# began close to Riecke paper then upped repro to give slightly growing pop

parameters = matrix(c(0.6, rep(0.8, 4),
                      0.3, 0, 0, 0, 0,
                      0, 0.5, 0, 0, 0,
                      0, 0, 0.5, 0, 0,
                      0, 0, 0, 0.5, 0), 
                    byrow = TRUE, 
                    ncol = max_age) # made sure that lambda is approx 1!!

# set up i

i <- as.list(1:100)

input_data <- map(.x = i, ~{simulation_setup(parameter_matrix = parameters,
                                             i = .x,
                                             max_age = max_age)})


# set up recapture probabilities

recapture <- 0.8

# set up IDs

IDs <- 101:200000000

#### TEST ####

# run state simulation
output_data <- run_simulation_state(input_data_old = input_data[[3]], 
                              parameters = parameters, 
                              max_age = max_age,
                              inc_trait = FALSE,
                              start_i = 2, end_i = 10, IDs = IDs)

((200*0.5)+(100*0.5)+(50*0.5)+(25*0))/(200+100+50+25) 
# 46.6667 = actual adult survival

# checks
mean(output_data$Offspring[output_data$Age == 1])
mean(output_data$Offspring[output_data$Age > 1])
mean(output_data$Surv[output_data$Age == 1])
mean(output_data$Surv[output_data$Age > 1])

# then observation process
observation <- run_observation_process(output_data, 
                                       p_adult = recapture,
                                       p_juvenile = 1,
                                       phi_juvenile = 0.3,
                                       phi_adult = 0.5,
                                       fecundity_error = FALSE,
                                       seed = 2)

# number of juveniles = same

length(which(output_data$Age == 1))

length(which(observation$Age == 1))

# number of adults = reduced to 80%

length(which(observation$Age > 1))/
length(which(output_data$Age > 1))

save(observation, file = "./Data files/test.RData")

x <- output_data %>% group_by(Year) %>% summarise(count = n(),
                                             repro = sum(Offspring))

#### Simulation 1: missing reproductive events (at random) ####

# Simulate the state only

seeds <- as.list(c(1:100))

# run normal set of simulations then edit
baseline_state <- map2(.x = seeds,
                       .y = input_data, ~{
  state <- run_simulation_state(defined_seed = .x,
                       input_data_old = .y, 
                       parameters = parameters, 
                       max_age = max_age,
                       inc_trait = FALSE,
                       start_i = 2, end_i = 10, IDs = IDs)
  return(state)
  }) 

# save
save(baseline_state, file = "./Data files/baseline_simulation_state.RData")

### ADD IN OBSERVATION ERROR INC. COLUMN OF OBSERVATION ERROR IN FECUNDITY
# can be removed at modelling stage

baseline_observations <- map2(.x = baseline_state,
                              .y = seeds, ~{run_observation_process(.x,
                                        p_adult = recapture,
                                        p_juvenile = 1,
                                        fecundity_error = TRUE,
                                        phi_adult = 0.5,
                                        phi_juvenile = 0.3,
                                        seed = .y)
})

# save
save(baseline_observations, file = "./Data files/baseline_simulation_observations.RData")

# randomly add 0s to the offspring column 10%
# apply it to observations file as need recapture to be < 1

random_missing_reproduction <- map(.x = baseline_observations, ~{
  set.seed(1)
  marker <- sample(1:length(.x$Offspring), length(.x$Offspring)/10)
  .x <- .x %>% mutate(Offspring_obs = Offspring)
  .x$Offspring_obs[marker] <- 0
  return(.x)
})

# check that random missing is different to baseline
length(which(baseline_observations[[1]]$Offspring - 
               random_missing_reproduction[[1]]$Offspring_obs != 0))
# YES are different

# save
save(random_missing_reproduction, 
     file = "./Data files/random_missing_simulation.RData")

#### Simulation 2: missing reproductive events (not at random - bias) ####
# miss juveniles

# add 0s to juveniles in the offspring column 50%
juvenile_missing_reproduction <- map(.x = baseline_observations, ~{
  marker1 <- which(.x$Age == 1)
  set.seed(1)
  marker2 <- sample(marker1, length(marker1)/50)
  .x <- .x %>% mutate(Offspring_obs = Offspring)
  .x$Offspring_obs[marker2] <- 0
  return(.x)
})

length(which(baseline_observations[[1]]$Offspring - 
               juvenile_missing_reproduction[[1]]$Offspring_obs != 0))

# save
save(juvenile_missing_reproduction, 
     file = "./Data files/juvenile_missing_simulation.RData")

# miss adults
adult_missing_reproduction <- map(.x = baseline_observations, ~{
  marker1 <- which(.x$Age > 1)
  set.seed(1)
  marker2 <- sample(marker1, length(marker1)/50)
  .x <- .x %>% mutate(Offspring_obs = Offspring)
  .x$Offspring_obs[marker2] <- 0
  return(.x)
})

length(which(adult_missing_reproduction[[1]]$Offspring_obs - 
  juvenile_missing_reproduction[[1]]$Offspring_obs != 0))

# save
save(adult_missing_reproduction, 
     file = "./Data files/adult_missing_simulation.RData")



