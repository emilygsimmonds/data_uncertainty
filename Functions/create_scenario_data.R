# FUNCTION to create all scenario data #

## Function cleans previous output data to become new input data

## INPUT :
#
# - parameter matrix and name of scenario
# - vector of stages
# - vector of recapture rates
# - vector of survival rates
# - proportion missing
# - vector of reproductive stages

## OUTPUT = saves out simulated datasets

# source necessary functions
source("./Functions/run_simulation.R")
source("./Functions/run_observation_process.R")

#### FUNCTION ####

create_scenario_data <- function(parameters,
                                 stages, 
                                 name,
                                 recapture,
                                 phi,
                                 missing,
                                 repro_stages,
                                 location){
  
#### Create simulated data ####

# set up i

i <- as.list(1:100)

# create first year of data for each matrix

input_data <- map(.x = i, ~{simulation_setup(parameter_matrix = parameters,
                                                 i = .x,
                                                 stages = stages)})
  
# set up recapture probabilities

recapture <- recapture

# set up IDs

IDs <- 101:200000000

#### Simulation 1: missing reproductive events (at random) ####
  
# Simulate the state only

seeds <- as.list(c(1:100))

# run normal set of simulations then edit
baseline_state <- map2(.x = seeds,
                       .y = input_data, ~{
  state <- run_simulation_state(defined_seed = .x,
                                input_data_old = .y, 
                                parameters = parameters, 
                                stages = stages,
                                inc_trait = FALSE,
                                start_i = 2, end_i = 5, IDs = IDs)
  return(state)
}) 

# save
save(baseline_state, 
     file = paste0(location, length(stages), "baseline_simulation_state", name, ".RData"))

### ADD IN OBSERVATION ERROR INC. COLUMN OF OBSERVATION ERROR IN FECUNDITY
# can be removed at modelling stage

baseline_observations <- map2(.x = baseline_state,
                              .y = seeds, ~{run_observation_process(.x,
                              p = recapture,
                              fecundity_error = TRUE,
                              phi = phi,
                              seed = .y,
                              stages = stages)
                              })

# save
save(baseline_observations, 
     file = paste0(location, length(stages), "baseline_simulation_observations", name, ".RData"))

# randomly remove 40% individuals
# apply it to state file

random_missing_reproduction <- map2(.x = baseline_state,
                                    .y = seeds, ~{run_observation_process(.x,
                                                  p = recapture*missing,
                                                  fecundity_error = FALSE,
                                                  phi = phi,
                                                  seed = .y,
                                                  stages = stages)
                                    })
#map(.x = baseline_observations, ~{
#  set.seed(1)
#  marker <- sample(1:length(.x$Offspring), round(length(.x$Offspring)*0.20))
#  .x <- .x %>% mutate(Offspring_obs = Offspring)
#  .x$Offspring_obs[marker] <- 0
#  return(.x)
#})

# check that random missing is different to baseline
length(which(baseline_observations[[1]]$Offspring - 
               random_missing_reproduction[[1]]$Offspring_obs != 0))
# YES are different

# save
save(random_missing_reproduction, 
file = paste0(location, length(stages), "random_missing_simulation", name, ".RData"))

#### Simulation 2: missing reproductive events (not at random - bias) ####
# miss lowest breeding class

# reduce juvenile recapture to 0.6
juvenile_missing_reproduction <- map2(.x = baseline_state,
                                    .y = seeds, ~{run_observation_process(.x,
                                                  p = c(recapture[1]*missing,
                                                        recapture[2:length(recapture)]),
                                                  fecundity_error = FALSE,
                                                  phi = phi,
                                                  seed = .y,
                                                  stages = stages)
                                    })
  
#  map(.x = baseline_observations, ~{
#  marker1 <- which(.x$Stage == repro_stages[1])
#  set.seed(1)
#  marker2 <- sample(marker1, round(length(marker1)*0.2))
#  .x <- .x %>% mutate(Offspring_obs = Offspring)
#  .x$Offspring_obs[marker2] <- 0
#  return(.x)
#})

# save
save(juvenile_missing_reproduction, 
file = paste0(location, length(stages), "juvenile_missing_simulation", name, ".RData"))

# miss adults
adult_missing_reproduction <- map2(.x = baseline_state,
                                   .y = seeds, ~{run_observation_process(.x,
                                                 p = c(recapture[1],
                                                       recapture[2:length(recapture)]*missing),
                                                 fecundity_error = FALSE,
                                                 phi = phi,
                                                 seed = .y,
                                                 stages = stages)
                                   })
  
#  map(.x = baseline_observations, ~{
#  marker1 <- which(.x$Stage %in% repro_stages[-1])
#  set.seed(1)
#  marker2 <- sample(marker1, round(length(marker1)*0.2))
#  .x <- .x %>% mutate(Offspring_obs = Offspring)
#  .x$Offspring_obs[marker2] <- 0
#  return(.x)
#})


# save
save(adult_missing_reproduction, 
     file = paste0(location, length(stages), "adult_missing_simulation", name, ".RData"))

}