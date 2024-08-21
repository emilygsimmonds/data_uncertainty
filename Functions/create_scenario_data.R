# FUNCTION to create all scenario data for missing at random #

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
# BASELINE IS THE SYSTEM STATE WITH NO OBSERVATION PROCESSES
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

# randomly remove 30% individuals
# apply it to state file

random_missing_reproduction <- map2(.x = baseline_state,
                                    .y = seeds, ~{run_observation_process(.x,
                                                  p = recapture*missing,
                                                  fecundity_error = FALSE,
                                                  phi = phi,
                                                  seed = .y,
                                                  stages = stages)
                                    })

# check that random missing is different to baseline
length(which(baseline_observations[[1]]$Offspring - 
               random_missing_reproduction[[1]]$Offspring_obs != 0))
# YES are different

# save
save(random_missing_reproduction, 
file = paste0(location, length(stages), "random_missing_simulation", name, ".RData"))


}


