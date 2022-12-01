# FUNCTION to create all scenario data #

## Function cleans previous output data to become new input data

## INPUT :
#
# - parameter matrix and max age and name of scenario
# - juvenile and adult recapture rates

## OUTPUT = saves out simulated datasets

# source necessary functions
source("./Functions/run_simulation.R")
source("./Functions/run_observation_process.R")

#### FUNCTION ####

create_scenario_data <- function(parameters,
                                 max_age, 
                                 name,
                                 recapture_a, recapture_j){
  
#### Create simulated data ####

# set up i

i <- as.list(1:100)

# create first year of data for each matrix

input_data <- map(.x = i, ~{simulation_setup(parameter_matrix = parameters,
                                                 i = .x,
                                                 max_age = max_age)})
  
# set up recapture probabilities

recapture <- recapture_a

# set up IDs

IDs <- 101:200000000

#### Simulation 1: missing reproductive events (at random) ####
  
# Simulate the state only

seeds <- as.list(c(1:100))
max_age <- max_age

# run normal set of simulations then edit
baseline_state <- map(.x = seeds, ~{
  state <- run_simulation_state(defined_seed = .x,
                                input_data_old = input_data, 
                                parameters = parameters, 
                                max_age = max_age,
                                inc_trait = FALSE,
                                start_i = 2, end_i = 10, IDs = IDs)
  return(state)
}) 

# save
save(baseline_state, 
     file = paste0("./Data files/baseline_simulation_state", name, ".RData"))

### ADD IN OBSERVATION ERROR INC. COLUMN OF OBSERVATION ERROR IN FECUNDITY
# can be removed at modelling stage

baseline_observations <- map2(.x = baseline_state,
                              .y = seeds, ~{run_observation_process(.x,
                              p_adult = recapture,
                              p_juvenile = recapture_j,
                              fecundity_error = TRUE,
                              phi_adult = parameters[2,2],
                              phi_juvenile = parameters[2,1],
                              seed = .y)
                              })

# save
save(baseline_observations, 
     file = paste0("./Data files/baseline_simulation_observations", name, ".RData"))

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
file = paste0("./Data files/random_missing_simulation", name, ".RData"))

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

# save
save(juvenile_missing_reproduction, 
file = paste0("./Data files/juvenile_missing_simulation", name, ".RData"))

# miss adults
adult_missing_reproduction <- map(.x = baseline_observations, ~{
  marker1 <- which(.x$Age > 1)
  set.seed(1)
  marker2 <- sample(marker1, length(marker1)/50)
  .x <- .x %>% mutate(Offspring_obs = Offspring)
  .x$Offspring_obs[marker2] <- 0
  return(.x)
})


# save
save(adult_missing_reproduction, 
     file = paste0("./Data files/adult_missing_simulation", name, ".RData"))

}