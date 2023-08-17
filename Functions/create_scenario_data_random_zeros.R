# FUNCTION to create all scenario data for random zeros scenario #

## Function cleans previous output data to become new input data

## INPUT :
#
# - parameter matrix and name of scenario
# - vector of stages
# - vector of recapture rates
# - vector of survival rates
# - vector of reproductive stages

## OUTPUT = saves out simulated datasets

# source necessary functions
source("./Functions/run_simulation.R")
source("./Functions/run_observation_process.R")

#### FUNCTION ####

create_scenario_data_zeros <- function(parameters,
                                 stages, 
                                 name,
                                 recapture,
                                 phi,
                                 repro_stages,
                                 location){
  
#### Create simulated data ####
  
# Simulate the state only

seeds <- as.list(c(1:100))

#### Simulation 1: randomly assigned zeros ####  

# randomly add 0s to the offspring column 30%
# apply it to observations file as need recapture to be < 1

# NEED to run observation process first

baseline2 <- map2(.x = baseline,
                  .y = seeds, ~{run_observation_process(.x,
                                                        p = recapture*missing,
                                                        fecundity_error = FALSE,
                                                        phi = phi,
                                                        seed = .y,
                                                        stages = stages,
                                                        type = "zeros")
                                    })

random_zeros_reproduction <- map(.x = baseline2, ~{
  set.seed(1)
  marker <- sample(1:length(.x$Offspring), round(length(.x$Offspring)*0.30))
  .x <- .x %>% mutate(Offspring_obs = Offspring)
  .x$Offspring_obs[marker] <- 0
  return(.x)
})

# save
save(random_zeros_reproduction, 
     file = paste0(location, length(stages), "random_zeros_simulation", name, ".RData"))

#### Simulation 2: zeros for part of population ####
# miss lowest breeding class

# add 0s to juveniles in the offspring column 30%
juvenile_zeros_reproduction <- map(.x = baseline2, ~{
  marker1 <- which(.x$Stage == repro_stages[1])
  set.seed(1)
  marker2 <- sample(marker1, round(length(marker1)*0.3))
  .x <- .x %>% mutate(Offspring_obs = Offspring)
  .x$Offspring_obs[marker2] <- 0
  return(.x)
})

# save
save(juvenile_zeros_reproduction, 
     file = paste0(location, length(stages), 
                   "juvenile_zeros_simulation", name, ".RData"))

# miss adults
adult_zeros_reproduction <- map(.x = baseline2, ~{
  marker1 <- which(.x$Stage %in% repro_stages[-1])
  set.seed(1)
  marker2 <- sample(marker1, round(length(marker1)*0.3))
  .x <- .x %>% mutate(Offspring_obs = Offspring)
  .x$Offspring_obs[marker2] <- 0
  return(.x)
})


# save
save(adult_zeros_reproduction, 
     file = paste0(location, length(stages), 
                   "adult_zeros_simulation", name, ".RData"))

}