# FUNCTION to create all scenario data for missing not at random #

## Function cleans previous output data to become new input data

## INPUT :
#
# - a baseline to alter
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

create_scenario_data_not_random <- function(baseline, 
                                            parameters,
                                            stages, 
                                            name,
                                            recapture,
                                            phi,
                                            missing,
                                            repo_stages,
                                            location){
  
#### Create simulated data ####

#### Simulation 3: missing not at random across whole dataset ####
  
# randomly remove 60% individuals from 50% of population (higher breeders)
# apply it to state file

not_random_missing_reproduction <- map(.x = baseline,
                                        ~{
                                        # read in baseline 
                                        baseline <- readRDS(.x)  
                                        run_observation_process(baseline,
                                        p = recapture*missing,
                                        fecundity_error = FALSE,
                                        phi = phi,
                                        seed = 1, # just want a single realisation per baseline
                                        stages = stages,
                                        repo_stages = repo_stages,
                                        random = FALSE)
                                    })

# save
save(not_random_missing_reproduction, 
     file = paste0(location, length(stages), "not_random_missing_simulation",
                   name, ".RData"))

}