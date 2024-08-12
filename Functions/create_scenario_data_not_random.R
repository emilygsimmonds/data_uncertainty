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
# - split = proportion of population in 'high capture' group
# - offset = how much 'high capture' will differ from baseline 
# - bias = either 'high' (biased to more high breeders so missing low) or 
# 'low' (biased to low breeders so missing high)

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
                                            location,
                                            split, 
                                            offset, bias){
  
#### Create simulated data ####

#### Simulation 3: missing not at random ####

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
                                        random = FALSE, split, offset,
                                        bias)
                                    })

# save
save(not_random_missing_reproduction, 
     file = paste0(location, length(stages), "not_random_missing_simulation_",
                  bias, name, ".RData"))

}

