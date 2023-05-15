# FUNCTION to run nimble model in parallel #

################################################################################


################################################################################

run_model <- function(filename,
                      niter,
                      nburnin,
                      scenario,
                      mat_num,
                      location,
                      num_stages = 2,
                      fecundity_error){
  
  simulations <- readRDS(filename)
  
  stages <- c("juvenile", "adult")
  
  if(num_stages == 3){stages <- c("juvenile", "subadult", "adult")}
  if(num_stages == 5){stages <- c("juvenile", "subadult", 
                                  "adult1", "adult2", "adult3")}
  
  # source model
  source("./Functions/make_input_data_function.R")
  # take all of the simulated datasets and edit into model inputs
  inputs <- make_input_data(simulations, 
        n_occasions = 5,
        fecundity_error = fecundity_error,
        stages = stages)
  
  rm(simulations)
  
  library(nimble)
  library(nimbleEcology)
  
  # source model
  source("./Scripts/T1.1_Model_hmm_22.R")
  
  model_result <- nimbleMCMC(code = Model_SS_hmm, 
                             data = inputs$data_input,
                             constants = inputs$constants,
                             inits = inputs$inits,
                             monitors = inputs$parameters_to_save,
                             niter = niter,
                             nburnin = nburnin,
                             nchains = 2)
  
  # save out result at each pass
  save(model_result, 
       file = paste0(location, 
                     scenario, mat_num, ".RData"))
}