#### T1.1: Script to run nimble model in parallel ####

################################################################################

#### Set up ####



################################################################################

run_model <- function(inputs,
                      niter,
                      nburnin,
                      scenario,
                      mat_num,
                      location){
  
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