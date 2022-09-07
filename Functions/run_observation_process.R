# FUNCTION to run the observation process #

################################################################################

library(tidyverse)

## Function applies recapture probabilities and count error to simulated states
## produces a datafile of observed data

## INPUT :
#
# - input_data =a dataframe with column names:
# ID (factor), Year (factor), Surv (0/1), Offspring (num), 
# Age (num), Trait (num)
#
# - p = recapture probability
# 
# - fecundity error = TRUE/FALSE, is there error in fecundity data?
#
# - juv_recapture = TRUE/FALSE, is capture of juveniles 100% or not


## OUTPUT = dataframe of observed data

#### FUNCTION ####


run_observation_process <- function(state_data, 
                                    p, 
                                    fecundity_error = TRUE,
                                    juv_recapture = TRUE){
# fill in recapture probabilities
# as survival is "did they survive until next year?" it is sort of lagged
# recapture wants to refer to the focal year so cannot use survival to define it
# instead will just same from rbinom
if(juv_recapture == TRUE){
  observed_data <- state_data %>% 
    mutate(Recapture = case_when(Age == 1 ~ 1,
                                 Age > 1 ~ rbinom(1, 1, p),
                                 TRUE ~ 1))}
if(juv_recapture == FALSE){
  observed_data <- state_data %>% 
    mutate(Recapture = rbinom(length(state_data$Age), 1, p))}

# add observation error to fecundity - poisson error (count error)
if(fecundity_error == TRUE){
  observed_data <- observed_data %>% 
    mutate(Offspring_obs = rpois(length(Offspring),
                                 Offspring))
}
  
# clean up the data to remove individuals not recaptured
observed_data <- filter(observed_data, Recapture == 1)

return(observed_data)  

}
