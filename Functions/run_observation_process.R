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
# - p_adult = adult recapture probability
# 
# - fecundity error = TRUE/FALSE, is there error in fecundity data?
#
# - p_juvenile = probability of capture for juveniles


## OUTPUT = dataframe of observed data

#### FUNCTION ####


run_observation_process <- function(state_data, 
                                    p_adult, 
                                    fecundity_error = TRUE,
                                    p_juvenile,
                                    phi_adult,
                                    phi_juvenile,
                                    seed = 1){
  
## Calculate probability of EVER being recaptured alive
  
# captured alive year 1
year1 <- dbinom(1, 1, p_juvenile) # probability of initial capture at 1 year

# not captured year 1 but captured year 2
year2 <- prod(dbinom(0, 1, p_juvenile),
              dbinom(1, 1, (p_adult*phi_juvenile))) 
# probability of surviving to year 2 and capture

# not captured year 1 or 2 but captured year 3-5
year3 <- prod(dbinom(0, 1, p_juvenile),
              dbinom(0, 1, (p_adult*phi_juvenile)),
              sum(dbinom(1:3, 3, (p_adult*phi_adult)))) # probability of surviving beyond year 2 and capture

total_prob <- sum(year1, year2, year3)   

# combine the recapture probabilities into a vector
recapture <- c(p_juvenile, rep(p_adult, max(state_data$Age)))
  
# fill in recapture probabilities
# as survival is "did they survive until next year?" it is sort of lagged
# recapture wants to refer to the focal year so cannot use survival to define it
# instead will just same from rbinom

# index the probability based on age
set.seed(seed)
observed_data <- state_data %>% 
    mutate(Recapture = rbinom(length(state_data$Age), 1, 
                              recapture[state_data$Age]))

# scale fecundity by chance of ever being observed
# binomial probability
set.seed(seed)
observed_data$Offspring <- rbinom(n=length(observed_data$Offspring),
                                  size = observed_data$Offspring,
                                 prob = total_prob)

# add observation error to fecundity - poisson error (count error)
if(fecundity_error == TRUE){
  set.seed(seed)
  observed_data <- observed_data %>% 
    mutate(Offspring_obs = rpois(length(Offspring),
                                 Offspring))
}
  
# clean up the data to remove individuals not recaptured
observed_data <- filter(observed_data, Recapture == 1)

return(observed_data)  

}
