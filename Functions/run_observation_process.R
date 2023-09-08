# FUNCTION to run the observation process #

################################################################################

library(tidyverse)

## Function applies recapture probabilities and count error to simulated states
## produces a datafile of observed data

## INPUT :
#
# - input_data =a dataframe with column names:
# ID (factor), Year (factor), Surv (0/1), Offspring (num), 
# Stage (num), Trait (num)
#
# - p = vector of stage recapture probability
# 
# - fecundity error = TRUE/FALSE, is there error in fecundity data?
#
# - phi = vector of survival probabilities by stage
#
# - stages = vector of stages
#
# - type = c("missing", "zero") indicates type of data errors
#
# - random = TRUE/FALSE, if the process is at random or not


## OUTPUT = dataframe of observed data

#### FUNCTION ####


run_observation_process <- function(state_data, 
                                    p, phi,
                                    fecundity_error = TRUE,
                                    stages,
                                    repo_stages = c("juvenile", "adult"),
                                    seed = 1,
                                    random = TRUE){
  
## Calculate probability of EVER being recaptured alive in study
  
# captured alive year 1
year1 <- dbinom(1, 1, p[1]) # probability of initial capture at 1 year

# not captured year 1 but captured year 2
year2 <- prod(dbinom(0, 1, p[1]),
              dbinom(1, 1, (p[2]*phi[1]))) 
# probability of surviving to year 2 and capture

# not captured year 1 or 2 but captured year 3-5
if(length(stages)==2){
  year3 <- prod(dbinom(0, 1, p[1]),
              dbinom(0, 1, (p[2]*phi[1])),
              sum(dbinom(1:3, 3, (p[2]*phi[2]))))
# probability of surviving beyond year 2 and capture
}

# not captured year 1 or 2 but captured year 3-5
if(length(stages)>2){
  # not captured year 1 or 2 but captured year 3
  year3 <- prod(dbinom(0, 1, p[1]),
                dbinom(0, 1, (p[2]*phi[1])),
                dbinom(1, 1, (p[3]*phi[2])))
  # not captured year 1,2,3 but captured year 4 or 5
  year4 <- prod(dbinom(0, 1, p[1]),
                dbinom(0, 1, (p[2]*phi[1])),
                dbinom(0, 1, (p[3]*phi[2])),
                sum(dbinom(1:2, 3, (p[3]*phi[3]))))  
  year3 <- sum(year3, year4)
}

total_prob <- sum(year1, year2, year3)   

# combine the recapture probabilities into a vector
recapture <- p # half the recapture rate as only applied to half population
names(recapture) <- stages
  
# fill in recapture probabilities
# as survival is "did they survive until next year?" it is sort of lagged
# recapture wants to refer to the focal year so cannot use survival to define it
# instead will just same from rbinom

# index the probability based on age
set.seed(seed)
observed_data <- state_data %>% 
    mutate(Recapture = rbinom(length(state_data$Stage), 1, 
                              recapture[state_data$Stage]))

if(random == FALSE){ 
  recapture <- p/2 # half the recapture rate as only applied to half population
  names(recapture) <- stages
  # then want to select just part of population to be missing
  # higher probability of selecting top 50% of breeding population based on offspring number
  # and apply half recapture rate of whole pop (so same tot pop % as other scenario)
  
  # need to make this apply only to the breeding population
  if(length(stages)>2){
  # get markers of all reproducing individuals
    repo_marker <- which(state_data$Stage %in% repo_stages)
    marker <- sample(repo_marker, 
                     length(repo_marker)/(2*(length(repo_stages)
                                               /length(stages))), 
                     # scaling 2 number by proportion of stages that are reproductive
                     replace = FALSE,
                     # scaling by max gives some probs of 0 - don't want this
                     # add 1 to all offspring numbers when doing calc
                     prob = ((state_data$Offspring[repo_marker]+1)/
                               (max(state_data$Offspring[repo_marker]+1)-0.001)))  
  }
  
  if(length(stages)==2){
  marker <- sample(1:length(state_data$Offspring), 
                   length(state_data$Offspring)/2,
                   replace = FALSE,
                   # scaling by max gives some probs of 0 - don't want this
                   # add 1 to all offspring numbers when doing calc
                   prob = ((state_data$Offspring+1)/
                             (max(state_data$Offspring+1)-0.001))) 
  }
  # scaled by max value and subtract 0.001 to make them all between 0 and 1
  observed_data <- state_data %>% 
    mutate(Recapture = rbinom(length(state_data$Stage), 1, 
                              recapture[state_data$Stage]))
  # then overwrite those not 'chosen' by marker to have been recaptured
  # higher probability of choosing those with higher breeding success
  observed_data$Recapture[marker] <- 1
}

# scale fecundity by chance of ever being observed
# binomial probability
set.seed(seed)
observed_data$Offspring <- rbinom(n = length(observed_data$Offspring),
                                  size = observed_data$Offspring,
                                  prob = total_prob)

# add observation error to fecundity - poisson error (count error)
if(fecundity_error == TRUE){
  set.seed(seed)
  observed_data <- observed_data %>% 
    mutate(Offspring_obs = rpois(length(Offspring),
                                 Offspring))
}
  
# clean up the data to remove individuals not recaptured - for missing scenarios
# but leave in for random zeros scenario - will filter survival at stage of adding
# data to model
observed_data <- filter(observed_data, Recapture == 1)

return(observed_data)  

}
