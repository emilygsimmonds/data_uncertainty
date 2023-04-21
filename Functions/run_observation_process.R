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


## OUTPUT = dataframe of observed data

#### FUNCTION ####


run_observation_process <- function(state_data, 
                                    p, phi,
                                    fecundity_error = TRUE,
                                    stages,
                                    seed = 1){
  
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
if(length(stages)==3){
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
if(length(stages)==5){
  # not captured year 1 or 2 but captured year 3
  year3 <- prod(dbinom(0, 1, p[1]),
                dbinom(0, 1, (p[2]*phi[1])),
                dbinom(1, 1, (p[3]*phi[2])))
  # not captured year 1,2,3 but captured year 4
  year4 <- prod(dbinom(0, 1, p[1]),
                dbinom(0, 1, (p[2]*phi[1])),
                dbinom(0, 1, (p[3]*phi[2])),
                dbinom(1, 1, (p[4]*phi[3])))  
  # not captured year 1,2,3 but captured year 5
  year5 <- prod(dbinom(0, 1, p[1]),
                dbinom(0, 1, (p[2]*phi[1])),
                dbinom(0, 1, (p[3]*phi[2])),
                dbinom(0, 1, (p[4]*phi[3])),
                sum(dbinom(1, 1, (p[4]*phi[4])))) 
  year3 <- sum(year3, year4, year5)
}


total_prob <- sum(year1, year2, year3)   

# combine the recapture probabilities into a vector
recapture <- p
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
