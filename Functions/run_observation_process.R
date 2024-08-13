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
# - split = proportion of population in 'high capture' group
# - offset = how much 'high capture' will differ from baseline 
# - bias = either 'high' (biased to more high breeders so missing low) or 
# 'low' (biased to low breeders so missing high)

## OUTPUT = dataframe of observed data

#### FUNCTION ####


run_observation_process <- function(state_data, 
                                    p, phi,
                                    fecundity_error = TRUE,
                                    stages,
                                    repo_stages = c("juvenile", "adult"),
                                    seed = 1,
                                    random = TRUE,
                                    split = NA, 
                                    offset = NA, 
                                    bias = "high",
                                    do_checks = FALSE){
  
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
recapture <- p
names(recapture) <- stages
  
# fill in recapture probabilities
# as survival is "did they survive until next year?" it is sort of lagged
# recapture wants to refer to the focal year so cannot use survival to define it
# instead will just same from rbinom

# index the probability based on age
set.seed(seed)
if(random == TRUE){observed_data <- state_data %>% 
    mutate(Recapture = rbinom(length(state_data$Stage), 1, 
                              recapture[state_data$Stage]))
}

if(random == FALSE){ 
  
  if(length(stages)==2){
  
  # calculate the recapture rates for the two groups
  
  recapture_higher <- p+offset # split does not matter for the higher group
  
  # then calculate higher recapture rate at population level
  relative_recapture <- recapture_higher * split 
  # then calculate the recapture for lower group so that at pop level it = p
  # do this by scaling by the proportion of population in the lower group
  recapture_lower <- (p - relative_recapture) / (1-split)
  
  names(recapture_lower) <- stages
  names(recapture_higher) <- stages
  # then want to select just part of population to be missing
  # higher probability observing top breeders based on offspring number
  # split into two equally sized groups of 'lower recapture' and 
  # 'higher recapture' probability of being in either group determined by 
  # offspring number
    
  # then get markers for those in 'higher recapture' group
  set.seed(seed) # place seed before each random step
  group_higher_marker <- sample(1:length(state_data$Offspring), 
                   length(state_data$Offspring)*split,
                   replace = FALSE,
                   # scaling by max gives some probs of 0 - don't want this
                   # add 1 to all offspring numbers when doing calc
                   # also add 0.001 to denominator so that all probs >0<1
                   if(bias == "high"){prob = ((state_data$Offspring+1)/
                   (max(state_data$Offspring+1)+0.001))}else{
                   # take opposite of probabilities
                   prob = 1-((state_data$Offspring+1)/
                   (max(state_data$Offspring+1)+0.001))  
                   })    
  # apply the lower recapture rate to the whole population
  set.seed(seed) # place seed before each random step
  observed_data <- state_data %>% 
    mutate(Recapture = rbinom(length(state_data$Stage), 1, 
                              recapture_lower[state_data$Stage]))
  # PRINTING OF RECAPTURE RATE TO CHECK FUNCTIONING  
  if(do_checks == TRUE){print(sum(observed_data$Recapture)/
                                length(observed_data$Recapture))}
  # then overwrite recapture for 'higher' group with higher rate
  set.seed(seed) # place seed before each random step
  observed_data$Recapture[group_higher_marker] <- 
    rbinom(length(observed_data$Stage[group_higher_marker]), 1, 
           recapture_higher[observed_data$Stage])
  # PRINTING OF RECAPTURE RATE TO CHECK FUNCTIONING  
  if(do_checks == TRUE){print(sum(observed_data$Recapture)/
                                length(observed_data$Recapture))
    print(sum(observed_data$Recapture[group_higher_marker])/
            length(observed_data$Recapture[group_higher_marker]))}
  }

# need to make this apply only to the breeding population
if(length(stages)>2){
  
  # calculate the recapture rates for the groups
  
  recapture_higher <- p+offset # split does not matter for the higher group
  
  # then calculate higher recapture rate at population level
  relative_recapture <- recapture_higher * split 
  # then calculate the recapture for lower group so that at pop level it = p
  # do this by scaling by the proportion of population in the lower group
  recapture_lower <- (p - relative_recapture) / (1-split)
  
  names(recapture_lower) <- stages
  names(recapture_higher) <- stages
  # then want to select just part of population to be missing
  # higher probability observing top breeders based on offspring number
  # split into two equally sized groups of 'lower recapture' and 
  # 'higher recapture' probability of being in either group determined by 
  # offspring number
  
  # get markers of all reproducing individuals
  repo_marker <- which(state_data$Stage %in% repo_stages)
  # then get markers for those in 'higher recapture' group
  # do this by sampling IDs with a probability related to reproductive output
  # number of IDs selected is controlled by the 'split'  
  set.seed(seed) # place seed before each random step
  group_higher_marker <- sample(repo_marker, 
         (length(repo_marker)*split), 
         # take the split of those that reproduce
         replace = FALSE,
         # scaling by max gives some probs of 0 - don't want this
         # add 1 to all offspring numbers when doing calc
         # also add 0.001 to denominator so that all probs >0<1
         if(bias == "high"){prob = ((state_data$Offspring[repo_marker]+1)/
         (max(state_data$Offspring[repo_marker]+1)+0.001))}else{
           # take opposite of probabilities
           prob = 1-((state_data$Offspring[repo_marker]+1)/
                       (max(state_data$Offspring[repo_marker]+1)+0.001))  
         })
  # apply the pop recapture rate to the whole population 
  set.seed(seed) # place seed before each random step
  observed_data <- state_data %>% 
    mutate(Recapture = rbinom(length(state_data$Stage), 1, 
                              recapture[state_data$Stage]))
  # PRINTING OF RECAPTURE RATE TO CHECK FUNCTIONING  
  if(do_checks == TRUE){print(sum(observed_data$Recapture)/
                                length(observed_data$Recapture))}
  # Then apply low recapture to BREEDING STAGES
  observed_data$Recapture[repo_marker] <- 
    rbinom(length(observed_data$Recapture[repo_marker]),1,
           recapture_lower[stages %in% repo_stages])
  # PRINTING OF RECAPTURE RATE TO CHECK FUNCTIONING  
  if(do_checks == TRUE){print(sum(observed_data$Recapture)/
                                length(observed_data$Recapture))}
  # then overwrite recapture for 'higher' group with higher rate
  set.seed(seed) # place seed before each random step
  observed_data$Recapture[group_higher_marker] <- 
    rbinom(length(observed_data$Stage[group_higher_marker]), 1, 
           recapture_higher[observed_data$Stage])
  # PRINTING OF RECAPTURE RATE TO CHECK FUNCTIONING  
  if(do_checks == TRUE){print(sum(observed_data$Recapture)/
                                length(observed_data$Recapture))
    print(sum(observed_data$Recapture[group_higher_marker])/
            length(observed_data$Recapture[group_higher_marker]))}
}
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
