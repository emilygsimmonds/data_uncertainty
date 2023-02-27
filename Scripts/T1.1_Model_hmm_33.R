#### T1.1: Model script: state-space model with hMM coding ####

################################################################################

#### Set up ####

# load packages

library(tidyverse)
library(nimble)
library(MCMCvis)

################################################################################

#### Model ####

Model_SS_hmm <- nimbleCode({
  
#-------------------------------------------------------------------------------
## DEFINE PRIORS SURVIVAL
# outside of loop as constant
# but within a stage-size loop

for(j in 1:stage_length){
mean_phi[j] ~ dunif(0, 1)
mean_p[j] ~ dunif(0, 1)}

## parameters

# set initial survivals
initial_survival[1] <- 0.25
initial_survival[2] <- 0.25
initial_survival[3] <- 0.5
initial_survival[4] <- 0

# matrix of transitions from juv, to adult, to dead (STATE)
# the probabilities should be the probability of being any stage
# STATES = alive juv 1, alive adult 2-5, dead 3/4/5/6

transition[1, 1] <- 0 # Pr(juv alive t -> juv alive t+1)
transition[1, 2] <- mean_phi[1] # Pr(juv alive t -> sub-adult alive t+1)
transition[1, 3] <- 0 # Pr(juv alive t -> adult t+1)
transition[1, 4] <- 1-mean_phi[1]# Pr(juv alive t -> dead t+1)
transition[2, 1] <- 0 # Pr(sub-adult alive t -> juv alive t+1)
transition[2, 2] <- 0 # Pr(sub-adult alive t -> sub-adult alive t+1)
transition[2, 3] <- mean_phi[2] # Pr(sub-adult alive t -> adult t+1)
transition[2, 4] <- 1 - mean_phi[2] # Pr(sub-adult alive t -> dead t+1)
transition[3, 1] <- 0 # Pr(adult t -> juvenile alive t+1)
transition[3, 2] <- 0 # Pr(adult t -> sub-adult alive t+1)
transition[3, 3] <- mean_phi[3] # Pr(adult t -> adult t+1)
transition[3, 4] <- 1-mean_phi[3] # Pr(adult t -> dead t+1)
transition[4, 1] <- 0 # Pr(dead t -> juvenile alive t+1)
transition[4, 2] <- 0 # Pr(dead t -> sub-adult alive t+1)
transition[4, 3] <- 0 # Pr(dead t -> adult t+1)
transition[4, 4] <- 1 # Pr(dead t -> dead t+1)

# observation matrix (captures recapture probability)
# OBS =  alive juv 1, alive adult 2, not detected 3
# row 1 = alive juv
# row 2 = alive adult
# row 3 = dead

# column 1 = observed juv, column 2 = observed sub-adult, 
# column 3 = observed adult, column 4 = not detected
observations[1, 1] <- mean_p[1] # Pr(juv alive t and detected t)
observations[1, 2] <- 0 # Pr(juv alive t and detected as sub adult t)
observations[1, 3] <- 0 # Pr(juv alive t and detected as adult t)
observations[1, 4] <- 1 - mean_p[1] # Pr(juv alive t but not detected t)
observations[2, 1] <- 0 # Pr(adult alive t but detected as juv t)
observations[2, 2] <- mean_p[2] # Pr(sub adult alive t and detected t)
observations[2, 3] <- 0 # Pr(sub adult alive t and detected as adult t)
observations[2, 4] <- 1 - mean_p[2] # Pr(sub adult alive t and not detected t)
observations[3, 1] <- 0 # Pr(adult t and detected as juvenile t)
observations[3, 2] <- 0 # Pr(adult t and detected as sub adult t)
observations[3, 3] <- mean_p[3] # Pr(adult t and detected as adult t)
observations[3, 4] <- 1 - mean_p[3] # Pr(adult t and not detected t)
observations[4, 1] <- 0 # Pr(dead t and detected as juvenile t)
observations[4, 2] <- 0 # Pr(dead t and detected as sub adult t)
observations[4, 3] <- 0 # Pr(dead t and detected as adult t)
observations[4, 4] <- 1 # Pr(dead t and not detected t)

## likelihood survival

# need to iterate over each individual and time
# first go over time for individual i using categorical distribution
# need to identify the first entry in the state where the individual is alive
# THEN apply the likelihood
for (i in 1:N){
  init[i, 1:4] <- initial_survival[1:4]}
for (i in 1:N){
  # distribution for first observations
 surv_obs[i,first[i]:occasions] ~ dHMM(init = init[i, 1:4], 
                  probObs = observations[1:4,1:4], # observation matrix
                  probTrans = transition[1:4,1:4], # transition matrix
                  len = occasions-(first[i]-1), # nb of sampling occasions
                  checkRowSums = 0)
}

#-------------------------------------------------------------------------------
## DEFINE PRIORS FECUNDITY 
## these are outside loop as constant for all breeding attempts

# vague priors
beta_age[1] ~ dnorm(0, sd = 1.5) 
beta_age[2] ~ dnorm(0, sd = 1.5) 

## LIKELIHOOD FECUNDITY IN LOOP

# split fecundity into two

for(f in 1:O_N_sa){
  
  # observed offspring
  #offspring_obs[f] ~ dpois(offspring_state[f])
  
  # process for offspring
  offspring_obs_sa[f] ~ dpois(fecundity_rate_sa[f])
  log(fecundity_rate_sa[f]) <- log_fecundity_rate_sa[f]
  # need to allow different effect for each age
  log_fecundity_rate_sa[f] <- beta_age[1]
}

for(j in 1:O_N_a){
  
  # observed offspring
  #offspring_obs[f] ~ dpois(offspring_state[f])
  
  # process for offspring
  offspring_obs_a[j] ~ dpois(fecundity_rate_a[j])
  log(fecundity_rate_a[j]) <- log_fecundity_rate_a[j]
  # need to allow different effect for each age
  log_fecundity_rate_a[j] <- beta_age[2]
}

#-------------------------------------------------------------------------------
## Set up MPM

mean_fecundity_juv <- 0 # fecundity for juveniles
mean_fecundity_subadult <- exp(beta_age[1]) # fecundity for sub-adults 
mean_fecundity_adult <- exp(beta_age[2]) # fecundity for adults 

transition_matrix[1,1] <- mean_fecundity_juv
transition_matrix[1,2] <- mean_fecundity_subadult # fecundity for adults  
transition_matrix[1,3] <- mean_fecundity_adult # fecundity for adults  
transition_matrix[2,1] <- mean_phi[1] # juvenile survival
transition_matrix[2,2] <- 0 # sub-adult-sub-adult survival
transition_matrix[2,3] <- 0 # sub-adult-adult survival
transition_matrix[3,1] <- 0 # juvenile-adult survival
transition_matrix[3,2] <- mean_phi[2] # sub-adult survival
transition_matrix[3,3] <- mean_phi[3] # adult survival

#-------------------------------------------------------------------------------
## Derived quantities MPM

# lambda
lambda <- nimEigen(transition_matrix[1:3,1:3])$values[1]

## Final parameters
reproduction_juvenile <- mean_fecundity_juv
reproduction_subadult <- mean_fecundity_subadult # fecundity for adult
reproduction_adult <- mean_fecundity_adult # fecundity for adults 

recapture_juvenile <- mean_p[1]
recapture_subadult <- mean_p[2]
recapture_adult <- mean_p[3]

survival_juvenile <- mean_phi[1]
survival_subadult <- mean_phi[2]
survival_adult <- mean_phi[3]

})

