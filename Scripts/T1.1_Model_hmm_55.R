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
initial_survival[1] <- 0.2
initial_survival[2] <- 0.2
initial_survival[3] <- 0.2
initial_survival[4] <- 0.2
initial_survival[5] <- 0.2
initial_survival[6] <- 0

# matrix of transitions from juv, to adult, to dead (STATE)
# the probabilities should be the probability of being any stage
# STATES = alive juv 1, alive adult 2-5, dead 3/4/5/6

transition[1, 1] <- 0 # Pr(juv alive t -> juv alive t+1)
transition[1, 2] <- mean_phi[1] # Pr(juv alive t -> sub-adult alive t+1)
transition[1, 3] <- 0 # Pr(juv alive t -> adult1 t+1)
transition[1, 4] <- 0 # Pr(juv alive t -> adult2 t+1)
transition[1, 5] <- 0 # Pr(juv alive t -> adult t+1)
transition[1, 6] <- 1-mean_phi[1]# Pr(juv alive t -> dead t+1)
transition[2, 1] <- 0 # Pr(sub-adult alive t -> juv alive t+1)
transition[2, 2] <- 0 # Pr(sub-adult alive t -> sub-adult alive t+1)
transition[2, 3] <- mean_phi[2] # Pr(sub-adult alive t -> adult1 t+1)
transition[2, 4] <- 0 # Pr(sub-adult alive t -> adult2 t+1)
transition[2, 5] <- 0 # Pr(sub-adult alive t -> adult3 t+1)
transition[2, 6] <- 1 - mean_phi[2] # Pr(sub-adult alive t -> dead t+1)
transition[3, 1] <- 0 # Pr(adult1 t -> juvenile alive t+1)
transition[3, 2] <- 0 # Pr(adult1 t -> sub-adult alive t+1)
transition[3, 3] <- 0 # Pr(adult1 t -> adult1 t+1)
transition[3, 4] <- mean_phi[3] # Pr(adult1 t -> adult2 t+1)
transition[3, 5] <- 0 # Pr(adult1 t -> adult3 t+1)
transition[3, 6] <- 1-mean_phi[3] # Pr(adult1 t -> dead t+1)
transition[4, 1] <- 0 # Pr(adult2 t -> juvenile alive t+1)
transition[4, 2] <- 0 # Pr(adult2 t -> sub-adult alive t+1)
transition[4, 3] <- 0 # Pr(adult2 t -> adult1 t+1)
transition[4, 4] <- 0 # Pr(adult2 t -> adult2 t+1)
transition[4, 5] <- mean_phi[4] # Pr(adult2 t -> adult3 t+1)
transition[4, 6] <- 1-mean_phi[4] # Pr(adult2 t -> dead t+1)
transition[5, 1] <- 0 # Pr(adult3 t -> juvenile alive t+1)
transition[5, 2] <- 0 # Pr(adult3 t -> sub-adult alive t+1)
transition[5, 3] <- 0 # Pr(adult3 t -> adult1 t+1)
transition[5, 4] <- 0 # Pr(adult3 t -> adult2 t+1)
transition[5, 5] <- mean_phi[5] # Pr(adult3 t -> adult3 t+1)
transition[5, 6] <- 1-mean_phi[5] # Pr(adult3 t -> dead t+1)
transition[6, 1] <- 0 # Pr(dead t -> juvenile alive t+1)
transition[6, 2] <- 0 # Pr(dead t -> sub-adult alive t+1)
transition[6, 3] <- 0 # Pr(dead t -> adult1 t+1)
transition[6, 4] <- 0 # Pr(dead t -> adult2 t+1)
transition[6, 5] <- 0 # Pr(dead t -> adult3 t+1)
transition[6, 6] <- 1 # Pr(dead t -> dead t+1)

# observation matrix (captures recapture probability)
# OBS =  alive juv 1, alive subadult 2, adult1 3
# adult2 4 adult3 5 dead 6
# row 1 = alive juv
# row 2 = alive subadult
# row 3 = alive adult1
# row 4 = alive adult2
# row 5 = alive adult 3
# row 6 = dead

# column 1 = observed juv, column 2 = observed sub-adult, 
# column 3 = observed adult1, column 4 = observed adult2,
# column 5 = observed adult3, column 6 = not detected
observations[1, 1] <- mean_p[1] # Pr(juv alive t and detected t)
observations[1, 2] <- 0 # Pr(juv alive t and detected as sub adult t)
observations[1, 3] <- 0 # Pr(juv alive t and detected as adult1 t)
observations[1, 4] <- 0 # Pr(juv alive t but detected as adult2 t)
observations[1, 5] <- 0 # Pr(juv alive t and detected as adult3 t)
observations[1, 6] <- 1 - mean_p[1] # Pr(juv alive t but not detected t)

observations[2, 1] <- 0 # Pr(adult alive t but detected as juv t)
observations[2, 2] <- mean_p[2] # Pr(sub adult alive t and detected t)
observations[2, 3] <- 0 # Pr(sub adult alive t and detected as adult1 t)
observations[2, 4] <- 0 # Pr(sub adult alive t and detected as adult2 t)
observations[2, 5] <- 0 # Pr(sub adult alive t and detected as adult3 t)
observations[2, 6] <- 1 - mean_p[2] # Pr(sub adult alive t and not detected t)

observations[3, 1] <- 0 # Pr(adult1 t and detected as juvenile t)
observations[3, 2] <- 0 # Pr(adult1 t and detected as sub adult t)
observations[3, 3] <- mean_p[3] # Pr(adult1 t and detected as adult1 t)
observations[3, 4] <- 0 # Pr(adult1 t and adult1 t)
observations[3, 5] <- 0 # Pr(adult1 t and detected as adult2 t)
observations[3, 6] <- 1 - mean_p[3] # Pr(adult1 t and not detected t)

observations[4, 1] <- 0 # Pr(adult2 t and detected as juvenile t)
observations[4, 2] <- 0 # Pr(adult2 t and detected as sub adult t)
observations[4, 3] <- 0 # Pr(adult2 t and detected as adult1 t)
observations[4, 4] <- mean_p[4] # Pr(adult2 t and adult1 t)
observations[4, 5] <- 0 # Pr(adult2 t and detected as adult2 t)
observations[4, 6] <- 1 - mean_p[4] # Pr(adult2 t and not detected t)

observations[5, 1] <- 0 # Pr(adult3 t and detected as juvenile t)
observations[5, 2] <- 0 # Pr(adult3 t and detected as sub adult t)
observations[5, 3] <- 0 # Pr(adult3 t and detected as adult1 t)
observations[5, 4] <- 0 # Pr(adult3 t and adult1 t)
observations[5, 5] <- mean_p[5] # Pr(adult3 t and detected as adult2 t)
observations[5, 6] <- 1 - mean_p[5] # Pr(adult3 t and not detected t)

observations[6, 1] <- 0 # Pr(dead t and detected as juvenile t)
observations[6, 2] <- 0 # Pr(dead t and detected as sub adult t)
observations[6, 3] <- 0 # Pr(dead t and detected as adult1 t)
observations[6, 4] <- 0 # Pr(dead t and detected as adult2 t)
observations[6, 5] <- 0 # Pr(dead t and detected as adult3 t)
observations[6, 6] <- 1 # Pr(dead t and not detected t)

## likelihood survival

# need to iterate over each individual and time
# first go over time for individual i using categorical distribution
# need to identify the first entry in the state where the individual is alive
# THEN apply the likelihood
for (i in 1:N){
  init[i, 1:6] <- initial_survival[1:6]}
for (i in 1:N){
  # distribution for first observations
 surv_obs[i,first[i]:occasions] ~ dHMM(init = init[i, 1:6], 
                  probObs = observations[1:6,1:6], # observation matrix
                  probTrans = transition[1:6,1:6], # transition matrix
                  len = occasions-(first[i]-1), # nb of sampling occasions
                  checkRowSums = 0)
}

#-------------------------------------------------------------------------------
## DEFINE PRIORS FECUNDITY 
## these are outside loop as constant for all breeding attempts

# vague priors
beta_age[1] ~ dnorm(0, sd = 1.5) 
beta_age[2] ~ dnorm(0, sd = 1.5) 
beta_age[3] ~ dnorm(0, sd = 1.5)
beta_age[4] ~ dnorm(0, sd = 1.5)

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

for(j in 1:O_N_a1){
  
  # observed offspring
  #offspring_obs[f] ~ dpois(offspring_state[f])
  
  # process for offspring
  offspring_obs_a1[j] ~ dpois(fecundity_rate_a1[j])
  log(fecundity_rate_a1[j]) <- log_fecundity_rate_a1[j]
  # need to allow different effect for each age
  log_fecundity_rate_a1[j] <- beta_age[2]
}

for(k in 1:O_N_a2){
  
  # observed offspring
  #offspring_obs[f] ~ dpois(offspring_state[f])
  
  # process for offspring
  offspring_obs_a2[k] ~ dpois(fecundity_rate_a2[k])
  log(fecundity_rate_a2[k]) <- log_fecundity_rate_a2[k]
  # need to allow different effect for each age
  log_fecundity_rate_a2[k] <- beta_age[3]
}

for(l in 1:O_N_a3){
  
  # observed offspring
  #offspring_obs[f] ~ dpois(offspring_state[f])
  
  # process for offspring
  offspring_obs_a3[l] ~ dpois(fecundity_rate_a3[l])
  log(fecundity_rate_a3[l]) <- log_fecundity_rate_a3[l]
  # need to allow different effect for each age
  log_fecundity_rate_a3[l] <- beta_age[4]
}

#-------------------------------------------------------------------------------
## Set up MPM

mean_fecundity_juv <- 0 # fecundity for juveniles
mean_fecundity_subadult <- exp(beta_age[1]) # fecundity for sub-adults 
mean_fecundity_adult1 <- exp(beta_age[2]) # fecundity for adults 
mean_fecundity_adult2 <- exp(beta_age[3]) # fecundity for adults2 
mean_fecundity_adult3 <- exp(beta_age[4]) # fecundity for adults3 

transition_matrix[1,1] <- mean_fecundity_juv
transition_matrix[1,2] <- mean_fecundity_subadult # fecundity for adults  
transition_matrix[1,3] <- mean_fecundity_adult1 # fecundity for adults  
transition_matrix[1,4] <- mean_fecundity_adult2 # fecundity for adults  
transition_matrix[1,5] <- mean_fecundity_adult3 # fecundity for adults  

transition_matrix[2,1] <- mean_phi[1] # juvenile survival
transition_matrix[2,2] <- 0 # sub-adult-sub-adult survival
transition_matrix[2,3] <- 0 # sub-adult-adult1 survival
transition_matrix[2,4] <- 0 # sub-adult-adult2 survival
transition_matrix[2,5] <- 0 # sub-adult-adult3 survival

transition_matrix[3,1] <- 0 # juvenile-adult survival
transition_matrix[3,2] <- mean_phi[2] # sub-adult survival
transition_matrix[3,3] <- 0 # adult1 survival
transition_matrix[3,4] <- 0 # adult2 survival
transition_matrix[3,5] <- 0 # adult3 survival

transition_matrix[4,1] <- 0 # juvenile survival
transition_matrix[4,2] <- 0 # sub-adult-sub-adult survival
transition_matrix[4,3] <- mean_phi[3] # sub-adult-adult1 survival
transition_matrix[4,4] <- 0 # sub-adult-adult2 survival
transition_matrix[4,5] <- 0 # sub-adult-adult3 survival

transition_matrix[5,1] <- 0 # juvenile survival
transition_matrix[5,2] <- 0 # sub-adult-sub-adult survival
transition_matrix[5,3] <- 0 # sub-adult-adult1 survival
transition_matrix[5,4] <- mean_phi[4] # sub-adult-adult2 survival
transition_matrix[5,5] <- mean_phi[5] # sub-adult-adult3 survival

#-------------------------------------------------------------------------------
## Derived quantities MPM

# lambda
lambda <- nimEigen(transition_matrix[1:5,1:5])$values[1]

## Final parameters
reproduction_juvenile <- mean_fecundity_juv
reproduction_subadult <- mean_fecundity_subadult # fecundity for adult
reproduction_adult1 <- mean_fecundity_adult1 # fecundity for adults 
reproduction_adult2 <- mean_fecundity_adult2 # fecundity for adults 
reproduction_adult3 <- mean_fecundity_adult3 # fecundity for adults 

recapture_juvenile <- mean_p[1]
recapture_subadult <- mean_p[2]
recapture_adult1 <- mean_p[3]
recapture_adult2 <- mean_p[4]
recapture_adult3 <- mean_p[5]

survival_juvenile <- mean_phi[1]
survival_subadult <- mean_phi[2]
survival_adult1 <- mean_phi[3]
survival_adult2 <- mean_phi[4]
survival_adult3 <- mean_phi[5]

})

