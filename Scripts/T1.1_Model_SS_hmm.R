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
mean_p[j] ~ dunif(0, 1)

## parameters

# vector of initial state probabilities (juv surv, adult stages, death)
initial_survival[j] <- dunif(0, 1)

}

# reset 'dead' to 0
initial_survival[stage_length] <- 0

for(j in 1:(stage_length-1)){
# matrix of transitions from juv, to adult, to dead (STATE)
# the probabilities should be the probability of being any stage
# STATES = alive juv 1, alive adult 2-5, dead 3/4/5/6

transition[j, j] <- 0 # Pr(juv alive t -> juv alive t+1)
transition[1, 2] <- mean_phi_juv # Pr(juv alive t -> adult alive t+1)
transition[1, 3] <- 1-mean_phi_juv # Pr(juv alive t -> dead t+1)
transition[2, 1] <- 0 # Pr(adult alive t -> juv alive t+1)
transition[2, 2] <- mean_phi_adult # Pr(adult alive t -> adult alive t+1)
transition[2, 3] <- 1 - mean_phi_adult # Pr(adult alive t -> dead t+1)
transition[3, 1] <- 0 # Pr(dead t -> juvenile alive t+1)
transition[3, 2] <- 0 # Pr(dead t -> adult alive t+1)
transition[3, 3] <- 1 # Pr(dead t -> dead t+1)

}

# observation matrix (captures recapture probability)
# OBS =  alive juv 1, alive adult 2, not detected 3
# row 1 = alive juv
# row 2 = alive adult
# row 3 = dead

# column 1 = observed juv, column 2 = observed adult, column 3 not detected
observations[1, 1] <- mean_p_juv # Pr(juv alive t and detected t)
observations[1, 2] <- 0 # Pr(juv alive t and detected as adult t)
observations[1, 3] <- 1 - mean_p_juv # Pr(juv alive t but not detected t)
observations[2, 1] <- 0 # Pr(adult alive t but detected as juv t)
observations[2, 2] <- mean_p_adult # Pr(adult alive t and detected t)
observations[2, 3] <- 1 - mean_p_adult # Pr(adult alive t and not detected t)
observations[3, 1] <- 0 # Pr(dead t and detected as juvenile t)
observations[3, 2] <- 0 # Pr(dead t and detected as adult t)
observations[3, 3] <- 1 # Pr(dead t and not detected t)

## likelihood survival

# need to iterate over each individual and time
# first go over time for individual i using categorical distribution
# need to identify the first entry in the state where the individual is alive
# THEN apply the likelihood
for (i in 1:N){
  init[i, 1:3] <- initial_survival[1:3]}
for (i in 1:N){
  # distribution for first observations
 surv_obs[i,first[i]:occasions] ~ dHMM(init = init[i, 1:3], 
                  probObs = observations[1:3,1:3], # observation matrix
                  probTrans = transition[1:3,1:3], # transition matrix
                  len = occasions-(first[i]-1), # nb of sampling occasions
                  checkRowSums = 0)
}

#-------------------------------------------------------------------------------
## DEFINE PRIORS FECUNDITY 
## these are outside loop as constant for all breeding attempts

# vague priors
alpha ~ dnorm(0, sd = 1.5)  
beta_age ~ dnorm(0, sd = 1.5)  

## LIKELIHOOD FECUNDITY IN LOOP

for(f in 1:O_N){
  
  # observed offspring
  #offspring_obs[f] ~ dpois(offspring_state[f])
  
  # process for offspring
  offspring_obs[f] ~ dpois(fecundity_rate[f])
  log(fecundity_rate[f]) <- log_fecundity_rate[f]
  log_fecundity_rate[f] <- alpha + beta_age*(age[f]-1)
  
}

#-------------------------------------------------------------------------------
## Set up MPM

mean_fecundity_juv <- exp(alpha) # fecundity for juveniles
mean_fecundity_adult <- exp(alpha + beta_age) # fecundity for adults 

transition_matrix[1,1] <- mean_fecundity_juv
transition_matrix[1,2] <- mean_fecundity_adult # fecundity for adults                           
transition_matrix[2,1] <- mean_phi_juv # juvenile survival
transition_matrix[2,2] <- mean_phi_adult # adult survival

#-------------------------------------------------------------------------------
## Derived quantities MPM

# lambda
lambda <- nimEigen(transition_matrix[1:2,1:2])$values[1]

# stable size distribution
size_distribution[1:2] <- eigen(transition_matrix[1:2,1:2])$vectors[,1]

})

