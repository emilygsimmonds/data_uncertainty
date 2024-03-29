#### T1.1: Model script: state-space model ####

################################################################################

#### FUNCTION TO SET UP THE NIMBLE MODEL

################################################################################

#### Set up ####

# load packages

library(tidyverse)
library(nimble)
library(MCMCvis)

################################################################################

#### Model ####

Model_SS_raw <- nimbleCode({

#-------------------------------------------------------------------------------
## DEFINE PRIORS SURVIVAL
# outside of loop as constant

mean_phi_juv ~ dunif(0, 1)
mean_phi_adult ~ dunif(0, 1)
mean_p ~ dunif(0, 1)

## parameters

# vector of initial state probabilities (juv surv, adult surv, death)
initial_survival[1] <- 1
initial_survival[2] <- 1
initial_survival[3] <- 0

# matrix of transitions from juv, to adult, to dead (STATE)
# the probabilities should be the probability of being 1, 2 or 3
# STATES = alive juv 1, alive adult 2, dead 3

transition[1, 1] <- 0 # Pr(juv alive t -> juv alive t+1)
transition[1, 2] <- mean_phi_juv # Pr(juv alive t -> adult alive t+1)
transition[1, 3] <- 1-mean_phi_juv # Pr(juv alive t -> dead t+1)
transition[2, 1] <- 0 # Pr(adult alive t -> juv alive t+1)
transition[2, 2] <- mean_phi_adult # Pr(adult alive t -> adult alive t+1)
transition[2, 3] <- 1 - mean_phi_adult # Pr(adult alive t -> dead t+1)
transition[3, 1] <- 0 # Pr(dead t -> juvenile alive t+1)
transition[3, 2] <- 0 # Pr(dead t -> adult alive t+1)
transition[3, 3] <- 1 # Pr(dead t -> dead t+1)

# observation matrix (captures recapture probability)
# OBS =  alive juv 1, alive adult 2, not detected 3
# row 1 = alive juv
# row 2 = alive adult
# row 3 = dead

# column 1 = not observed, column 2 = observed juv, column 3 observed adult
observations[1, 1] <- mean_p # Pr(juv alive t and detected t)
observations[1, 2] <- 0 # Pr(juv alive t and detected as adult t)
observations[1, 3] <- 1 - mean_p # Pr(juv alive t but not detected t)
observations[2, 1] <- 0 # Pr(adult alive t but detected as juv t)
observations[2, 2] <- mean_p # Pr(adult alive t and detected t)
observations[2, 3] <- 1 - mean_p # Pr(adult alive t and not detected t)
observations[3, 1] <- 0 # Pr(dead t and detected as juvenile t)
observations[3, 2] <- 0 # Pr(dead t and detected as adult t)
observations[3, 3] <- 1 # Pr(dead t and not detected t)

## likelihood survival

# need to iterate over each individual and time
# first go over time for individual i using categorical distribution
# need to identify the first entry in the state where the individual is alive
# THEN apply the likelihood
for (i in 1:N){
  surv_state[i, first[i]] ~ dcat(initial_survival[1:3])
  for (j in (first[i]+1):occasions){
    surv_state[i,j] ~ dcat(transition[surv_state[i, j-1], 1:3])
    surv_obs[i,j] ~ dcat(observations[surv_state[i, j], 1:3])
  }
}

#-------------------------------------------------------------------------------
## DEFINE PRIORS FECUNDITY 
## these are outside loop as constant for all breeding attempts

# vague priors
alpha ~ dnorm(0, sd = 1.5)  
beta_age ~ dnorm(0, sd = 1.5)  

## LIKELIHOOD FECUNDITY IN LOOP

for(f in 1:length(offspring_obs)){

# observed offspring
offspring_obs[f] ~ dpois(offspring_state[f])

# process for offspring
offspring_state[f] ~ dpois(fecundity_rate[f])
log(fecundity_rate[f]) <- log_fecundity_rate[f]
log_fecundity_rate[f] <- alpha + beta_age*age[f]

}

#-------------------------------------------------------------------------------
## Set up MPM

mean_fecundity_juv <- exp(alpha + (beta_age*1)) # fecundity for juveniles
mean_fecundity_adult <- exp(alpha + (beta_age*2)) # fecundity for adults 

# make a transition matrix
#transition_matrix <- makeMPM(f_juv = c(mean_fecundity_juv), 
#                             s_juv = c(mean_phi_juv),
#                             f_adult = c(mean_fecundity_adult),
#                             s_adult = c(mean_phi_adult))

transition_matrix[1,1] <- mean_fecundity_juv
transition_matrix[1,2] <- mean_fecundity_adult # fecundity for adults                           
transition_matrix[2,1] <- mean_phi_juv # juvenile survival
transition_matrix[2,2] <- mean_phi_adult # adult survival



#-------------------------------------------------------------------------------
## Derived quantities MPM

eigen_values <- nimEigen(transition_matrix[1:2,1:2])

# lambda
#lambda[1] <- eigen(transition_matrix)$values[1]

# stable size distribution
#size_distribution[1:2] <- eigen(transition_matrix)$vectors[,1]/
#sum(eigen(transition_matrix)$vectors[,1])

})

#### TRY add nimble function to create transition matrix ####

makeMPM <- nimbleFunction( run = function(
    mat = double(2)
)
{ 
  # Specify matrix of correct dimensions
  MPM <- nimMatrix(mat, nrow = 2, ncol = 2)
  
  eigen_values <- nimEigen(MPM)
  
# return
return(eigen_values)
returnType(double(2))

} 
)



