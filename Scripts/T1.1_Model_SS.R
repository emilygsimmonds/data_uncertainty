# T1.1: Model script: state-space model #

# WILL WANT TO CHECK CODE WORKS IN PARTS AND ALL TOGETHER - DO LOTS OF CHECKS
# Inc. check that matrix inside and matrix outside = same

#### Set up ####

# load packages

library(tidyverse)
library(nimble)
library(MCMCvis)

# load data

#### model ####

Model_SS <- nimbleCode({

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
## DEFINE PARAMETERS MPM

# can this be vectorised?

#logit(mean_survival[1:2]) <- c(mean_phi_juv, mean_phi_adult)

#log(mean_fecundity[1:2]) <- c((alpha + beta_age*1), 
#                              (alpha + beta_age*2))

#-------------------------------------------------------------------------------
## Set up MPM

# make a transition matrix

#transition_matrix <- matrix(c(mean_fecundity[1], mean_survival[1],
#                               mean_fecundity[2], mean_survival[2]), 
#                            nrow = 2, ncol = 2)

#-------------------------------------------------------------------------------
## Derived quantities MPM

#eigen_values <- eigen(transition_matrix)

# lambda
#lambda[1] <- eigen_values$values[1]

# stable size distribution
#size_distribution[1:2] <- eigen_values$vectors[1:2,1]

})

#### Define constants, data and inits ####

load("./Data files/test.RData")

# re-code some of raw data
output_data <- input_data %>%
  filter(Recap == 1) %>% # only keep those that were recaptured
  # re-value Survival so 2 = adult survival and 1 = alive at t
  mutate(Surv = case_when(Age > 1 ~ 2, 
                          TRUE ~ Recap), 
         Age = case_when(Age == 1 ~ 1,
                         Age > 1 ~ 2)) # change age to just juv (1) and adult (2)
  
## DATA
# need to make a capture history and save out age and offspring columns
offspring_obs <- output_data$Offspring
age <- output_data$Age

# capture history
# first re-code survival so it = 1 whenever recaptured
capture_history <- output_data %>%
  # spread out data. The fill = 0 fills in 0s when combo was not observed
  pivot_wider(id_cols = ID, names_from = Year, values_from = Surv,
              values_fill = 3) %>%
  as.matrix()

# store ready for model
data_input <- list(surv_obs = capture_history[,2:10],
                   age = age,
                   offspring_obs = offspring_obs)

## INITS

# set up initial values for survival states (1 on first occasion and 2 after)
surv_state_init <- capture_history[,2:10]
# create a vector of first occasions (THIS WILL BE A CONSTANT)
first <- apply(surv_state_init, 1, function(x) which(x<3)[1])
for(i in 1:nrow(surv_state_init)){
  if(first[i] < 9)
    surv_state_init[i, (first[i]+1):9] <- 2}


inits <- list(mean_phi_juv = runif(1, 0, 1),
              mean_phi_adult = runif(1, 0, 1),
              mean_p = runif(1, 0, 1),
              alpha = rnorm(1, 0, 0.1),
              beta_age = rnorm(1, 0, 0.1),
              surv_state = surv_state_init,
              offspring_state = offspring_obs,
              fecundity_rate = rep(1, length(offspring_obs)))

## CONSTANTS

# number of occasions (Occasions) and number of individuals (N)
constants <- list(N = nrow(capture_history), 
                  occasions = 9, 
                  first = first)

 #### Define parameters to track ####

parameters_to_save <- c("mean_phi_adult",
                        "mean_phi_juv",
                        "mean_p",
                        "alpha",
                        "beta_age" 
#                        "transition_matrix",
#                        "lambda", 
#                        "size_distribution"
)

#### Run set up ####

n_iter <- 500
n_burnin <- 10
n_chains <- 2

## Try the model

Rmodel <- nimbleModel(code = Model_SS, 
                      constants = constants, 
                      data = data_input, 
                      inits = inits)

Rmodel$calculate()
Rmodel$initializeInfo()

Rmodel$beta_age

mcmc.output <- nimbleMCMC(code = Model_SS,
                          constants = constants,
                          data = data_input,
                          inits = inits,
                          monitors = parameters_to_save,
                          niter = n_iter,
                          nburnin = n_burnin,
                          nchains = n_chains)

MCMCsummary(mcmc.output, round = 2)
