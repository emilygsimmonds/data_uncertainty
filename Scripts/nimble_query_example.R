# T1.1: Model script: state-space model #

#### Set up ####

# load packages

library(tidyverse)
library(nimble)
library(MCMCvis)

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
initial_survival[1] <- 1/3
initial_survival[2] <- 1/3
initial_survival[3] <- 1/3

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
# OBS = not detected 1, alive juv 2, alive adult 3
# row 1 = alive juv
# row 2 = alive adult
# row 3 = dead

# column 1 = not observed, column 2 = observed juv, column 3 observed adult

observations[1, 1] <- 1 - mean_p # Pr(alive juv t -> non-detected t)
observations[1, 2] <- mean_p # Pr(alive juv t -> detected juv t)
observations[1, 3] <- 0 # Pr(alive juv t -> detected adult t)
observations[2, 1] <- 1 - mean_p # Pr(alive adult t -> not detected t)
observations[2, 2] <- 0 # Pr(alive adult t -> detected juv t)
observations[2, 3] <- mean_p # Pr(alive adult t -> detected adult t)
observations[3, 1] <- 1 # Pr(dead t -> not detected t)
observations[3, 2] <- 0 # Pr(dead t -> detected juv t)
observations[3, 3] <- 0 # Pr(dead t -> detected adult t)

## likelihood survival

# need to iterate over each individual and time
# first go over time for individual i using categorical distribution
for (i in 1:N){
  surv_state[i, 1] ~ dcat(initial_survival[1:3])
  for (j in 2:occasions){
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



})

#### Define constants, data and inits ####

load("test.RData")

output_data <- input_data

input_data <- output_data %>% filter(Year < 10)

# re-code some of raw data
output_data <- input_data %>%
  filter(Recap == 1) %>% # only keep those that were recaptured
  # re-value Survival so 3 = adult survival and 2 = alive at t
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
              values_fill = 0) %>%
  as.matrix()

# store ready for model
data_input <- list(surv_obs = capture_history[,2:10]+1,
                   age = age,
                   offspring_obs = offspring_obs)

## CONSTANTS

# number of occasions (Occasions) and number of individuals (N)
constants <- list(N = nrow(capture_history), 
                     occasions = 9)

## INITS
# STATES = alive juv 1, alive adult 2, dead 3
# have loop to set up initial values for state
# can only be options that can exist e.g. juvenile to adult to dead not vice versa

# first convert from obs values to state values 0 -> 3
surv_state_init <- capture_history[,2:10] 
surv_state_init[surv_state_init == 0] <- 3 # non-detection -> dead
for(i in 1:nrow(surv_state_init)){
  for(j in 2:ncol(surv_state_init)){
    if(surv_state_init[i,j] == 3){
      if(surv_state_init[i,j-1] == 1){surv_state_init[i,j] <- 2}
      if(surv_state_init[i,j-1] == 2){surv_state_init[i,j] <- 2}
      if(surv_state_init[i,j-1] == 3){surv_state_init[i,j] <- 3}
      }
  }
}


inits <- list(mean_phi_juv = 0.3,
              mean_phi_adult = 0.6,
              mean_p = runif(1, 0, 1),
              alpha = rnorm(1, -0.05, 0.1),
              beta_age = rnorm(1, 0.05, 0.1),
              surv_state = surv_state_init,
              offspring_state = offspring_obs,
              fecundity_rate = rep(1, length(offspring_obs)))

#### Define parameters to track ####

parameters_to_save <- c("mean_phi_adult",
                        "mean_phi_juv",
                        "mean_p",
                        "alpha",
                        "beta_age" 
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

mcmc.output <- nimbleMCMC(code = Model_SS,
                          constants = constants,
                          data = data_input,
                          inits = inits,
                          monitors = parameters_to_save,
                          niter = n_iter,
                          nburnin = n_burnin,
                          nchains = n_chains)

MCMCsummary(mcmc.output, round = 2)
