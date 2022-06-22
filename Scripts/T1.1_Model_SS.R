# T1.1: Model script: state-space model #

# WILL WANT TO CHECK CODE WORKS IN PARTS AND ALL TOGETHER - DO LOTS OF CHECKS
# Inc. check that matrix inside and matrix outside = same

#### Set up ####

# load packages

# load data

#### model ####

#-------------------------------------------------------------------------------
## priors for survival

## likelihood survival

#-------------------------------------------------------------------------------
## DEFINE PRIORS FECUNDITY 
## these are outside loop as constant for all breeding attempts

alpha ~ dnorm(x, x)  
beta_age ~ dnorm(x, x)  

## LIKELIHOOD FECUNDITY IN LOOP

for(f in 1:length(offspring_obs)){

# observed offspring
offspring_obs[f] ~ dpois(offspring[f])

# process for offspring
offspring[f] ~ dpois(fecundity_rate[f])
log(fecundity_rate[f]) <- alpha + beta_age*age[f]

}

#-------------------------------------------------------------------------------
## DEFINE PARAMETERS MPM

logit(mean_surv_juv) <- mean_phi_juv
logit(mean_surv_adult) <- mean_phi_adult

log(mean_juv_offspring) <- alpha + beta_age*0 # first years
log(mean_adult_offspring) <- alpha + beta_age*1 # adults

#-------------------------------------------------------------------------------
## Set up MPM

#-------------------------------------------------------------------------------
## Derived quantities MPM

# lambda

# stable size distribution