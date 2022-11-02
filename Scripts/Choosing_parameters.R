#### Script to choose and test parameters for different stages of recruitment ####

#### Set up ####

# load packages

library(HMMpa) # has functions for generalised Poisson distribution

#### Set up parameters ####

breeding_probability <- rbinom(10000, 1, 0.95) # assume very high breeding probability

# mean of the generalised poisson = lambda1/1-lambda2 so 6 here
clutch_size <- rgenpois(10000, lambda1 = 9, 
                        lambda2 = -0.5)*breeding_probability # upper limit (-lambda1/lambda2)

mean(clutch_size)
var(clutch_size)

nest_success <- rbinom(10000, 1, 0.88) # high nest success or litter success 
# (i.e. low predation, low abandonment)

# but low juvenile survival - individuals prone to die later or from individual causes
juvenile_survival <- rbinom(10000, clutch_size*nest_success, 0.15)

mean(juvenile_survival) 
var(juvenile_survival)

# all parameters chosen to give recruitment rate of approx 0.75 to match 
# simulation parameters. Also, to give mean and variance that are almost equal
# so can use Poisson model 

# To get final numbers breeding_prob * mean clutch * nest_success * juvenile survival

0.85 * 6 * 0.80 * 0.15

0.95 * 6 * 0.95 * 0.15
