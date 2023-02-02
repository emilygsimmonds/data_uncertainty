# T1.1: Script to run the popbio model #

################################################################################

# This script uses the popbio package to directly estimate vital rates
# it then conducts a bootstrap to get uncertainty on them and lambda

################################################################################

#### Set up ####

# load packages

library(tidyverse)
library(popbio)

# source functions
source("./Functions/transition_frequency.R")
source("./Functions/make_matrix.R")
source("./Functions/bootstrap_tf.R")

# load data
load("./Data files/baseline_observations_statemat1.RData")

################################################################################

#### construct a transition frequency table

# for the state
tf_tables <- map(.x = baseline_observations, 
                 ~{create_transition_frequency_table(.x,
                  max_year = max(.x$Year),
                  stages = c("juvenile", "adult")})

#### run bootstrap to get CIs for vital rates and lambda

# for state
state_results <- bootstrap_summary(tf_table_state, 
                                  iterations = 2000)




