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
source("./Functions/make_input_data_function.R")
source("./Functions/transition_frequency.R")

# load data
load("./Data files/baseline_simulation_statemat1.RData")

################################################################################

#### construct a transition frequency table

# for the state
tf_table_state <- create_transition_frequency_table(baseline_state[[5]],
                                                    max_year = max(baseline_state[[5]]$Year))

# make population matrix
make_matrix(tf_table_state)


#### run bootstrap to get CIs for vital rates and lambda

# for state
state_results <- boot.transitions(tf_table_state, 
                                  iterations = 2000)




