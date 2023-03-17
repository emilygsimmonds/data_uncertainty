# T1.1: Script to run the popbio model #

################################################################################

# This script uses the popbio package to directly estimate vital rates
# it then conducts a bootstrap to get uncertainty on them and lambda

################################################################################

#### Set up ####

# load packages

library(tidyverse)
library(popbio)
library(furrr)
library(magrittr)

# source functions
source("./Functions/transition_frequency.R")
source("./Functions/make_matrix.R")
source("./Functions/bootstrap_tf.R")


################################################################################
#### 2x2 ####
################################################################################

#### Baseline ####

# load data
filenames <- list.files("./Data files/2x2/", 
                        pattern = "baseline_simulation_observations",
                        full.names = TRUE)
filenames <- filenames[-c(1,102,203,304,405)]

# make a transition frequency table
plan(multisession, workers = 4)

baseline_2x2_tf_tables <- future_map(.x = filenames, ~{
  simulation <- readRDS(.x)
  create_transition_frequency_table(simulation,
  max_year = max(simulation$Year),
  stages = c("juvenile", "adult"))})

#### run bootstrap to get CIs for vital rates and lambda

# then do direct calculation of results with bootstrap
baseline_results_2x2 <- future_map(.x = baseline_2x2_tf_tables, ~{
                            bootstrap_summary(.x, 
                                  iterations =10000,
                                  stages = c("juvenile", "adult"))})

# save
save(baseline_results_2x2, file = "baseline_results_2x2.RData")

#### R error ####

# make a transition frequency table
plan(multisession, workers = 4)

r_error_2x2_tf_tables <- future_map(.x = filenames, ~{
  simulation <- readRDS(.x)
  simulation$Offspring <- simulation$Offspring_obs
  create_transition_frequency_table(simulation,
                                    max_year = max(simulation$Year),
                                    stages = c("juvenile", "adult"))})

 #### run bootstrap to get CIs for vital rates and lambda

# then do direct calculation of results with bootstrap
plan(multisession, workers = 4)

r_error_results_2x2 <- future_map(.x = r_error_2x2_tf_tables, ~{
  bootstrap_summary(.x, 
                    iterations =10000,
                    stages = c("juvenile", "adult"))})

# save
save(r_error_results_2x2, file = "r_error_results_2x2.RData")

#### R missing ####

# load data
filenames <- list.files("./Data files/2x2/", 
                        pattern = "r_missing_simulations",
                        full.names = TRUE)
filenames <- filenames[-c(1,102,203,304,405)]

# make a transition frequency table
plan(multisession, workers = 4)

r_missing_2x2_tf_tables <- future_map(.x = filenames, ~{
  simulation <- readRDS(.x)
  simulation$Offspring <- simulation$Offspring_obs
  create_transition_frequency_table(simulation,
                                    max_year = max(simulation$Year),
                                    stages = c("juvenile", "adult"))})

#### run bootstrap to get CIs for vital rates and lambda

# then do direct calculation of results with bootstrap
plan(multisession, workers = 4)

r_missing_results_2x2 <- future_map(.x = r_missing_2x2_tf_tables, ~{
  bootstrap_summary(.x, 
                    iterations =10000,
                    stages = c("juvenile", "adult"))})

# save
save(r_missing_results_2x2, file = "r_missing_results_2x2.RData")

#### J missing ####

# load data
filenames <- list.files("./Data files/2x2/", 
                        pattern = "j_missing_simulations",
                        full.names = TRUE)
filenames <- filenames[-c(1,102,203,304,405)]

# make a transition frequency table
plan(multisession, workers = 4)

j_missing_2x2_tf_tables <- future_map(.x = filenames, ~{
  simulation <- readRDS(.x)
  simulation$Offspring <- simulation$Offspring_obs
  create_transition_frequency_table(simulation,
                                    max_year = max(simulation$Year),
                                    stages = c("juvenile", "adult"))})

#### run bootstrap to get CIs for vital rates and lambda

# then do direct calculation of results with bootstrap
plan(multisession, workers = 4)

j_missing_results_2x2 <- future_map(.x = j_missing_2x2_tf_tables, ~{
  bootstrap_summary(.x, 
                    iterations =10000,
                    stages = c("juvenile", "adult"))})

# save
save(j_missing_results_2x2, file = "j_missing_results_2x2.RData")

#### A missing ####

# load data
filenames <- list.files("./Data files/2x2/", 
                        pattern = "a_missing_simulations",
                        full.names = TRUE)
filenames <- filenames[-c(1,102,203,304,405)]

# make a transition frequency table
plan(multisession, workers = 4)

a_missing_2x2_tf_tables <- future_map(.x = filenames, ~{
  simulation <- readRDS(.x)
  simulation$Offspring <- simulation$Offspring_obs
  create_transition_frequency_table(simulation,
                                    max_year = max(simulation$Year),
                                    stages = c("juvenile", "adult"))})

#### run bootstrap to get CIs for vital rates and lambda

# then do direct calculation of results with bootstrap
plan(multisession, workers = 4)

a_missing_results_2x2 <- future_map(.x = a_missing_2x2_tf_tables, ~{
  bootstrap_summary(.x, 
                    iterations =10000,
                    stages = c("juvenile", "adult"))})

# save
save(a_missing_results_2x2, file = "a_missing_results_2x2.RData")
