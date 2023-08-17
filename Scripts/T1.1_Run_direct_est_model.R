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
                                    stages = c("juvenile", "adult"))},
  .seed = TRUE)

#### run bootstrap to get CIs for vital rates and lambda

# then do direct calculation of results with bootstrap
baseline_results_2x2 <- future_map(.x = baseline_2x2_tf_tables, ~{
  bootstrap_summary(.x, 
                    iterations =10000,
                    stages = c("juvenile", "adult"))},
  .seed = TRUE)

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
                                    stages = c("juvenile", "adult"))},
  .seed = TRUE)

#### run bootstrap to get CIs for vital rates and lambda

# then do direct calculation of results with bootstrap
plan(multisession, workers = 4)

r_error_results_2x2 <- future_map(.x = r_error_2x2_tf_tables, ~{
  bootstrap_summary(.x, 
                    iterations =10000,
                    stages = c("juvenile", "adult"))},
  .seed = TRUE)

# save
save(r_error_results_2x2, file = "r_error_results_2x2.RData")

#### R missing ####

# load data
filenames <- list.files("./Data files/2x2/", 
                        pattern = "random_missing_simulation",
                        full.names = TRUE)
filenames <- filenames[-c(1,102,203,304,405)]

# make a transition frequency table
plan(multisession, workers = 4)

r_missing_2x2_tf_tables <- future_map(.x = filenames, ~{
  simulation <- readRDS(.x)
  simulation$Offspring <- simulation$Offspring
  create_transition_frequency_table(simulation,
                                    max_year = max(simulation$Year),
                                    stages = c("juvenile", "adult"))},
  .seed = TRUE)

#### run bootstrap to get CIs for vital rates and lambda

# then do direct calculation of results with bootstrap
plan(multisession, workers = 4)

r_missing_results_2x2 <- future_map(.x = r_missing_2x2_tf_tables, ~{
  bootstrap_summary(.x, 
                    iterations =10000,
                    stages = c("juvenile", "adult"))},
  .seed = TRUE)

# save
save(r_missing_results_2x2, file = "r_missing_results_2x2.RData")

#### J missing ####

# load data
filenames <- list.files("./Data files/2x2/", 
                        pattern = "juvenile_missing_simulation",
                        full.names = TRUE)
filenames <- filenames[-c(1,102,203,304,405)]

# make a transition frequency table
plan(multisession, workers = 4)

j_missing_2x2_tf_tables <- future_map(.x = filenames, ~{
  simulation <- readRDS(.x)
  simulation$Offspring <- simulation$Offspring
  create_transition_frequency_table(simulation,
                                    max_year = max(simulation$Year),
                                    stages = c("juvenile", "adult"))},
  .seed = TRUE)

#### run bootstrap to get CIs for vital rates and lambda

# then do direct calculation of results with bootstrap
plan(multisession, workers = 4)

j_missing_results_2x2 <- future_map(.x = j_missing_2x2_tf_tables, ~{
  bootstrap_summary(.x, 
                    iterations =10000,
                    stages = c("juvenile", "adult"))},
  .seed = TRUE)

# save
save(j_missing_results_2x2, file = "j_missing_results_2x2.RData")

#### A missing ####

# load data
filenames <- list.files("./Data files/2x2/", 
                        pattern = "adult_missing_simulation",
                        full.names = TRUE)
filenames <- filenames[-c(1,102,203,304,405)]

# make a transition frequency table
plan(multisession, workers = 4)

a_missing_2x2_tf_tables <- future_map(.x = filenames, ~{
  simulation <- readRDS(.x)
  simulation$Offspring <- simulation$Offspring
  create_transition_frequency_table(simulation,
                                    max_year = max(simulation$Year),
                                    stages = c("juvenile", "adult"))},
  .seed = TRUE)

#### run bootstrap to get CIs for vital rates and lambda

# then do direct calculation of results with bootstrap
plan(multisession, workers = 4)

a_missing_results_2x2 <- future_map(.x = a_missing_2x2_tf_tables, ~{
  bootstrap_summary(.x, 
                    iterations =10000,
                    stages = c("juvenile", "adult"))},
  .seed = TRUE)

# save
save(a_missing_results_2x2, file = "a_missing_results_2x2.RData")

################################################################################
#### 3x3 ####
################################################################################

#### Baseline ####

# load data
filenames <- list.files("./Data files/3x3/", 
                        pattern = "baseline_simulation_observations",
                        full.names = TRUE)
filenames <- filenames[-c(1,102,203,304,405)]

# make a transition frequency table
plan(multisession, workers = 4)

baseline_3x3_tf_tables <- future_map(.x = filenames, ~{
  simulation <- readRDS(.x)
  create_transition_frequency_table(simulation,
                                    max_year = max(simulation$Year),
                                    stages = c("juvenile", "subadult", "adult"))},
  .seed = TRUE)

#### run bootstrap to get CIs for vital rates and lambda

# then do direct calculation of results with bootstrap
baseline_results_3x3 <- future_map(.x = baseline_3x3_tf_tables, ~{
  bootstrap_summary(.x, 
                    iterations =10000,
                    stages = c("juvenile", "subadult", "adult"))},
  .seed = TRUE)

# save
save(baseline_results_3x3, file = "./Data files/3x3/baseline_results_3x3.RData")

#### R error ####

# make a transition frequency table
plan(multisession, workers = 4)

r_error_3x3_tf_tables <- future_map(.x = filenames, ~{
  simulation <- readRDS(.x)
  simulation$Offspring <- simulation$Offspring_obs
  create_transition_frequency_table(simulation,
                                    max_year = max(simulation$Year),
                                    stages = c("juvenile", "subadult", "adult"))},
  .seed = TRUE)

#### run bootstrap to get CIs for vital rates and lambda

# then do direct calculation of results with bootstrap
plan(multisession, workers = 4)

r_error_results_3x3 <- future_map(.x = r_error_3x3_tf_tables, ~{
  bootstrap_summary(.x, 
                    iterations =10000,
                    stages = c("juvenile", "subadult", "adult"))},
  .seed = TRUE)

# save
save(r_error_results_3x3, file = "./Data files/3x3/r_error_results_3x3.RData")

#### R missing ####

# load data
filenames <- list.files("./Data files/3x3/", 
                        pattern = "random_missing_simulation",
                        full.names = TRUE)
filenames <- filenames[-c(1,102,203,304,405)]

# make a transition frequency table
plan(multisession, workers = 4)

r_missing_3x3_tf_tables <- future_map(.x = filenames, ~{
  simulation <- readRDS(.x)
  simulation$Offspring <- simulation$Offspring
  create_transition_frequency_table(simulation,
                                    max_year = max(simulation$Year),
                                    stages = c("juvenile", "subadult", "adult"))},
  .seed = TRUE)

#### run bootstrap to get CIs for vital rates and lambda

# then do direct calculation of results with bootstrap
plan(multisession, workers = 4)

r_missing_results_3x3 <- future_map(.x = r_missing_3x3_tf_tables, ~{
  bootstrap_summary(.x, 
                    iterations =10000,
                    stages = c("juvenile", "subadult", "adult"))},
  .seed = TRUE)

# save
save(r_missing_results_3x3, file = "./Data files/3x3/r_missing_results_3x3.RData")

#### J missing ####

# load data
filenames <- list.files("./Data files/3x3/", 
                        pattern = "juvenile_missing_simulation",
                        full.names = TRUE)
filenames <- filenames[-c(1,102,203,304,405)]

# make a transition frequency table
plan(multisession, workers = 4)

j_missing_3x3_tf_tables <- future_map(.x = filenames, ~{
  simulation <- readRDS(.x)
  simulation$Offspring <- simulation$Offspring
  create_transition_frequency_table(simulation,
                                    max_year = max(simulation$Year),
                                    stages = c("juvenile", "subadult", "adult"))},
  .seed = TRUE)

#### run bootstrap to get CIs for vital rates and lambda

# then do direct calculation of results with bootstrap
plan(multisession, workers = 4)

j_missing_results_3x3 <- future_map(.x = j_missing_3x3_tf_tables, ~{
  bootstrap_summary(.x, 
                    iterations =10000,
                    stages = c("juvenile", "subadult", "adult"))},
  .seed = TRUE)

# save
save(j_missing_results_3x3, file = "./Data files/3x3/j_missing_results_3x3.RData")

#### A missing ####

# load data
filenames <- list.files("./Data files/3x3/", 
                        pattern = "adult_missing_simulation",
                        full.names = TRUE)
filenames <- filenames[-c(1,102,203,304,405)]

# make a transition frequency table
plan(multisession, workers = 4)

a_missing_3x3_tf_tables <- future_map(.x = filenames, ~{
  simulation <- readRDS(.x)
  simulation$Offspring <- simulation$Offspring
  create_transition_frequency_table(simulation,
                                    max_year = max(simulation$Year),
                                    stages = c("juvenile", "subadult", "adult"))},
  .seed = TRUE)

#### run bootstrap to get CIs for vital rates and lambda

# then do direct calculation of results with bootstrap
plan(multisession, workers = 4)

a_missing_results_3x3 <- future_map(.x = a_missing_3x3_tf_tables, ~{
  bootstrap_summary(.x, 
                    iterations =10000,
                    stages = c("juvenile", "subadult", "adult"))},
  .seed = TRUE)

# save
save(a_missing_results_3x3, file = "./Data files/3x3/a_missing_results_3x3.RData")

################################################################################
#### 5x5 ####
################################################################################

#### Baseline ####

# load data
filenames <- list.files("./Data files/5x5/", 
                        pattern = "baseline_simulation_observations",
                        full.names = TRUE)
filenames <- filenames[-c(1,102,203,304,405)]

# make a transition frequency table
plan(multisession, workers = 4)

baseline_5x5_tf_tables <- future_map(.x = filenames, ~{
  simulation <- readRDS(.x)
  create_transition_frequency_table(simulation,
                                    max_year = max(simulation$Year),
                                    stages = c("juvenile", "subadult", 
                                               "adult1",
                                               "adult2",
                                               "adult3"))},
  .options = furrr_options(seed = TRUE))

#### run bootstrap to get CIs for vital rates and lambda

# then do direct calculation of results with bootstrap
baseline_results_5x5 <- future_map(.x = baseline_5x5_tf_tables, ~{
  bootstrap_summary(.x, 
                    iterations =10000,
                    stages = c("juvenile", "subadult", 
                               "adult1",
                               "adult2",
                               "adult3"))},
  .options = furrr_options(seed = TRUE))

# save
save(baseline_results_5x5, file = "./Data files/5x5/baseline_results_5x5.RData")

#### R error ####

# make a transition frequency table
plan(multisession, workers = 4)

r_error_5x5_tf_tables <- future_map(.x = filenames, ~{
  simulation <- readRDS(.x)
  simulation$Offspring <- simulation$Offspring_obs
  create_transition_frequency_table(simulation,
                                    max_year = max(simulation$Year),
                                    stages = c("juvenile", "subadult", 
                                               "adult1",
                                               "adult2",
                                               "adult3"))},
  .options = furrr_options(seed = TRUE))

#### run bootstrap to get CIs for vital rates and lambda

# then do direct calculation of results with bootstrap
plan(multisession, workers = 4)

r_error_results_5x5 <- future_map(.x = r_error_5x5_tf_tables, ~{
  bootstrap_summary(.x, 
                    iterations =10000,
                    stages = c("juvenile", "subadult", 
                               "adult1",
                               "adult2",
                               "adult3"))},
  .options = furrr_options(seed = TRUE))

# save
save(r_error_results_5x5, file = "./Data files/5x5/r_error_results_5x5.RData")

#### R missing ####

# load data
filenames <- list.files("./Data files/5x5/", 
                        pattern = "random_missing_simulation",
                        full.names = TRUE)
filenames <- filenames[-c(1,102,203,304,405)]

# make a transition frequency table
plan(multisession, workers = 4)

r_missing_5x5_tf_tables <- future_map(.x = filenames, ~{
  simulation <- readRDS(.x)
  simulation$Offspring <- simulation$Offspring
  create_transition_frequency_table(simulation,
                                    max_year = max(simulation$Year),
                                    stages = c("juvenile", "subadult", 
                                               "adult1",
                                               "adult2",
                                               "adult3"))},
  .options = furrr_options(seed = TRUE))

#### run bootstrap to get CIs for vital rates and lambda

# then do direct calculation of results with bootstrap
plan(multisession, workers = 4)

r_missing_results_5x5 <- future_map(.x = r_missing_5x5_tf_tables, ~{
  bootstrap_summary(.x, 
                    iterations =10000,
                    stages = c("juvenile", "subadult", 
                               "adult1",
                               "adult2",
                               "adult3"))},
  .options = furrr_options(seed = TRUE))

# save
save(r_missing_results_5x5, file = "./Data files/5x5/r_missing_results_5x5.RData")

#### J missing ####

# load data
filenames <- list.files("./Data files/5x5/", 
                        pattern = "juvenile_missing_simulation",
                        full.names = TRUE)
filenames <- filenames[-c(1,102,203,304,405)]

# make a transition frequency table
plan(multisession, workers = 4)

j_missing_5x5_tf_tables <- future_map(.x = filenames, ~{
  simulation <- readRDS(.x)
  simulation$Offspring <- simulation$Offspring
  create_transition_frequency_table(simulation,
                                    max_year = max(simulation$Year),
                                    stages = c("juvenile", "subadult", 
                                               "adult1",
                                               "adult2",
                                               "adult3"))},
  .options = furrr_options(seed = TRUE))

#### run bootstrap to get CIs for vital rates and lambda

# then do direct calculation of results with bootstrap
plan(multisession, workers = 4)

j_missing_results_5x5 <- future_map(.x = j_missing_5x5_tf_tables, ~{
  bootstrap_summary(.x, 
                    iterations =10000,
                    stages = c("juvenile", "subadult", 
                               "adult1",
                               "adult2",
                               "adult3"))},
  .options = furrr_options(seed = TRUE))

# save
save(j_missing_results_5x5, file = "./Data files/5x5/j_missing_results_5x5.RData")

#### A missing ####

# load data
filenames <- list.files("./Data files/5x5/", 
                        pattern = "adult_missing_simulation",
                        full.names = TRUE)
filenames <- filenames[-c(1,102,203,304,405)]

# make a transition frequency table
plan(multisession, workers = 4)

a_missing_5x5_tf_tables <- future_map(.x = filenames, ~{
  simulation <- readRDS(.x)
  simulation$Offspring <- simulation$Offspring
  create_transition_frequency_table(simulation,
                                    max_year = max(simulation$Year),
                                    stages = c("juvenile", "subadult", 
                                               "adult1",
                                               "adult2",
                                               "adult3"))},
  .options = furrr_options(seed = TRUE))

#### run bootstrap to get CIs for vital rates and lambda

# then do direct calculation of results with bootstrap
plan(multisession, workers = 4)

a_missing_results_5x5 <- future_map(.x = a_missing_5x5_tf_tables, ~{
  bootstrap_summary(.x, 
                    iterations =10000,
                    stages = c("juvenile", "subadult", 
                               "adult1",
                               "adult2",
                               "adult3"))},
  .options = furrr_options(seed = TRUE))

# save
save(a_missing_results_5x5, file = "./Data files/5x5/a_missing_results_5x5.RData")

