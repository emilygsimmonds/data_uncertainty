#### T1.1: Script that simulates data for all scenarios #

################################################################################
#
# Features of the population:
# - female-based (reproduction is the number of juv. females)
# - does NOT include density-dependence
#
# Inputs:
# - Input_data = a dataframe with column names: ID (factor), Year (factor), 
# Surv (0/1), Recap (0/1), Clutch_size (num), Offspring (num), Age (num), 
# Trait (num)
#
# - parameters = matrix of parameter values (transition matrix) inc phi, f
# 
# - inc_trait = TRUE or FALSE if you want to include a trait as well
# (so far not used)
#
#
################################################################################

#### Set up ####

# load packages

library(tidyverse)

# load any data

# source necessary functions
source("./Functions/run_simulation.R")
source("./Functions/run_observation_process.R")
source("./Functions/simulation_setup.R")
source("./Functions/create_scenario_data.R")
source("./Functions/create_scenario_data_not_random.R")
source("./Functions/split_simulations_function.R")

################################################################################

#### ALL RANDOM SIMULATIONS 2x2: baseline, random error, random missing ####

#### Import matrices ####

load("./Data files/2x2_files/input_files/twobytwo_matrices.RData")

# name each matrix

names(output_matrices) <- c("mat1",
                            "mat2",
                            "mat3",
                            "mat4",
                            "mat5")

#### Create simulated data ####

# BASELINE = No observation processes
# RANDOM ERROR = ONLY Poisson error to reproduction
# RANDOM MISSING = 30% missing randomly across whole population (does impact 
# survival too) - still structured by stage
# NOT RANDOM MISSING = 30% missing but not random - has own reproduction dataset

create_scenario_data(parameters = output_matrices[["mat1"]],
                     name = "mat1", recapture = c(1,1),
                     missing = c(0.7,0.7),
                     phi = c(output_matrices[["mat1"]][2,]),
                     stages = c("juvenile", "adult"),
                     repro_stages = c("juvenile", "adult"),
                     location = "./Data files/2x2_files/simulations/") 

create_scenario_data(parameters = output_matrices[["mat2"]],
                     name = "mat2", recapture = c(1,1),
                     missing = c(0.7,0.7),
                     phi = c(output_matrices[["mat2"]][2,]),
                     stages = c("juvenile", "adult"),
                     repro_stages = c("juvenile", "adult"),
                     location = "./Data files/2x2_files/simulations/") 

create_scenario_data(parameters = output_matrices[["mat3"]],
                     name = "mat3", recapture = c(1,1),
                     missing = c(0.7,0.7),
                     phi = c(output_matrices[["mat3"]][2,]),
                     stages = c("juvenile", "adult"),
                     repro_stages = c("juvenile", "adult"),
                     location = "./Data files/2x2_files/simulations/") 

create_scenario_data(parameters = output_matrices[["mat4"]],
                     name = "mat4", recapture = c(1,1),
                     missing = c(0.7,0.7),
                     phi = c(output_matrices[["mat4"]][2,]),
                     stages = c("juvenile", "adult"),
                     repro_stages = c("juvenile", "adult"),
                     location = "./Data files/2x2_files/simulations/") 

create_scenario_data(parameters = output_matrices[["mat5"]],
                     name = "mat5", recapture = c(1,1),
                     missing = c(0.7,0.7),
                     phi = c(output_matrices[["mat5"]][2,]),
                     stages = c("juvenile", "adult"),
                     repro_stages = c("juvenile", "adult"),
                     location = "./Data files/2x2_files/simulations/") 

################################################################################

#### Split simulations for faster programming ####

# get filenames

filenames <- list.files("./Data files/2x2", 
                        pattern = "baseline_simulation_observations",
                        full.names = TRUE)

map(.x = filenames, 
    split_simulations,
    location = "./Data files/2x2/")


filenames <- list.files("./Data files/2x2", 
                        pattern = "random_missing_simulation",
                        full.names = TRUE)

map(.x = filenames, 
    split_simulations,
    location = "./Data files/2x2/")

################################################################################

#### ALL RANDOM SIMULATIONS 3x3: baseline, random error, random missing ####

#### Import matrices ####

load("./Data files/3x3_files/input_files/threebythree_matrices.RData")

# name each matrix

names(matrices_33) <- c("mat1",
                            "mat2",
                            "mat3",
                            "mat4",
                            "mat5")

#### Create simulated data ####

create_scenario_data(parameters = matrices_33[["mat1"]],
                     name = "mat1", recapture = c(1,1,1),
                     missing = c(0.7,0.7,0.7),
                     phi = c(matrices_33[["mat1"]][2,1],
                             matrices_33[["mat1"]][3,2],
                             matrices_33[["mat1"]][3,3]),
                     stages = c("juvenile", "subadult", "adult"),
                     repro_stages = c("subadult", "adult"),
                     location = "./Data files/3x3_files/simulations") 

create_scenario_data(parameters = matrices_33[["mat2"]],
                     name = "mat2", recapture = c(1,1,1),
                     missing = c(0.7,0.7,0.7),
                     phi = c(matrices_33[["mat2"]][2,1],
                             matrices_33[["mat2"]][3,2],
                             matrices_33[["mat2"]][3,3]),
                     stages = c("juvenile", "subadult", "adult"),
                     repro_stages = c("subadult", "adult"),
                     location = "./Data files/3x3_files/simulations")  

create_scenario_data(parameters = matrices_33[["mat3"]],
                     name = "mat3", recapture = c(1,1,1),
                     missing = c(0.7,0.7,0.7),
                     phi = c(matrices_33[["mat3"]][2,1],
                             matrices_33[["mat3"]][3,2],
                             matrices_33[["mat3"]][3,3]),
                     stages = c("juvenile", "subadult", "adult"),
                     repro_stages = c("subadult", "adult"),
                     location = "./Data files/3x3_files/simulations") 

create_scenario_data(parameters = matrices_33[["mat4"]],
                     name = "mat4", recapture = c(1,1,1),
                     missing = c(0.7,0.7,0.7),
                     phi = c(matrices_33[["mat4"]][2,1],
                             matrices_33[["mat4"]][3,2],
                             matrices_33[["mat4"]][3,3]),
                     stages = c("juvenile", "subadult", "adult"),
                     repro_stages = c("subadult", "adult"),
                     location = "./Data files/3x3_files/simulations") 

create_scenario_data(parameters = matrices_33[["mat5"]],
                     name = "mat5", recapture = c(1,1,1),
                     missing = c(0.7,0.7,0.7),
                     phi = c(matrices_33[["mat5"]][2,1],
                             matrices_33[["mat5"]][3,2],
                             matrices_33[["mat5"]][3,3]),
                     stages = c("juvenile", "subadult", "adult"),
                     repro_stages = c("subadult", "adult"),
                     location = "./Data files/3x3_files/simulations") 



################################################################################

#### Split simulations for faster programming ####

# get filenames

filenames <- list.files("./Data files/3x3_files/simulations", 
                        pattern = "baseline_simulation_observations",
                        full.names = TRUE)

# then split RData file into smaller RDS

map(.x = filenames, 
    split_simulations,
    location = "./Data files/3x3_files/simulations/")

# get filenames

filenames <- list.files("./Data files/3x3_files/simulations", 
                        pattern = "random_missing_simulation",
                        full.names = TRUE)

# then split RData file into smaller RDS

map(.x = filenames, 
    split_simulations,
    location = "./Data files/3x3_files/simulations/")

################################################################################

#### ALL NON-RANDOM SIMULATIONS 2x2: low non random, high non random ####

#### Import matrices ####

load("./Data files/2x2_files/input_files/twobytwo_matrices.RData")

# get list of all baseline data files
filenames <- list.files("./Data files/2x2_files/simulations", 
                        pattern = "baseline_simulation_observations",
                        # removing .RData files
                        full.names = TRUE)[-c(1,102,203,304,405)] 

# name each matrix
matrices_22 <- output_matrices

names(matrices_22) <- c("mat1",
                        "mat2",
                        "mat3",
                        "mat4",
                        "mat5")

#### Create simulated data: LOW NON RANDOM ####
create_scenario_data_not_random(
  baseline = as.list(filenames[1:100]),
  parameters = matrices_22[["mat1"]],
  name = "mat1", recapture = c(1,1),
  missing = c(0.7,0.7),
  phi = c(matrices_22[["mat1"]][2,1],
          matrices_22[["mat1"]][2,2]),
  stages = c("juvenile", "adult"),
  repo_stages = c("juvenile", "adult"),
  split = 0.5, offset = 0.2, bias = "low",
  location = "./Data files/2x2_files/simulations/")

create_scenario_data_not_random(
  baseline = as.list(filenames[101:200]),
  parameters = matrices_22[["mat2"]],
  name = "mat2", recapture = c(1,1),
  missing = c(0.7,0.7),
  phi = c(matrices_22[["mat2"]][2,1],
          matrices_22[["mat2"]][2,2]),
  stages = c("juvenile", "adult"),
  repo_stages = c("juvenile", "adult"),
  location = "./Data files/2x2_files/simulations/",
  split = 0.5, offset = 0.2, bias = "low")  

create_scenario_data_not_random(
  baseline = as.list(filenames[201:300]),
  parameters = matrices_22[["mat3"]],
  name = "mat3", recapture = c(1,1),
  missing = c(0.7,0.7),
  phi = c(matrices_22[["mat3"]][2,1],
          matrices_22[["mat3"]][2,2]),
  stages = c("juvenile", "adult"),
  repo_stages = c("juvenile", "adult"),
  location = "./Data files/2x2_files/simulations/",
  split = 0.5, offset = 0.2, bias = "low") 

create_scenario_data_not_random(
  baseline = as.list(filenames[301:400]),
  parameters = matrices_22[["mat4"]],
  name = "mat4", recapture = c(1,1),
  missing = c(0.7,0.7),
  phi = c(matrices_22[["mat4"]][2,1],
          matrices_22[["mat4"]][2,2]),
  stages = c("juvenile", "adult"),
  repo_stages = c("juvenile", "adult"),
  location = "./Data files/2x2_files/simulations/",
  split = 0.5, offset = 0.2, bias = "low") 

create_scenario_data_not_random(
  baseline = as.list(filenames[401:500]),
  parameters = matrices_22[["mat5"]],
  name = "mat5", recapture = c(1,1),
  missing = c(0.7,0.7),
  phi = c(matrices_22[["mat5"]][2,1],
          matrices_22[["mat5"]][2,2]),
  stages = c("juvenile", "adult"),
  repo_stages = c("juvenile", "adult"),
  location = "./Data files/2x2_files/simulations/",
  split = 0.5, offset = 0.2, bias = "low")

#### Create simulated data: HIGH NON RANDOM ####
create_scenario_data_not_random(
  baseline = as.list(filenames[1:100]),
  parameters = matrices_22[["mat1"]],
  name = "mat1", recapture = c(1,1),
  missing = c(0.7,0.7),
  phi = c(matrices_22[["mat1"]][2,1],
          matrices_22[["mat1"]][2,2]),
  stages = c("juvenile", "adult"),
  repo_stages = c("juvenile", "adult"),
  location = "./Data files/2x2_files/simulations/",
  split = 0.5, offset = 0.2, bias = "high")

create_scenario_data_not_random(
  baseline = as.list(filenames[101:200]),
  parameters = matrices_22[["mat2"]],
  name = "mat2", recapture = c(1,1),
  missing = c(0.7,0.7),
  phi = c(matrices_22[["mat2"]][2,1],
          matrices_22[["mat2"]][2,2]),
  stages = c("juvenile", "adult"),
  repo_stages = c("juvenile", "adult"),
  location = "./Data files/2x2_files/simulations/",
  split = 0.5, offset = 0.2, bias = "high")  

create_scenario_data_not_random(
  baseline = as.list(filenames[201:300]),
  parameters = matrices_22[["mat3"]],
  name = "mat3", recapture = c(1,1),
  missing = c(0.7,0.7),
  phi = c(matrices_22[["mat3"]][2,1],
          matrices_22[["mat3"]][2,2]),
  stages = c("juvenile", "adult"),
  repo_stages = c("juvenile", "adult"),
  location = "./Data files/2x2_files/simulations/",
  split = 0.5, offset = 0.2, bias = "high") 

create_scenario_data_not_random(
  baseline = as.list(filenames[301:400]),
  parameters = matrices_22[["mat4"]],
  name = "mat4", recapture = c(1,1),
  missing = c(0.7,0.7),
  phi = c(matrices_22[["mat4"]][2,1],
          matrices_22[["mat4"]][2,2]),
  stages = c("juvenile", "adult"),
  repo_stages = c("juvenile", "adult"),
  location = "./Data files/2x2_files/simulations/",
  split = 0.5, offset = 0.2, bias = "high") 

create_scenario_data_not_random(
  baseline = as.list(filenames[401:500]),
  parameters = matrices_22[["mat5"]],
  name = "mat5", recapture = c(1,1),
  missing = c(0.7,0.7),
  phi = c(matrices_22[["mat5"]][2,1],
          matrices_22[["mat5"]][2,2]),
  stages = c("juvenile", "adult"),
  repo_stages = c("juvenile", "adult"),
  location = "./Data files/2x2_files/simulations/",
  split = 0.5, offset = 0.2, bias = "high")

################################################################################
#### ALL NON-RANDOM SIMULATIONS 3x3: low non random, high non random ####

#### Import matrices ####

load("./Data files/3x3_files/input_files/threebythree_matrices.RData")
# get list of all baseline data files
filenames <- list.files("./Data files/3x3_files/simulations", 
                        pattern = "baseline_simulation_observations",
                        full.names = TRUE)[-c(1,102,203,304,405)]

# name each matrix

names(matrices_33) <- c("mat1",
                        "mat2",
                        "mat3",
                        "mat4",
                        "mat5")

#### Create simulated data: HIGH NON RANDOM ####
create_scenario_data_not_random(
                     baseline = as.list(filenames[1:100]),
                     parameters = matrices_33[["mat1"]],
                     name = "mat1", recapture = c(1,1,1),
                     missing = c(0.7,0.7,0.7),
                     phi = c(matrices_33[["mat1"]][2,1],
                             matrices_33[["mat1"]][3,2],
                             matrices_33[["mat1"]][3,3]),
                     stages = c("juvenile", "subadult", "adult"),
                     repo_stages = c("subadult", "adult"),
                     location = "./Data files/3x3_files/simulations/",
                     split = 0.5, offset = 0.2, bias = "high")

create_scenario_data_not_random(
                     baseline = as.list(filenames[101:200]),
                     parameters = matrices_33[["mat2"]],
                     name = "mat2", recapture = c(1,1,1),
                     missing = c(0.7,0.7,0.7),
                     phi = c(matrices_33[["mat2"]][2,1],
                             matrices_33[["mat2"]][3,2],
                             matrices_33[["mat2"]][3,3]),
                     stages = c("juvenile", "subadult", "adult"),
                     repo_stages = c("subadult", "adult"),
                     location = "./Data files/3x3_files/simulations/",
                     split = 0.5, offset = 0.2, bias = "high")  

create_scenario_data_not_random(
                     baseline = as.list(filenames[201:300]),
                     parameters = matrices_33[["mat3"]],
                     name = "mat3", recapture = c(1,1,1),
                     missing = c(0.7,0.7,0.7),
                     phi = c(matrices_33[["mat3"]][2,1],
                             matrices_33[["mat3"]][3,2],
                             matrices_33[["mat3"]][3,3]),
                     stages = c("juvenile", "subadult", "adult"),
                     repo_stages = c("subadult", "adult"),
                     location = "./Data files/3x3_files/simulations/",
                     split = 0.5, offset = 0.2, bias = "high") 

create_scenario_data_not_random(
                     baseline = as.list(filenames[301:400]),
                     parameters = matrices_33[["mat4"]],
                     name = "mat4", recapture = c(1,1,1),
                     missing = c(0.7,0.7,0.7),
                     phi = c(matrices_33[["mat4"]][2,1],
                             matrices_33[["mat4"]][3,2],
                             matrices_33[["mat4"]][3,3]),
                     stages = c("juvenile", "subadult", "adult"),
                     repo_stages = c("subadult", "adult"),
                     location = "./Data files/3x3_files/simulations/",
                     split = 0.5, offset = 0.2, bias = "high") 

create_scenario_data_not_random(
                     baseline = as.list(filenames[401:500]),
                     parameters = matrices_33[["mat5"]],
                     name = "mat5", recapture = c(1,1,1),
                     missing = c(0.7,0.7,0.7),
                     phi = c(matrices_33[["mat5"]][2,1],
                             matrices_33[["mat5"]][3,2],
                             matrices_33[["mat5"]][3,3]),
                     stages = c("juvenile", "subadult", "adult"),
                     repo_stages = c("subadult", "adult"),
                     location = "./Data files/3x3_files/simulations/",
                     split = 0.5, offset = 0.2, bias = "high")

#### Create simulated data: LOW NON RANDOM ####
create_scenario_data_not_random(
  baseline = as.list(filenames[1:100]),
  parameters = matrices_33[["mat1"]],
  name = "mat1", recapture = c(1,1,1),
  missing = c(0.7,0.7,0.7),
  phi = c(matrices_33[["mat1"]][2,1],
          matrices_33[["mat1"]][3,2],
          matrices_33[["mat1"]][3,3]),
  stages = c("juvenile", "subadult", "adult"),
  repo_stages = c("subadult", "adult"),
  location = "./Data files/3x3_files/simulations/",
  split = 0.5, offset = 0.2, bias = "low")

create_scenario_data_not_random(
  baseline = as.list(filenames[101:200]),
  parameters = matrices_33[["mat2"]],
  name = "mat2", recapture = c(1,1,1),
  missing = c(0.7,0.7,0.7),
  phi = c(matrices_33[["mat2"]][2,1],
          matrices_33[["mat2"]][3,2],
          matrices_33[["mat2"]][3,3]),
  stages = c("juvenile", "subadult", "adult"),
  repo_stages = c("subadult", "adult"),
  location = "./Data files/3x3_files/simulations/",
  split = 0.5, offset = 0.2, bias = "low")  

create_scenario_data_not_random(
  baseline = as.list(filenames[201:300]),
  parameters = matrices_33[["mat3"]],
  name = "mat3", recapture = c(1,1,1),
  missing = c(0.7,0.7,0.7),
  phi = c(matrices_33[["mat3"]][2,1],
          matrices_33[["mat3"]][3,2],
          matrices_33[["mat3"]][3,3]),
  stages = c("juvenile", "subadult", "adult"),
  repo_stages = c("subadult", "adult"),
  location = "./Data files/3x3_files/simulations/",
  split = 0.5, offset = 0.2, bias = "low") 

create_scenario_data_not_random(
  baseline = as.list(filenames[301:400]),
  parameters = matrices_33[["mat4"]],
  name = "mat4", recapture = c(1,1,1),
  missing = c(0.7,0.7,0.7),
  phi = c(matrices_33[["mat4"]][2,1],
          matrices_33[["mat4"]][3,2],
          matrices_33[["mat4"]][3,3]),
  stages = c("juvenile", "subadult", "adult"),
  repo_stages = c("subadult", "adult"),
  location = "./Data files/3x3_files/simulations/",
  split = 0.5, offset = 0.2, bias = "low") 

create_scenario_data_not_random(
  baseline = as.list(filenames[401:500]),
  parameters = matrices_33[["mat5"]],
  name = "mat5", recapture = c(1,1,1),
  missing = c(0.7,0.7,0.7),
  phi = c(matrices_33[["mat5"]][2,1],
          matrices_33[["mat5"]][3,2],
          matrices_33[["mat5"]][3,3]),
  stages = c("juvenile", "subadult", "adult"),
  repo_stages = c("subadult", "adult"),
  location = "./Data files/3x3_files/simulations/",
  split = 0.5, offset = 0.2, bias = "low")

################################################################################
#### Split simulations for faster programming ####


# get filenames

filenames <- list.files("./Data files/3x3_files/simulations", 
                        pattern = "not_random_missing",
                        full.names = TRUE)

map(.x = filenames, 
    split_simulations,
    location = "./Data files/3x3_files/simulations/")

# get filenames

filenames <- list.files("./Data files/2x2_files/simulations", 
                        pattern = "not_random_missing",
                        full.names = TRUE)

map(.x = filenames, 
    split_simulations,
    location = "./Data files/2x2_files/simulations/")

