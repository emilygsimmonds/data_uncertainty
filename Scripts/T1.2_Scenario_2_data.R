#### T1.1: Data simulation for scenario 2: different matrix sizes #

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
# - max_age = maximum age species can get to
# 
# - inc_trait = TRUE or FALSE if you want to include a trait as well
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

#### Set up simulation parameters TEST ####

# set up i

i <- as.list(1:100)

parameter_matrix <- matrix(data = c(0, 0.5, 0.5, 
                              0.3, 0, 0,
                              0, 0.7, 0.7),
                     nrow = 3, byrow = TRUE)

input_data <- map(.x = i, ~{simulation_setup(parameter_matrix = parameter_matrix,
                                             i = .x,
                                             stages = c("juvenile",
                                                        "subadult",
                                                        "adult"))})


# set up recapture probabilities

recapture <- 0.8

# set up IDs

IDs <- 101:200000000

#### TEST 3x3  ####

# run state simulation
output_data <- run_simulation_state(input_data_old = input_data[[1]], 
                                    parameters = parameter_matrix, 
                                    stages = c("juvenile",
                                               "subadult",
                                               "adult"),
                                    inc_trait = FALSE,
                                    start_i = 2, end_i = 5, IDs = IDs)

# checks
mean(output_data$Offspring[output_data$Stage == "juvenile"])
mean(output_data$Offspring[output_data$Stage == "subadult"])
mean(output_data$Offspring[output_data$Stage == "adult"])
mean(output_data$Surv[output_data$Stage == "juvenile"])
mean(output_data$Surv[output_data$Stage == "subadult"])
mean(output_data$Surv[output_data$Stage == "adult"])

# then observation process
observation <- run_observation_process(output_data, 
                                       p = c(1,0.8,0.8),
                                       phi = c(0.3,0.7,0.7),
                                       fecundity_error = TRUE,
                                       seed = 2,
                                       stages = c("juvenile",
                                                  "subadult",
                                                  "adult"))

# number of juveniles = same

length(which(output_data$Stage == "juvenile"))

length(which(observation$Stage == "juvenile"))

# number of adults = reduced to 80%

length(which(observation$Stage == "subadult"))/
  length(which(output_data$Stage == "subadult"))

length(which(observation$Stage == "adult"))/
  length(which(output_data$Stage == "adult"))

# check offspring no = different
observation$Offspring - observation$Offspring_obs

save(observation, file = "./Data files/test.RData")

x <- output_data %>% group_by(Year) %>% summarise(count = n(),
                                                  repro = sum(Offspring))

#### TEST 5x5  ####

# set up i

i <- as.list(1:100)

parameter_matrix <- matrix(data = c(0, 0.5, 0.5, 0.5, 0.5, 
                                    0.3, 0, 0, 0, 0,
                                    0, 0.7, 0, 0, 0,
                                    0, 0, 0.7, 0, 0,
                                    0, 0, 0, 0.7, 0.7),
                           nrow = 5, byrow = TRUE)

input_data <- map(.x = i, ~{simulation_setup(parameter_matrix = parameter_matrix,
                                             i = .x,
                                             stages = c("juvenile",
                                                        "subadult",
                                                        "adult1",
                                                        "adult2",
                                                        "adult3"))})


# set up recapture probabilities

recapture <- 0.8

# set up IDs

IDs <- 101:200000000

# run state simulation
output_data <- run_simulation_state(input_data_old = input_data[[1]], 
                                    parameters = parameter_matrix, 
                                    stages = c("juvenile",
                                               "subadult",
                                               "adult1",
                                               "adult2",
                                               "adult3"),
                                    inc_trait = FALSE,
                                    start_i = 2, end_i = 5, IDs = IDs)

# checks
mean(output_data$Offspring[output_data$Stage == "juvenile"])
mean(output_data$Offspring[output_data$Stage == "subadult"])
mean(output_data$Offspring[output_data$Stage == "adult1"])
mean(output_data$Offspring[output_data$Stage == "adult2"])
mean(output_data$Surv[output_data$Stage == "juvenile"])
mean(output_data$Surv[output_data$Stage == "subadult"])
mean(output_data$Surv[output_data$Stage == "adult1"])

# then observation process
observation <- run_observation_process(output_data, 
                                       p = c(1,0.8,0.8,0.8,0.8),
                                       phi = c(0.3,0.7,0.7,0.7,0.7),
                                       fecundity_error = TRUE,
                                       seed = 2,
                                       stages = c("juvenile",
                                                  "subadult",
                                                  "adult1",
                                                  "adult2",
                                                  "adult3"))

# number of juveniles = same

length(which(output_data$Stage == "juvenile"))

length(which(observation$Stage == "juvenile"))

# number of adults = reduced to 80%

length(which(observation$Stage == "subadult"))/
  length(which(output_data$Stage == "subadult"))

length(which(observation$Stage == "adult1"))/
  length(which(output_data$Stage == "adult1"))

# check offspring no = different
observation$Offspring - observation$Offspring_obs


################################################################################

#### SIMULATIONS 3x3 ####

#### Import matrices ####

load("./Data files/3x3/threebythree_matrices.RData")

# name each matrix

names(matrices_33) <- c("mat1",
                            "mat2",
                            "mat3",
                            "mat4",
                            "mat5")

#### Create simulated data ####

create_scenario_data(parameters = matrices_33[["mat1"]],
                     name = "mat1", recapture = c(1,0.8,0.8),
                     phi = c(matrices_33[["mat1"]][2,1],
                             matrices_33[["mat1"]][3,2],
                             matrices_33[["mat1"]][3,3]),
                     stages = c("juvenile", "subadult", "adult"),
                     repro_stages = c("subadult", "adult"),
                     location = "./Data files/3x3/") 

create_scenario_data(parameters = matrices_33[["mat2"]],
                     name = "mat2", recapture = c(1,0.8,0.8),
                     phi = c(matrices_33[["mat2"]][2,1],
                             matrices_33[["mat2"]][3,2],
                             matrices_33[["mat2"]][3,3]),
                     stages = c("juvenile", "subadult", "adult"),
                     repro_stages = c("subadult", "adult"),
                     location = "./Data files/3x3/")  

create_scenario_data(parameters = matrices_33[["mat3"]],
                     name = "mat3", recapture = c(1,0.8,0.8),
                     phi = c(matrices_33[["mat3"]][2,1],
                             matrices_33[["mat3"]][3,2],
                             matrices_33[["mat3"]][3,3]),
                     stages = c("juvenile", "subadult", "adult"),
                     repro_stages = c("subadult", "adult"),
                     location = "./Data files/3x3/") 

create_scenario_data(parameters = matrices_33[["mat4"]],
                     name = "mat4", recapture = c(1,0.8,0.8),
                     phi = c(matrices_33[["mat4"]][2,1],
                             matrices_33[["mat4"]][3,2],
                             matrices_33[["mat4"]][3,3]),
                     stages = c("juvenile", "subadult", "adult"),
                     repro_stages = c("subadult", "adult"),
                     location = "./Data files/3x3/") 

create_scenario_data(parameters = matrices_33[["mat5"]],
                     name = "mat5", recapture = c(1,0.8,0.8),
                     phi = c(matrices_33[["mat5"]][2,1],
                             matrices_33[["mat5"]][3,2],
                             matrices_33[["mat5"]][3,3]),
                     stages = c("juvenile", "subadult", "adult"),
                     repro_stages = c("subadult", "adult"),
                     location = "./Data files/3x3/") 



################################################################################

#### SIMULATIONS 5x5 ####

#### Import matrices ####

load("./Data files/5x5/fivebyfive_matrices.RData")

# name each matrix

names(matrices_55) <- c("mat1",
                        "mat2",
                        "mat3",
                        "mat4",
                        "mat5")

#### Create simulated data ####

create_scenario_data(parameters = matrices_55[["mat1"]],
                     name = "mat1", recapture = c(1, 0.8, 0.8, 0.8, 0.8),
                     phi = c(matrices_55[["mat1"]][2,1],
                             matrices_55[["mat1"]][3,2],
                             matrices_55[["mat1"]][4,3],
                             matrices_55[["mat1"]][5,4],
                             matrices_55[["mat1"]][5,5]),
                     stages = c("juvenile", "subadult", "adult1",
                                "adult2", "adult3"),
                     repro_stages = c("subadult", "adult1",
                                      "adult2", "adult3"),
                     location = "./Data files/5x5/") 

create_scenario_data(parameters = matrices_55[["mat2"]],
                     name = "mat2", recapture = c(1, 0.8, 0.8, 0.8, 0.8),
                     phi = c(matrices_55[["mat2"]][2,1],
                             matrices_55[["mat2"]][3,2],
                             matrices_55[["mat2"]][4,3],
                             matrices_55[["mat2"]][5,4],
                             matrices_55[["mat2"]][5,5]),
                     stages = c("juvenile", "subadult", "adult1",
                                "adult2", "adult3"),
                     repro_stages = c("subadult", "adult1",
                                      "adult2", "adult3"),
                     location = "./Data files/5x5/") 

create_scenario_data(parameters = matrices_55[["mat3"]],
                     name = "mat3", recapture = c(1, 0.8, 0.8, 0.8, 0.8),
                     phi = c(matrices_55[["mat3"]][2,1],
                             matrices_55[["mat3"]][3,2],
                             matrices_55[["mat3"]][4,3],
                             matrices_55[["mat3"]][5,4],
                             matrices_55[["mat3"]][5,5]),
                     stages = c("juvenile", "subadult", "adult1",
                                "adult2", "adult3"),
                     repro_stages = c("subadult", "adult1",
                                      "adult2", "adult3"),
                     location = "./Data files/5x5/") 

create_scenario_data(parameters = matrices_55[["mat4"]],
                     name = "mat4", recapture = c(1, 0.8, 0.8, 0.8, 0.8),
                     phi = c(matrices_55[["mat4"]][2,1],
                             matrices_55[["mat4"]][3,2],
                             matrices_55[["mat4"]][4,3],
                             matrices_55[["mat4"]][5,4],
                             matrices_55[["mat4"]][5,5]),
                     stages = c("juvenile", "subadult", "adult1",
                                "adult2", "adult3"),
                     repro_stages = c("subadult", "adult1",
                                      "adult2", "adult3"),
                     location = "./Data files/5x5/") 

create_scenario_data(parameters = matrices_55[["mat5"]],
                     name = "mat5", recapture = c(1, 0.8, 0.8, 0.8, 0.8),
                     phi = c(matrices_55[["mat5"]][2,1],
                             matrices_55[["mat5"]][3,2],
                             matrices_55[["mat5"]][4,3],
                             matrices_55[["mat5"]][5,4],
                             matrices_55[["mat5"]][5,5]),
                     stages = c("juvenile", "subadult", "adult1",
                                "adult2", "adult3"),
                     repro_stages = c("subadult", "adult1",
                                      "adult2", "adult3"),
                     location = "./Data files/5x5/") 





################################################################################

#### Split simulations for faster programming ####


#### source function

source("./Functions/split_simulations_function.R")

# get filenames

filenames <- list.files("./Data files/3x3", 
                        pattern = "baseline_simulation_observations",
                        full.names = TRUE)

map(.x = filenames, 
    split_simulations,
    location = "./Data files/3x3/")


filenames <- list.files("./Data files/3x3", 
                        pattern = "random_missing_simulation",
                        full.names = TRUE)

map(.x = filenames, 
    split_simulations,
    location = "./Data files/3x3/")


filenames <- list.files("./Data files/3x3", 
                        pattern = "adult_missing_simulation",
                        full.names = TRUE)

map(.x = filenames, 
    split_simulations,
    location = "./Data files/3x3/")


filenames <- list.files("./Data files/3x3", 
                        pattern = "juvenile_missing_simulation",
                        full.names = TRUE)

map(.x = filenames, 
    split_simulations,
    location = "./Data files/3x3/")

# get filenames

filenames <- list.files("./Data files/5x5", 
                        pattern = "baseline_simulation_observations",
                        full.names = TRUE)

map(.x = filenames, 
    split_simulations,
    location = "./Data files/5x5/")


filenames <- list.files("./Data files/5x5", 
                        pattern = "random_missing_simulation",
                        full.names = TRUE)

map(.x = filenames, 
    split_simulations,
    location = "./Data files/5x5/")


filenames <- list.files("./Data files/5x5", 
                        pattern = "adult_missing_simulation",
                        full.names = TRUE)

map(.x = filenames, 
    split_simulations,
    location = "./Data files/5x5/")


filenames <- list.files("./Data files/5x5", 
                        pattern = "juvenile_missing_simulation",
                        full.names = TRUE)

map(.x = filenames, 
    split_simulations,
    location = "./Data files/5x5/")
