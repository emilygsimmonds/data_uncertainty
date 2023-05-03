# T1.1: Data simulation for scenario 1: bias and error in fecundity #

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

parameters <- matrix(data = c(0.5, 0.5, 0.3, 0.7),
                     nrow = 2, byrow = TRUE)

input_data <- map(.x = i, ~{simulation_setup(parameter_matrix = parameters,
                                             i = .x,
                                             stages = c("juvenile",
                                                        "adult"))})

# set up IDs

IDs <- 101:200000000

#### TEST ####

# run state simulation
output_data <- run_simulation_state(input_data_old = input_data[[3]], 
                              parameters = parameters, 
                              stages = c("juvenile",
                                         "adult"),
                              inc_trait = FALSE,
                              start_i = 2, end_i = 5, IDs = IDs)

# checks
mean(output_data$Offspring[output_data$Stage == "juvenile"])
mean(output_data$Offspring[output_data$Stage == "adult"])
mean(output_data$Surv[output_data$Stage == "juvenile"])
mean(output_data$Surv[output_data$Stage == "adult"])

# then observation process
observation <- run_observation_process(output_data, 
                                       p = c(1*0.7, 1*0.7),
                                       phi = c(0.3, 0.7),
                                       fecundity_error = FALSE,
                                       seed = 2,
                                       stages = c("juvenile",
                                                  "adult"))

# number of juveniles = 0.7

length(which(observation$Stage == "juvenile"))/
  length(which(output_data$Stage == "juvenile"))

# number of adults = reduced to 80%

length(which(observation$Stage == "adult"))/
length(which(output_data$Stage == "adult"))

save(observation, file = "./Data files/test.RData")

x <- output_data %>% group_by(Year) %>% summarise(count = n(),
                                             repro = sum(Offspring))

################################################################################

#### SIMULATIONS 2x2 ####

#### Import matrices ####

load("./Data files/2x2/twobytwo_matrices.RData")

# name each matrix

names(output_matrices) <- c("mat1",
                            "mat2",
                            "mat3",
                            "mat4",
                            "mat5")

#### Create simulated data ####

create_scenario_data(parameters = output_matrices[["mat1"]],
                     name = "mat1", recapture = c(1,1),
                     missing = c(0.7,0.7),
                     phi = c(output_matrices[["mat1"]][2,]),
                    stages = c("juvenile", "adult"),
                    repro_stages = c("juvenile", "adult"),
                    location = "./Data files/2x2/") 

create_scenario_data(parameters = output_matrices[["mat2"]],
                     name = "mat2", recapture = c(1,1),
                     missing = c(0.7,0.7),
                     phi = c(output_matrices[["mat2"]][2,]),
                     stages = c("juvenile", "adult"),
                     repro_stages = c("juvenile", "adult"),
                     location = "./Data files/2x2/") 

create_scenario_data(parameters = output_matrices[["mat3"]],
                     name = "mat3", recapture = c(1,1),
                     missing = c(0.7,0.7),
                     phi = c(output_matrices[["mat3"]][2,]),
                     stages = c("juvenile", "adult"),
                     repro_stages = c("juvenile", "adult"),
                     location = "./Data files/2x2/") 

create_scenario_data(parameters = output_matrices[["mat4"]],
                     name = "mat4", recapture = c(1,1),
                     missing = c(0.7,0.7),
                     phi = c(output_matrices[["mat4"]][2,]),
                     stages = c("juvenile", "adult"),
                     repro_stages = c("juvenile", "adult"),
                     location = "./Data files/2x2/") 

create_scenario_data(parameters = output_matrices[["mat5"]],
                     name = "mat5", recapture = c(1,1),
                     missing = c(0.7,0.7),
                     phi = c(output_matrices[["mat5"]][2,]),
                     stages = c("juvenile", "adult"),
                     repro_stages = c("juvenile", "adult"),
                     location = "./Data files/2x2/") 

################################################################################

#### Split simulations for faster programming ####


#### source function

source("./Functions/split_simulations_function.R")

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


filenames <- list.files("./Data files/2x2", 
                        pattern = "adult_missing_simulation",
                        full.names = TRUE)

map(.x = filenames, 
    split_simulations,
    location = "./Data files/2x2/")


filenames <- list.files("./Data files/2x2", 
                        pattern = "juvenile_missing_simulation",
                        full.names = TRUE)

map(.x = filenames, 
    split_simulations,
    location = "./Data files/2x2/")
