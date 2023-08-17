#### T1.1: Script to join results files and make summaries
# both Bayesian model results and stage-fate estimates

################################################################################

#### Set up ####

# load packages

library(tidyverse)
library(nimble)
library(MCMCvis)

# load scripts

source("./Functions/summarise_summary_function.R")
source("./Scripts/theme_script.R")

# load results and 'true' matrices

#### 2x2 ####

## get filenames for all 2x2

load("./Data files/2x2/twobytwo_matrices.RData")
names(output_matrices) <- c("mat1", "mat2", "mat3", "mat4", "mat5")

filenames <- data.frame(filename = c(list.files("./Data files/2x2/a_A_missing/", 
                                                full.names = TRUE),
                                     list.files("./Data files/2x2/a_Baseline/", 
                                  full.names = TRUE),
                                  list.files("./Data files/2x2/a_J_missing/", 
                                             full.names = TRUE),
               list.files("./Data files/2x2/a_R_error/", 
                          full.names = TRUE),
               list.files("./Data files/2x2/a_R_missing/", 
                          full.names = TRUE))) %>%
  mutate(matrix_number = as.numeric(str_sub(filename, -7, -7)))

################################################################################

true_parameters_22 <- data.frame(parameter = rep(c("reproduction_juvenile",
                                            "reproduction_adult",
                                            "recapture_juvenile",
                                            "recapture_adult",
                                            "survival_juvenile",
                                            "survival_adult",
                                            "lambda"), 5),
                              value = rep(c(output_matrices[["mat1"]][1,1],
                                        output_matrices[["mat1"]][1,2],
                                        1, 0.7, 
                                        output_matrices[["mat1"]][2,1],
                                        output_matrices[["mat1"]][2,2],
                                        as.numeric(eigen(output_matrices[["mat1"]])$values[1]),
                                        output_matrices[["mat2"]][1,1],
                                        output_matrices[["mat2"]][1,2],
                                        1, 0.7,
                                        output_matrices[["mat2"]][2,1],
                                        output_matrices[["mat2"]][2,2],
                                        as.numeric(eigen(output_matrices[["mat2"]])$values[1]),
                                        output_matrices[["mat3"]][1,1],
                                        output_matrices[["mat3"]][1,2],
                                        1, 0.7,
                                        output_matrices[["mat3"]][2,1],
                                        output_matrices[["mat3"]][2,2],
                                        as.numeric(eigen(output_matrices[["mat3"]])$values[1]),
                                        output_matrices[["mat4"]][1,1],
                                        output_matrices[["mat4"]][1,2],
                                        1, 0.7,
                                        output_matrices[["mat4"]][2,1],
                                        output_matrices[["mat4"]][2,2],
                                        as.numeric(eigen(output_matrices[["mat4"]])$values[1]),
                                        output_matrices[["mat5"]][1,1],
                                        output_matrices[["mat5"]][1,2],
                                        1, 0.7,
                                        output_matrices[["mat5"]][2,1],
                                        output_matrices[["mat5"]][2,2],
                                        as.numeric(eigen(output_matrices[["mat5"]])$values[1])),
                                        5),
                              matrix_number = rep(rep(1:5, each = 7),5),
                              scenario = rep(c("adult_missing",
                                               "baseline",
                                               "juvenile_missing",
                                               "random_error",
                                               "random_missing"), each = 7*5)) %>%
  pivot_wider(values_from = value, names_from = parameter) %>%
  mutate(recapture_juvenile = rep(c(1,1,0.7,1,0.7), each = 5),
         recapture_adult = rep(c(0.7,1,1,1,0.7), each = 5)) %>% 
  group_by(scenario) %>% # split true parameters by scenario
  group_split()

# now join all of the true data to a filename using map

# set up index list for filenames 
filenames_index <- list(c(1:500),
                          c(501:1000),
                          c(1001:1500),
                          c(1501:2000),
                          c(2001:2500))

test <- map2_df(.x = true_parameters_22,
               .y = filenames_index, ~{
  
  # join together the filenames and the true parameters by matrix number
  left_join(filenames[.y,], .x)[,-3] # remove scenario column
  
}) %>%
  arrange(filenames) %>%
  group_by(filename) %>%
  group_split()

#### Summary ####

## summarise the summaries

i <- as.list(rep(c("adult_missing",
                   "baseline",
                   "juvenile_missing",
                   "random_error",
                   "random_missing"), each = 500))

summary_results <- map2_df(.x = test,
                           .y = i, ~{
                summary_summary(inputs = .x,
                                scenario = .y,
                                stages = c("juvenile","adult"))})

save(summary_results, file = "./Data files/2x2/summary_results_2x2.RData")

#### 3x3 ####
## get filenames for all 3x3

load("./Data files/3x3/threebythree_matrices.RData")
names(matrices_33) <- c("mat1", "mat2", "mat3", "mat4", "mat5")

filenames <- data.frame(filename = c(list.files("./Data files/3x3/a_A_missing/", 
                                                full.names = TRUE),
                                     list.files("./Data files/3x3/a_Baseline/", 
                                                full.names = TRUE),
                                     list.files("./Data files/3x3/a_J_missing/", 
                                                full.names = TRUE),
                                     list.files("./Data files/3x3/a_R_error/", 
                                                full.names = TRUE),
                                     list.files("./Data files/3x3/a_R_missing/", 
                                                full.names = TRUE))) %>%
  mutate(matrix_number = as.numeric(str_sub(filename, -7, -7)))

################################################################################

true_parameters_33 <- data.frame(parameter = rep(c("reproduction_juvenile",
                                                   "reproduction_subadult",
                                                   "reproduction_adult",
                                                   "recapture_juvenile",
                                                   "recapture_subadult",
                                                   "recapture_adult",
                                                   "survival_juvenile",
                                                   "survival_subadult",
                                                   "survival_adult",
                                                   "lambda"), 5),
                                 value = rep(c(matrices_33[["mat1"]][1,1],
                                           matrices_33[["mat1"]][1,2],
                                           matrices_33[["mat1"]][1,3],
                                           1, 0.8, 0.8,
                                           matrices_33[["mat1"]][2,1],
                                           matrices_33[["mat1"]][3,2],
                                           matrices_33[["mat1"]][3,3],
                                           as.numeric(eigen(matrices_33[["mat1"]])$values[1]),
                                           matrices_33[["mat2"]][1,1],
                                           matrices_33[["mat2"]][1,2],
                                           matrices_33[["mat2"]][1,3],
                                           1, 0.8, 0.8,
                                           matrices_33[["mat2"]][2,1],
                                           matrices_33[["mat2"]][3,2],
                                           matrices_33[["mat2"]][3,3],
                                           as.numeric(eigen(matrices_33[["mat2"]])$values[1]),
                                           matrices_33[["mat3"]][1,1],
                                           matrices_33[["mat3"]][1,2],
                                           matrices_33[["mat3"]][1,3],
                                           1, 0.8, 0.8,
                                           matrices_33[["mat3"]][2,1],
                                           matrices_33[["mat3"]][3,2],
                                           matrices_33[["mat3"]][3,3],
                                           as.numeric(eigen(matrices_33[["mat3"]])$values[1]),
                                           matrices_33[["mat4"]][1,1],
                                           matrices_33[["mat4"]][1,2],
                                           matrices_33[["mat4"]][1,3],
                                           1, 0.8, 0.8,
                                           matrices_33[["mat4"]][2,1],
                                           matrices_33[["mat4"]][3,2],
                                           matrices_33[["mat4"]][3,3],
                                           as.numeric(eigen(matrices_33[["mat4"]])$values[1]),
                                           matrices_33[["mat5"]][1,1],
                                           matrices_33[["mat5"]][1,2],
                                           matrices_33[["mat5"]][1,3],
                                           1, 0.8, 0.8,
                                           matrices_33[["mat5"]][2,1],
                                           matrices_33[["mat5"]][3,2],
                                           matrices_33[["mat5"]][3,3],
                                           as.numeric(eigen(matrices_33[["mat5"]])$values[1])),5),
                                 matrix_number = rep(rep(1:5, each = 10),5),
                                 scenario = rep(c("adult_missing",
                                                  "baseline",
                                                  "juvenile_missing",
                                                  "random_error",
                                                  "random_missing"), each = 10*5)) %>%
  pivot_wider(values_from = value, names_from = parameter) %>%
  mutate(recapture_juvenile = rep(c(1,1,0.7,1,0.7), each = 5),
         recapture_subadult = rep(c(0.7,1,1,1,0.7), each = 5),
         recapture_adult = rep(c(0.7,1,1,1,0.7), each = 5)) %>% 
  group_by(scenario) %>% # split true parameters by scenario
  group_split()

# now join all of the true data to a filename using map

# set up index list for filenames 
filenames_index <- list(c(1:500),
                        c(501:1000),
                        c(1001:1500),
                        c(1501:2000),
                        c(2001:2500))

test <- map2_df(.x = true_parameters_33,
                .y = filenames_index, ~{
                  # join together the filenames and the true parameters by matrix number
                  left_join(filenames[.y,], .x)[,-3] # remove scenario column
                }) %>%
  arrange(filenames) %>%
  group_by(filename) %>%
  group_split()

#### Summary ####

## summarise the summaries

i <- as.list(rep(c("adult_missing",
                   "baseline",
                   "juvenile_missing",
                   "random_error",
                   "random_missing"), each = 500))

summary_results <- map2_df(.x = test,
                           .y = i, ~{
                             summary_summary(inputs = .x,
                                             scenario = .y,
                                             stages = c("juvenile",
                                                        "subadult","adult"))})

save(summary_results, file = "./Data files/3x3/summary_results_3x3.RData")

#### 5x5 ####
## get filenames for all 5x5

load("./Data files/5x5/fivebyfive_matrices.RData")
names(matrices_55) <- c("mat1", "mat2", "mat3", "mat4", "mat5")

filenames <- data.frame(filename = c(list.files("./Data files/5x5/a_A_missing/", 
                                                full.names = TRUE),
                                     list.files("./Data files/5x5/a_Baseline/", 
                                                full.names = TRUE),
                                     list.files("./Data files/5x5/a_J_missing/", 
                                                full.names = TRUE),
                                     list.files("./Data files/5x5/a_R_error/", 
                                                full.names = TRUE),
                                     list.files("./Data files/5x5/a_R_missing/", 
                                                full.names = TRUE))) %>%
  mutate(matrix_number = as.numeric(str_sub(filename, -7, -7)))

################################################################################

true_parameters_55 <- data.frame(parameter = rep(c("reproduction_juvenile",
                                                   "reproduction_subadult",
                                                   "reproduction_adult1",
                                                   "reproduction_adult2",
                                                   "reproduction_adult3",
                                                   "recapture_juvenile",
                                                   "recapture_subadult",
                                                   "recapture_adult1",
                                                   "recapture_adult2",
                                                   "recapture_adult3",
                                                   "survival_juvenile",
                                                   "survival_subadult",
                                                   "survival_adult1",
                                                   "survival_adult2",
                                                   "survival_adult3",
                                                   "lambda"), 5),
                                 value = rep(c(matrices_55[["mat1"]][1,1],
                                           matrices_55[["mat1"]][1,2],
                                           matrices_55[["mat1"]][1,3],
                                           matrices_55[["mat1"]][1,4],
                                           matrices_55[["mat1"]][1,5],
                                           1, 0.8, 0.8, 0.8, 0.8,
                                           matrices_55[["mat1"]][2,1],
                                           matrices_55[["mat1"]][3,2],
                                           matrices_55[["mat1"]][4,3],
                                           matrices_55[["mat1"]][5,4],
                                           matrices_55[["mat1"]][5,5],
                                           as.numeric(eigen(matrices_55[["mat1"]])$values[1]),
                                           matrices_55[["mat2"]][1,1],
                                           matrices_55[["mat2"]][1,2],
                                           matrices_55[["mat2"]][1,3],
                                           matrices_55[["mat2"]][1,4],
                                           matrices_55[["mat2"]][1,5],
                                           1, 0.8, 0.8, 0.8, 0.8,
                                           matrices_55[["mat2"]][2,1],
                                           matrices_55[["mat2"]][3,2],
                                           matrices_55[["mat2"]][4,3],
                                           matrices_55[["mat2"]][5,4],
                                           matrices_55[["mat2"]][5,5],
                                           as.numeric(eigen(matrices_55[["mat2"]])$values[1]),
                                           matrices_55[["mat3"]][1,1],
                                           matrices_55[["mat3"]][1,2],
                                           matrices_55[["mat3"]][1,3],
                                           matrices_55[["mat3"]][1,4],
                                           matrices_55[["mat3"]][1,5],
                                           1, 0.8, 0.8, 0.8, 0.8,
                                           matrices_55[["mat3"]][2,1],
                                           matrices_55[["mat3"]][3,2],
                                           matrices_55[["mat3"]][4,3],
                                           matrices_55[["mat3"]][5,4],
                                           matrices_55[["mat3"]][5,5],
                                           as.numeric(eigen(matrices_55[["mat3"]])$values[1]),
                                           matrices_55[["mat4"]][1,1],
                                           matrices_55[["mat4"]][1,2],
                                           matrices_55[["mat4"]][1,3],
                                           matrices_55[["mat4"]][1,4],
                                           matrices_55[["mat4"]][1,5],
                                           1, 0.8, 0.8, 0.8, 0.8,
                                           matrices_55[["mat4"]][2,1],
                                           matrices_55[["mat4"]][3,2],
                                           matrices_55[["mat4"]][4,3],
                                           matrices_55[["mat4"]][5,4],
                                           matrices_55[["mat4"]][5,5],
                                           as.numeric(eigen(matrices_55[["mat4"]])$values[1]),
                                           matrices_55[["mat5"]][1,1],
                                           matrices_55[["mat5"]][1,2],
                                           matrices_55[["mat5"]][1,3],
                                           matrices_55[["mat5"]][1,4],
                                           matrices_55[["mat5"]][1,5],
                                           1, 0.8, 0.8, 0.8, 0.8,
                                           matrices_55[["mat5"]][2,1],
                                           matrices_55[["mat5"]][3,2],
                                           matrices_55[["mat5"]][4,3],
                                           matrices_55[["mat5"]][5,4],
                                           matrices_55[["mat5"]][5,5],
                                           as.numeric(eigen(matrices_55[["mat5"]])$values[1])),5),
                                 matrix_number = rep(rep(1:5, each = 16),5),
                                 scenario = rep(c("adult_missing",
                                                  "baseline",
                                                  "juvenile_missing",
                                                  "random_error",
                                                  "random_missing"), each = 16*5)) %>%
  pivot_wider(values_from = value, names_from = parameter) %>%
  mutate(recapture_juvenile = rep(c(1,1,0.7,1,0.7), each = 5),
         recapture_subadult = rep(c(0.7,1,1,1,0.7), each = 5),
         recapture_adult1 = rep(c(0.7,1,1,1,0.7), each = 5),
         recapture_adult2 = rep(c(0.7,1,1,1,0.7), each = 5),
         recapture_adult3 = rep(c(0.7,1,1,1,0.7), each = 5)) %>% 
  group_by(scenario) %>% # split true parameters by scenario
  group_split()

# set up index list for filenames 
filenames_index <- list(c(1:500),
                        c(501:1000),
                        c(1001:1500),
                        c(1501:2000),
                        c(2001:2500))

test <- map2_df(.x = true_parameters_55,
                .y = filenames_index, ~{
                  # join together the filenames and the true parameters by matrix number
                  left_join(filenames[.y,], .x)[,-3] # remove scenario column
                }) %>%
  arrange(filenames) %>%
  group_by(filename) %>%
  group_split()

#### Summary ####

## summarise the summaries

i <- as.list(rep(c("adult_missing",
                   "baseline",
                   "juvenile_missing",
                   "random_error",
                   "random_missing"), each = 500))

summary_results <- map2_df(.x = test,
                           .y = i, ~{
                             summary_summary(inputs = .x,
                                             scenario = .y,
                                             stages = c("juvenile", "subadult",
                                                        "adult1", "adult2", 
                                                        "adult3"))})

save(summary_results, file = "./Data files/5x5/summary_results_5x5.RData")

################################################################################
#### STAGE-FATE RESULTS ####
################################################################################
#### Set up: 2x2####

# load packages

library(tidyverse)

# load scripts

source("./Functions/summarise_summary_function.R")
source("./Scripts/theme_script.R")

# load results and 'true' matrices

load("./Data files/2x2/twobytwo_matrices.RData")
names(output_matrices) <- c("mat1", "mat2", "mat3", "mat4", "mat5")

# import all results and combine
load("./Data files/2x2/baseline_results_2x2.RData")

baseline_results <- baseline_results_2x2 %>% bind_rows() %>%
  mutate(scenario = "baseline",
         simulation = rep(rep(1:100, each = 5),5),
         matrix_number = rep(c(1:5),
                             each = 500)) %>%
  mutate(parameter = case_when(parameter == "s_adult" ~ "survival_adult",
                               parameter == "s_juvenile" ~ "survival_juvenile",
                               parameter == "f_adult" ~ "reproduction_adult",
                               parameter == "f_juvenile" ~ "reproduction_juvenile",
                               TRUE ~ parameter)) 

# remove attributes from these columns to help later
attr(baseline_results$lower_ci, "names") <- NULL
attr(baseline_results$upper_ci, "names") <- NULL

load("./Data files/2x2/r_error_results_2x2.RData")

r_error_results <- r_error_results_2x2 %>% bind_rows() %>%
  mutate(scenario = "r_error",
         simulation = rep(rep(1:100, each = 5),5),
         matrix_number = rep(c(1:5),
                             each = 500)) %>%
  mutate(parameter = case_when(parameter == "s_adult" ~ "survival_adult",
                               parameter == "s_juvenile" ~ "survival_juvenile",
                               parameter == "f_adult" ~ "reproduction_adult",
                               parameter == "f_juvenile" ~ "reproduction_juvenile",
                               TRUE ~ parameter)) 

# remove attributes from these columns to help later
attr(r_error_results$lower_ci, "names") <- NULL
attr(r_error_results$upper_ci, "names") <- NULL

load("./Data files/2x2/r_missing_results_2x2.RData")

r_missing_results <- r_missing_results_2x2 %>% bind_rows() %>%
  mutate(scenario = "r_missing",
         simulation = rep(rep(1:100, each = 5),5),
         matrix_number = rep(c(1:5),
                             each = 500)) %>%
  mutate(parameter = case_when(parameter == "s_adult" ~ "survival_adult",
                               parameter == "s_juvenile" ~ "survival_juvenile",
                               parameter == "f_adult" ~ "reproduction_adult",
                               parameter == "f_juvenile" ~ "reproduction_juvenile",
                               TRUE ~ parameter)) 

# remove attributes from these columns to help later
attr(r_missing_results$lower_ci, "names") <- NULL
attr(r_missing_results$upper_ci, "names") <- NULL

load("./Data files/2x2/j_missing_results_2x2.RData")

j_missing_results <- j_missing_results_2x2 %>% bind_rows() %>%
  mutate(scenario = "j_missing",
         simulation = rep(rep(1:100, each = 5),5),
         matrix_number = rep(c(1:5),
                             each = 500)) %>%
  mutate(parameter = case_when(parameter == "s_adult" ~ "survival_adult",
                               parameter == "s_juvenile" ~ "survival_juvenile",
                               parameter == "f_adult" ~ "reproduction_adult",
                               parameter == "f_juvenile" ~ "reproduction_juvenile",
                               TRUE ~ parameter)) 

# remove attributes from these columns to help later
attr(j_missing_results$lower_ci, "names") <- NULL
attr(j_missing_results$upper_ci, "names") <- NULL

load("./Data files/2x2/a_missing_results_2x2.RData")

a_missing_results <- a_missing_results_2x2 %>% bind_rows() %>%
  mutate(scenario = "a_missing",
         simulation = rep(rep(1:100, each = 5),5),
         matrix_number = rep(c(1:5),
                             each = 500)) %>%
  mutate(parameter = case_when(parameter == "s_adult" ~ "survival_adult",
                               parameter == "s_juvenile" ~ "survival_juvenile",
                               parameter == "f_adult" ~ "reproduction_adult",
                               parameter == "f_juvenile" ~ "reproduction_juvenile",
                               TRUE ~ parameter)) 

# remove attributes from these columns to help later
attr(a_missing_results$lower_ci, "names") <- NULL
attr(a_missing_results$upper_ci, "names") <- NULL

#### combine all results files

results_all <- bind_rows(baseline_results, r_error_results,
                         r_missing_results, j_missing_results,
                         a_missing_results)

true_parameters_22 <- data.frame(parameter = rep(c("reproduction_juvenile",
                                                   "reproduction_adult",
                                                   "survival_juvenile",
                                                   "survival_adult",
                                                   "lambda"), 5),
                                 value = c(output_matrices[["mat1"]][1,1],
                                           output_matrices[["mat1"]][1,2],
                                           output_matrices[["mat1"]][2,1],
                                           output_matrices[["mat1"]][2,2],
                                           as.numeric(eigen(output_matrices[["mat1"]])$values[1]),
                                           output_matrices[["mat2"]][1,1],
                                           output_matrices[["mat2"]][1,2],
                                           output_matrices[["mat2"]][2,1],
                                           output_matrices[["mat2"]][2,2],
                                           as.numeric(eigen(output_matrices[["mat2"]])$values[1]),
                                           output_matrices[["mat3"]][1,1],
                                           output_matrices[["mat3"]][1,2],
                                           output_matrices[["mat3"]][2,1],
                                           output_matrices[["mat3"]][2,2],
                                           as.numeric(eigen(output_matrices[["mat3"]])$values[1]),
                                           output_matrices[["mat4"]][1,1],
                                           output_matrices[["mat4"]][1,2],
                                           output_matrices[["mat4"]][2,1],
                                           output_matrices[["mat4"]][2,2],
                                           as.numeric(eigen(output_matrices[["mat4"]])$values[1]),
                                           output_matrices[["mat5"]][1,1],
                                           output_matrices[["mat5"]][1,2],
                                           output_matrices[["mat5"]][2,1],
                                           output_matrices[["mat5"]][2,2],
                                           as.numeric(eigen(output_matrices[["mat5"]])$values[1])),
                                 matrix_number = rep(1:5, each = 5)) 

full_data <- left_join(results_all, true_parameters_22) %>%
  mutate(error = estimate - value,
         CI_width = upper_ci - lower_ci,
         check1 = value > lower_ci,
         check2 = value < upper_ci,
         true_in_ci = check1 ==TRUE & check2 == TRUE) %>%
  dplyr::mutate(scenario = as.factor(scenario),
                scenario = plyr::revalue(scenario, 
                                         c("baseline"="baseline", 
                                           "a_missing"="adult \nmissing",
                                           "j_missing" = "juvenile \nmissing",
                                           "r_error" = "random \nerror",
                                           "r_missing" = 
                                             "random \nmissing")),
                scenario = fct_relevel(scenario, 
                                       "baseline",
                                       "random \nerror",
                                       "random \nmissing",
                                       "adult \nmissing",
                                       "juvenile \nmissing"))

# save out full data

write.csv(full_data, "./Data files/2x2/full_direct_est_results_2x2.csv")

################################################################################

#### Set up: 3x3 ####

# load packages

library(tidyverse)

# load scripts

source("./Functions/summarise_summary_function.R")
source("./Scripts/theme_script.R")

colours <- c("#FFFFFF", "#feff54", "#30666B", "#134263", "#1E2E39")

# load results and 'true' matrices

load("./Data files/3x3/threebythree_matrices.RData")
names(matrices_33) <- c("mat1", "mat2", "mat3", "mat4", "mat5")

# import all results and combine
load("./Data files/3x3/baseline_results_3x3.RData")

baseline_results <- baseline_results_3x3 %>% bind_rows() %>%
  mutate(scenario = "baseline",
         simulation = rep(rep(1:100, each = 7),5),
         matrix_number = rep(c(1:5),
                             each = 700))

# remove attributes from these columns to help later
attr(baseline_results$lower_ci, "names") <- NULL
attr(baseline_results$upper_ci, "names") <- NULL

load("./Data files/3x3/r_error_results_3x3.RData")

r_error_results <- r_error_results_3x3 %>% bind_rows() %>%
  mutate(scenario = "r_error",
         simulation = rep(rep(1:100, each = 7),5),
         matrix_number = rep(c(1:5),
                             each = 700)) 

# remove attributes from these columns to help later
attr(r_error_results$lower_ci, "names") <- NULL
attr(r_error_results$upper_ci, "names") <- NULL

load("./Data files/3x3/r_missing_results_3x3.RData")

r_missing_results <- r_missing_results_3x3 %>% bind_rows() %>%
  mutate(scenario = "r_missing",
         simulation = rep(rep(1:100, each = 7),5),
         matrix_number = rep(c(1:5),
                             each = 700))

# remove attributes from these columns to help later
attr(r_missing_results$lower_ci, "names") <- NULL
attr(r_missing_results$upper_ci, "names") <- NULL

load("./Data files/3x3/j_missing_results_3x3.RData")

j_missing_results <- j_missing_results_3x3 %>% bind_rows() %>%
  mutate(scenario = "j_missing",
         simulation = rep(rep(1:100, each = 7),5),
         matrix_number = rep(c(1:5),
                             each = 700))

# remove attributes from these columns to help later
attr(j_missing_results$lower_ci, "names") <- NULL
attr(j_missing_results$upper_ci, "names") <- NULL

load("./Data files/3x3/a_missing_results_3x3.RData")

a_missing_results <- a_missing_results_3x3 %>% bind_rows() %>%
  mutate(scenario = "a_missing",
         simulation = rep(rep(1:100, each = 7),5),
         matrix_number = rep(c(1:5),
                             each = 700)) 

# remove attributes from these columns to help later
attr(a_missing_results$lower_ci, "names") <- NULL
attr(a_missing_results$upper_ci, "names") <- NULL

#### combine all results files

results_all <- bind_rows(baseline_results, r_error_results,
                         r_missing_results, j_missing_results,
                         a_missing_results)

true_parameters_33 <- data.frame(parameter = rep(c("reproduction_juvenile",
                                                   "reproduction_subadult",
                                                   "reproduction_adult",
                                                   "recapture_juvenile",
                                                   "recapture_subadult",
                                                   "recapture_adult",
                                                   "survival_juvenile",
                                                   "survival_subadult",
                                                   "survival_adult",
                                                   "lambda"), 5),
                                 value = c(matrices_33[["mat1"]][1,1],
                                           matrices_33[["mat1"]][1,2],
                                           matrices_33[["mat1"]][1,3],
                                           1, 0.8, 0.8,
                                           matrices_33[["mat1"]][2,1],
                                           matrices_33[["mat1"]][3,2],
                                           matrices_33[["mat1"]][3,3],
                                           as.numeric(eigen(matrices_33[["mat1"]])$values[1]),
                                           matrices_33[["mat2"]][1,1],
                                           matrices_33[["mat2"]][1,2],
                                           matrices_33[["mat2"]][1,3],
                                           1, 0.8, 0.8,
                                           matrices_33[["mat2"]][2,1],
                                           matrices_33[["mat2"]][3,2],
                                           matrices_33[["mat2"]][3,3],
                                           as.numeric(eigen(matrices_33[["mat2"]])$values[1]),
                                           matrices_33[["mat3"]][1,1],
                                           matrices_33[["mat3"]][1,2],
                                           matrices_33[["mat3"]][1,3],
                                           1, 0.8, 0.8,
                                           matrices_33[["mat3"]][2,1],
                                           matrices_33[["mat3"]][3,2],
                                           matrices_33[["mat3"]][3,3],
                                           as.numeric(eigen(matrices_33[["mat3"]])$values[1]),
                                           matrices_33[["mat4"]][1,1],
                                           matrices_33[["mat4"]][1,2],
                                           matrices_33[["mat4"]][1,3],
                                           1, 0.8, 0.8,
                                           matrices_33[["mat4"]][2,1],
                                           matrices_33[["mat4"]][3,2],
                                           matrices_33[["mat4"]][3,3],
                                           as.numeric(eigen(matrices_33[["mat4"]])$values[1]),
                                           matrices_33[["mat5"]][1,1],
                                           matrices_33[["mat5"]][1,2],
                                           matrices_33[["mat5"]][1,3],
                                           1, 0.8, 0.8,
                                           matrices_33[["mat5"]][2,1],
                                           matrices_33[["mat5"]][3,2],
                                           matrices_33[["mat5"]][3,3],
                                           as.numeric(eigen(matrices_33[["mat5"]])$values[1])),
                                 matrix_number = rep(1:5, each = 10))

full_data <- left_join(results_all, true_parameters_33) %>%
  mutate(error = estimate - value,
         CI_width = upper_ci - lower_ci,
         check1 = value > lower_ci,
         check2 = value < upper_ci,
         true_in_ci = check1 ==TRUE & check2 == TRUE) %>%
  dplyr::mutate(scenario = as.factor(scenario),
                scenario = plyr::revalue(scenario, 
                                         c("baseline"="baseline", 
                                           "a_missing"="adult \nmissing",
                                           "j_missing" = "juvenile \nmissing",
                                           "r_error" = "random \nerror",
                                           "r_missing" = 
                                             "random \nmissing")),
                scenario = fct_relevel(scenario, 
                                       "baseline",
                                       "random \nerror",
                                       "random \nmissing",
                                       "adult \nmissing",
                                       "juvenile \nmissing"))

# save out full data

write.csv(full_data, "./Data files/3x3/full_direct_est_results_3x3.csv")

################################################################################

load("./Data files/5x5/fivebyfive_matrices.RData")
names(output_matrices) <- c("mat1", "mat2", "mat3", "mat4", "mat5")

#### Set up: 5x5 ####

# load packages

library(tidyverse)

# load scripts

source("./Functions/summarise_summary_function.R")
source("./Scripts/theme_script.R")

colours <- c("#FFFFFF", "#feff54", "#30666B", "#134263", "#1E2E39")

# load results and 'true' matrices

load("./Data files/5x5/fivebyfive_matrices.RData")
names(matrices_55) <- c("mat1", "mat2", "mat3", "mat4", "mat5")

# import all results and combine
load("./Data files/5x5/baseline_results_5x5.RData")

baseline_results <- baseline_results_5x5 %>% bind_rows() %>%
  mutate(scenario = "baseline",
         simulation = rep(rep(1:100, each = 11),5),
         matrix_number = rep(c(1:5),
                             each = 1100))

# remove attributes from these columns to help later
attr(baseline_results$lower_ci, "names") <- NULL
attr(baseline_results$upper_ci, "names") <- NULL

load("./Data files/5x5/r_error_results_5x5.RData")

r_error_results <- r_error_results_5x5 %>% bind_rows() %>%
  mutate(scenario = "r_error",
         simulation = rep(rep(1:100, each = 11),5),
         matrix_number = rep(c(1:5),
                             each = 1100)) 

# remove attributes from these columns to help later
attr(r_error_results$lower_ci, "names") <- NULL
attr(r_error_results$upper_ci, "names") <- NULL

load("./Data files/5x5/r_missing_results_5x5.RData")

r_missing_results <- r_missing_results_5x5 %>% bind_rows() %>%
  mutate(scenario = "r_missing",
         simulation = rep(rep(1:100, each = 11),5),
         matrix_number = rep(c(1:5),
                             each = 1100))

# remove attributes from these columns to help later
attr(r_missing_results$lower_ci, "names") <- NULL
attr(r_missing_results$upper_ci, "names") <- NULL

load("./Data files/5x5/j_missing_results_5x5.RData")

j_missing_results <- j_missing_results_5x5 %>% bind_rows() %>%
  mutate(scenario = "j_missing",
         simulation = rep(rep(1:100, each = 11),5),
         matrix_number = rep(c(1:5),
                             each = 1100))

# remove attributes from these columns to help later
attr(j_missing_results$lower_ci, "names") <- NULL
attr(j_missing_results$upper_ci, "names") <- NULL

load("./Data files/5x5/a_missing_results_5x5.RData")

a_missing_results <- a_missing_results_5x5 %>% bind_rows() %>%
  mutate(scenario = "a_missing",
         simulation = rep(rep(1:100, each = 11),5),
         matrix_number = rep(c(1:5),
                             each = 1100)) 

# remove attributes from these columns to help later
attr(a_missing_results$lower_ci, "names") <- NULL
attr(a_missing_results$upper_ci, "names") <- NULL

#### combine all results files

results_all <- bind_rows(baseline_results, r_error_results,
                         r_missing_results, j_missing_results,
                         a_missing_results)

true_parameters_55 <- data.frame(parameter = rep(c("reproduction_juvenile",
                                                   "reproduction_subadult",
                                                   "reproduction_adult1",
                                                   "reproduction_adult2",
                                                   "reproduction_adult3",
                                                   "recapture_juvenile",
                                                   "recapture_subadult",
                                                   "recapture_adult1",
                                                   "recapture_adult2",
                                                   "recapture_adult3",
                                                   "survival_juvenile",
                                                   "survival_subadult",
                                                   "survival_adult1",
                                                   "survival_adult2",
                                                   "survival_adult3",
                                                   "lambda"), 5),
                                 value = c(matrices_55[["mat1"]][1,1],
                                           matrices_55[["mat1"]][1,2],
                                           matrices_55[["mat1"]][1,3],
                                           matrices_55[["mat1"]][1,4],
                                           matrices_55[["mat1"]][1,5],
                                           1, 0.8, 0.8, 0.8, 0.8,
                                           matrices_55[["mat1"]][2,1],
                                           matrices_55[["mat1"]][3,2],
                                           matrices_55[["mat1"]][4,3],
                                           matrices_55[["mat1"]][5,4],
                                           matrices_55[["mat1"]][5,5],
                                           as.numeric(eigen(matrices_55[["mat1"]])$values[1]),
                                           matrices_55[["mat2"]][1,1],
                                           matrices_55[["mat2"]][1,2],
                                           matrices_55[["mat2"]][1,3],
                                           matrices_55[["mat2"]][1,4],
                                           matrices_55[["mat2"]][1,5],
                                           1, 0.8, 0.8, 0.8, 0.8,
                                           matrices_55[["mat2"]][2,1],
                                           matrices_55[["mat2"]][3,2],
                                           matrices_55[["mat2"]][4,3],
                                           matrices_55[["mat2"]][5,4],
                                           matrices_55[["mat2"]][5,5],
                                           as.numeric(eigen(matrices_55[["mat2"]])$values[1]),
                                           matrices_55[["mat3"]][1,1],
                                           matrices_55[["mat3"]][1,2],
                                           matrices_55[["mat3"]][1,3],
                                           matrices_55[["mat3"]][1,4],
                                           matrices_55[["mat3"]][1,5],
                                           1, 0.8, 0.8, 0.8, 0.8,
                                           matrices_55[["mat3"]][2,1],
                                           matrices_55[["mat3"]][3,2],
                                           matrices_55[["mat3"]][4,3],
                                           matrices_55[["mat3"]][5,4],
                                           matrices_55[["mat3"]][5,5],
                                           as.numeric(eigen(matrices_55[["mat3"]])$values[1]),
                                           matrices_55[["mat4"]][1,1],
                                           matrices_55[["mat4"]][1,2],
                                           matrices_55[["mat4"]][1,3],
                                           matrices_55[["mat4"]][1,4],
                                           matrices_55[["mat4"]][1,5],
                                           1, 0.8, 0.8, 0.8, 0.8,
                                           matrices_55[["mat4"]][2,1],
                                           matrices_55[["mat4"]][3,2],
                                           matrices_55[["mat4"]][4,3],
                                           matrices_55[["mat4"]][5,4],
                                           matrices_55[["mat4"]][5,5],
                                           as.numeric(eigen(matrices_55[["mat4"]])$values[1]),
                                           matrices_55[["mat5"]][1,1],
                                           matrices_55[["mat5"]][1,2],
                                           matrices_55[["mat5"]][1,3],
                                           matrices_55[["mat5"]][1,4],
                                           matrices_55[["mat5"]][1,5],
                                           1, 0.8, 0.8, 0.8, 0.8,
                                           matrices_55[["mat5"]][2,1],
                                           matrices_55[["mat5"]][3,2],
                                           matrices_55[["mat5"]][4,3],
                                           matrices_55[["mat5"]][5,4],
                                           matrices_55[["mat5"]][5,5],
                                           as.numeric(eigen(matrices_55[["mat5"]])$values[1])),
                                 matrix_number = rep(1:5, each = 16))

full_data <- left_join(results_all, true_parameters_55) %>%
  mutate(error = estimate - value,
         CI_width = upper_ci - lower_ci,
         check1 = value > lower_ci,
         check2 = value < upper_ci,
         true_in_ci = check1 ==TRUE & check2 == TRUE) %>%
  dplyr::mutate(scenario = as.factor(scenario),
                scenario = plyr::revalue(scenario, 
                                         c("baseline"="baseline", 
                                           "a_missing"="adult \nmissing",
                                           "j_missing" = "juvenile \nmissing",
                                           "r_error" = "random \nerror",
                                           "r_missing" = 
                                             "random \nmissing")),
                scenario = fct_relevel(scenario, 
                                       "baseline",
                                       "random \nerror",
                                       "random \nmissing",
                                       "adult \nmissing",
                                       "juvenile \nmissing"))

# save out full data

write.csv(full_data, "./Data files/5x5/full_direct_est_results_5x5.csv")

################################################################################
