#### T1.1: Analysis script ####

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

filenames <- data.frame(filename = c(list.files("./Data files/2x2/a_Baseline/", 
                                  full.names = TRUE),
               list.files("./Data files/2x2/a_R_error/", 
                          full.names = TRUE),
               list.files("./Data files/2x2/a_R_missing/", 
                          full.names = TRUE),
               list.files("./Data files/2x2/a_A_missing/", 
                          full.names = TRUE),
               list.files("./Data files/2x2/a_J_missing/", 
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
                              value = c(output_matrices[["mat1"]][1,1],
                                        output_matrices[["mat1"]][1,2],
                                        1, 0.8,
                                        output_matrices[["mat1"]][2,1],
                                        output_matrices[["mat1"]][2,2],
                                        as.numeric(eigen(output_matrices[["mat1"]])$values[1]),
                                        output_matrices[["mat2"]][1,1],
                                        output_matrices[["mat2"]][1,2],
                                        1, 0.8,
                                        output_matrices[["mat2"]][2,1],
                                        output_matrices[["mat2"]][2,2],
                                        as.numeric(eigen(output_matrices[["mat2"]])$values[1]),
                                        output_matrices[["mat3"]][1,1],
                                        output_matrices[["mat3"]][1,2],
                                        1, 0.8,
                                        output_matrices[["mat3"]][2,1],
                                        output_matrices[["mat3"]][2,2],
                                        as.numeric(eigen(output_matrices[["mat3"]])$values[1]),
                                        output_matrices[["mat4"]][1,1],
                                        output_matrices[["mat4"]][1,2],
                                        1, 0.8,
                                        output_matrices[["mat4"]][2,1],
                                        output_matrices[["mat4"]][2,2],
                                        as.numeric(eigen(output_matrices[["mat4"]])$values[1]),
                                        output_matrices[["mat5"]][1,1],
                                        output_matrices[["mat5"]][1,2],
                                        1, 0.8,
                                        output_matrices[["mat5"]][2,1],
                                        output_matrices[["mat5"]][2,2],
                                        as.numeric(eigen(output_matrices[["mat5"]])$values[1])),
                              matrix_number = rep(1:5, each = 7)) %>%
  pivot_wider(values_from = value, names_from = parameter)

test <- left_join(filenames, true_parameters_22) %>% 
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

###############################################################################

colours <- c("#FFFFFF", "#feff54", "#30666B", "#134263", "#1E2E39")

# re-order the factor levels
summary_results <- summary_results %>%
  dplyr::mutate(scenario = as.factor(scenario),
                scenario = plyr::revalue(scenario, 
                                                 c("baseline"="baseline", 
                                                   "adult_missing"="adult \nmissing",
                                                   "juvenile_missing" = "juvenile \nmissing",
                                                   "random_error" = "random \nerror",
                                                   "random_missing" = 
                                                     "random \nmissing")),
                scenario = fct_relevel(scenario, 
                                               "baseline",
                                               "random \nerror",
                                               "random \nmissing",
                                               "adult \nmissing",
                                               "juvenile \nmissing"))

#### FIGURE 1: impact on accuracy of lambda ####

# plot error for each scenario

ggplot(data = filter(summary_results, parameter == "lambda"),
       aes(x = scenario,
           y = error, fill = scenario,
           colour = scenario)) +
  geom_violin(scale = "width", draw_quantiles = c(0.025, 0.5, 0.975)) +
  scale_fill_manual(values = colours) +
  scale_color_manual(values = c("black", "grey50", "white", "white", "white"))+
  plain_theme() +
  facet_grid(rows = vars(matrix_number))+
  labs(x = "", y = "Estimate - True") +
  theme(legend.position = "none")

ggsave("Figure1.png", last_plot(), width = 15, height = 15, units = "cm", 
       dpi = 300)

#### FIGURE 2: impact on uncertainty of lambda ####

# CI width

ggplot(data = filter(summary_results, parameter == "lambda"),
       aes(x = scenario,
           y = CI_width, fill = scenario, colour = scenario)) +
  geom_violin(scale = "width", draw_quantiles = c(0.025, 0.5, 0.975)) +
  scale_fill_manual(values = colours) +
  scale_color_manual(values = c("black", colours[2], "grey", "grey", "grey"))+
  plain_theme() +
  facet_grid(rows = vars(matrix_number))+
  labs(x = "", y = "CI width") +
  theme(legend.position = "none")

ggsave("Figure2.png", last_plot(), width = 15, height = 15, units = "cm", 
       dpi = 300)

#### FIGURE 3: impact on accuracy of uncertainty ####

# True in CI - might just need to be a %

true_in_ci <- summary_results %>%
  group_by(scenario, matrix_number, parameter, true_in_CI) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count)) %>%
  filter(true_in_CI == TRUE)

write.csv(true_in_ci, "true_in_ci_2x2.csv")


###############################################################################

#### 3x3 ####
## get filenames for all 3x3

load("./Data files/3x3/threebythree_matrices.RData")
names(matrices_33) <- c("mat1", "mat2", "mat3", "mat4", "mat5")

filenames <- data.frame(filename = c(list.files("./Data files/3x3/a_Baseline/", 
                                                full.names = TRUE),
                                     list.files("./Data files/3x3/a_R_error/", 
                                                full.names = TRUE),
                                     list.files("./Data files/3x3/a_R_missing/", 
                                                full.names = TRUE),
                                     list.files("./Data files/3x3/a_A_missing/", 
                                                full.names = TRUE),
                                     list.files("./Data files/3x3/a_J_missing/", 
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
                                 matrix_number = rep(1:5, each = 10)) %>%
  pivot_wider(values_from = value, names_from = parameter)

test <- left_join(filenames, true_parameters_33) %>% 
  arrange(filenames) %>%
  group_by(filename) %>%
  group_split()

#### Baseline ####

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

###############################################################################

colours <- c("#FFFFFF", "#feff54", "#30666B", "#134263", "#1E2E39")

# re-order the factor levels
summary_results <- summary_results %>%
  dplyr::mutate(scenario = as.factor(scenario),
                scenario = plyr::revalue(scenario, 
                                         c("baseline"="baseline", 
                                           "adult_missing"="adult \nmissing",
                                           "juvenile_missing" = "juvenile \nmissing",
                                           "random_error" = "random \nerror",
                                           "random_missing" = 
                                             "random \nmissing")),
                scenario = fct_relevel(scenario, 
                                       "baseline",
                                       "random \nerror",
                                       "random \nmissing",
                                       "adult \nmissing",
                                       "juvenile \nmissing"))

#### FIGURE 1: impact on accuracy of lambda ####

# plot error for each scenario

ggplot(data = filter(summary_results, parameter == "lambda"),
       aes(x = scenario,
           y = error, fill = scenario,
           colour = scenario)) +
  geom_violin(scale = "width", draw_quantiles = c(0.025, 0.5, 0.975)) +
  scale_fill_manual(values = colours) +
  scale_color_manual(values = c("black", "grey50", "white", "white", "white"))+
  plain_theme() +
  facet_grid(rows = vars(matrix_number))+
  labs(x = "", y = "Estimate - True") +
  theme(legend.position = "none")

ggsave("Figure1_3x3.png", last_plot(), width = 15, height = 15, units = "cm", 
       dpi = 300)

#### FIGURE 2: impact on uncertainty of lambda ####

# CI width

ggplot(data = filter(summary_results, parameter == "lambda"),
       aes(x = scenario,
           y = CI_width, fill = scenario, colour = scenario)) +
  geom_violin(scale = "width", draw_quantiles = c(0.025, 0.5, 0.975)) +
  scale_fill_manual(values = colours) +
  scale_color_manual(values = c("black", colours[2], "grey", "grey", "grey"))+
  plain_theme() +
  facet_grid(rows = vars(matrix_number))+
  labs(x = "", y = "CI width") +
  theme(legend.position = "none")

ggsave("Figure2_3x3.png", last_plot(), width = 15, height = 15, units = "cm", 
       dpi = 300)

#### FIGURE 3: impact on accuracy of uncertainty ####

# True in CI - might just need to be a %

true_in_ci <- summary_results %>%
  group_by(scenario, matrix_number, parameter, true_in_CI) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count)) %>%
  ungroup() %>%
  complete(true_in_CI, nesting(scenario, matrix_number, parameter), # add in rows that are missing
           fill = list(percent = 0,
                       count = 0)) %>%
  filter(true_in_CI == TRUE)

write.csv(true_in_ci, "true_in_ci_3x3.csv")
