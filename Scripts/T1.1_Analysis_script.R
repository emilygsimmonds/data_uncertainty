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

# load results

## load these ahead of each analysis to save space

################################################################################

true_parameters <- data.frame(parameter = c("reproduction_juvenile",
                                            "reproduction_adult",
                                            "recapture_juvenile",
                                            "recapture_adult",
                                            "survival_juvenile",
                                            "survival_adult",
                                            "lambda",
                                            "stable_juvenile",
                                            "stable_adult"),
                              value = c(0.6,
                                        0.8,
                                        1,
                                        0.8,
                                        0.3,
                                        0.47,
                                        1.04,
                                        0.88,
                                        0.48))

#### Baseline ####

## import each datafile and get a summary

filenames <- as.list(list.files("./Data files/Baseline_results/"))

baseline_summary_results <- map(.x = filenames, ~{
  load(paste("./Data files/Baseline_results/", .x, sep = ""))
  summary <- MCMCsummary(model_result, round = 2)
  return(summary)
})

## summarise the summaries

i <- as.list(1:100)

baseline_summary_results2 <- map2_df(.x = baseline_summary_results,
                                    .y = i, ~{
                                    summary_summary(results = .x,
                                                    true_parameters = true_parameters,
                                                    i = .y)  
                                    })

## % of simulations when certain criteria achieved

# plot of error

ggplot(data = filter(baseline_summary_results2,
                     parameter != "stable_adult" &
                       parameter != "stable_juvenile"), 
       aes(x = parameter, y = error, fill = parameter)) +
  geom_hline(yintercept = 0, colour = "grey") +
  geom_violin() +
  plain_theme() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90))

# % with true in CI by parameter

baseline_summary_results2 %>% 
  group_by(parameter, true_in_CI) %>%
  summarise(percent = n())

# mean CI width

baseline_summary_results2 %>% 
  group_by(parameter) %>%
  summarise(mean = mean(CI_width))

#### Random error ####

## import each datafile and get a summary

filenames <- as.list(list.files("./Data files/Random_error_results/"))

random_error_summary_results <- map(.x = filenames, ~{
  load(paste("./Data files/Random_error_results/", .x, sep = ""))
  summary <- MCMCsummary(model_result, round = 2)
  return(summary)
})

## summarise the summaries

i <- as.list(1:100)

random_error_summary_results2 <- map2_df(.x = random_error_summary_results,
                                     .y = i, ~{
                                       summary_summary(results = .x,
                                                       true_parameters = true_parameters,
                                                       i = .y)  
                                     })

## % of simulations when certain criteria achieved

# plot of error

ggplot(data = random_error_summary_results2, 
       aes(x = parameter, y = error, fill = parameter)) +
  geom_hline(yintercept = 0, colour = "grey") +
  geom_violin() +
  plain_theme() +
  theme(legend.position = "none")

# % with true in CI by parameter

random_error_summary_results2 %>% 
  group_by(parameter, true_in_CI) %>%
  summarise(percent = n())

# mean CI width

random_error_summary_results2 %>% 
  group_by(parameter) %>%
  summarise(mean = mean(CI_width))

#### Random missing ####

## import each datafile and get a summary

filenames <- as.list(list.files("./Data files/Random_missing_results/"))

random_missing_summary_results <- map(.x = filenames, ~{
  load(paste("./Data files/Random_missing_results/", .x, sep = ""))
  summary <- MCMCsummary(model_result, round = 2)
  return(summary)
})

## summarise the summaries

i <- as.list(1:100)

random_missing_summary_results2 <- map2_df(.x = random_missing_summary_results,
                                         .y = i, ~{
                                           summary_summary(results = .x,
                                                           true_parameters = true_parameters,
                                                           i = .y)  
                                         })

## % of simulations when certain criteria achieved

# plot of error

ggplot(data = random_missing_summary_results2, 
       aes(x = parameter, y = error, fill = parameter)) +
  geom_hline(yintercept = 0, colour = "grey") +
  geom_violin() +
  plain_theme() +
  theme(legend.position = "none")

# % with true in CI by parameter

random_missing_summary_results2 %>% 
  group_by(parameter, true_in_CI) %>%
  summarise(percent = n())

# mean CI width

random_missing_summary_results2 %>% 
  group_by(parameter) %>%
  summarise(mean = mean(CI_width))

