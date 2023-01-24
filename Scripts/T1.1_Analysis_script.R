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
                                        0.4666,
                                        1.03,
                                        0.88,
                                        0.47))

#### Baseline ####

## import each datafile and get a summary

filenames <- c(as.list(list.files("./Data files/Baseline_results/", 
                                  full.names = TRUE)[str_detect(list.files("./Data files/Baseline_results/"), 
                                "SS")]),
               as.list(list.files("./Data files/Adult_results_20/", 
                                  full.names = TRUE)[str_detect(list.files("./Data files/Adult_results_20/"), 
                                                                               "SS")]),
               as.list(list.files("./Data files/Juvenile_results_20/", 
                                  full.names = TRUE)[str_detect(list.files("./Data files/Juvenile_results_20/"), 
                                                                               "SS")]),
               as.list(list.files("./Data files/Random_error_results/", 
                                  full.names = TRUE)[str_detect(list.files("./Data files/Random_error_results/"), 
                                                                               "SS")]))


results100 <- map(.x = filenames[1:100], ~{
  load(.x)
  summary <- MCMCsummary(model_result, round = 2)
  return(summary)
})

results200 <- map(.x = filenames[101:200], ~{
  load(.x)
  summary <- MCMCsummary(model_result, round = 2)
  return(summary)
})

results300 <- map(.x = filenames[201:300], ~{
  load(.x)
  summary <- MCMCsummary(model_result, round = 2)
  return(summary)
})

results400 <- map(.x = filenames[301:400], ~{
  load(.x)
  summary <- MCMCsummary(model_result, round = 2)
  return(summary)
})

results <- c(results100,
             results200,
             results300,
             results400)


## summarise the summaries

i <- as.list(rep(c("baseline",
                   "adult_missing",
                   "juvenile_missing",
                   "random_error"), each = 100))

summary_results <- map2_df(.x = results,
                           .y = i, ~{
                                    summary_summary(results = .x,
                                                    true_parameters = true_parameters,
                                                    i = .y)  
                                    })

save(summary_results, file = "summary_results.RData")

###############################################################################

colours <- c("#FFFFFF", "#feff54", "#30666B", "#1E2E39")

# re-order the factor levels
summary_results <- summary_results %>%
  dplyr::mutate(scenario = as.factor(scenario),
                scenario = plyr::revalue(scenario, 
                                                 c("baseline"="baseline", 
                                                   "adult_missing"="adult \nmissing",
                                                   "juvenile_missing" = "juvenile \nmissing",
                                                   "random_error" = "random \nerror")),
                scenario = fct_relevel(scenario, 
                                               "baseline",
                                               "random \nerror",
                                               "adult \nmissing",
                                               "juvenile \nmissing"))

#### FIGURE 1 BES: impact on accuracy of lambda ####

# plot error for each scenario

ggplot(data = filter(summary_results, parameter == "lambda"),
       aes(x = scenario,
           y = error, fill = scenario,
           colour = scenario)) +
  geom_violin(scale = "width", draw_quantiles = c(0.025, 0.5, 0.975)) +
  scale_fill_manual(values = colours) +
  scale_color_manual(values = c("black", "grey50", "white", "white"))+
  BES_theme() +
  labs(x = "", y = "Estimate - True") +
  theme(legend.position = "none")

ggsave("BES_Fig4.png", last_plot(), width = 15, height = 15, units = "cm", 
       dpi = 300)

#### FIGURE 2 BES: impact on uncertainty of lambda ####

# CI width

ggplot(data = filter(summary_results, parameter == "lambda"),
       aes(x = scenario,
           y = CI_width, fill = scenario, colour = scenario)) +
  geom_violin(scale = "width", draw_quantiles = c(0.025, 0.5, 0.975)) +
  scale_fill_manual(values = colours) +
  scale_color_manual(values = c(colours[1:2], "white", "white"))+
  BES_theme() +
  labs(x = "", y = "CI width") +
  theme(legend.position = "none")

ggsave("BES_Fig5.png", last_plot(), width = 15, height = 15, units = "cm", 
       dpi = 300)

#### FIGURE 3 BES: impact on accuracy of uncertainty ####

# True in CI - might just need to be a %

summary_results %>% filter(parameter == "lambda") %>%
  group_by(scenario, parameter, true_in_CI) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count)) %>%
  filter(true_in_CI == TRUE)

summary_results %>% filter(parameter == "reproduction_juvenile") %>%
  group_by(scenario, parameter, true_in_CI) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count)) %>%
  filter(true_in_CI == TRUE)

summary_results %>% filter(parameter == "reproduction_adult") %>%
  group_by(scenario, parameter, true_in_CI) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count)) %>%
  filter(true_in_CI == TRUE)

summary_results %>% filter(parameter == "survival_juvenile") %>%
  group_by(scenario, parameter, true_in_CI) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count)) %>%
  filter(true_in_CI == TRUE)
