#### T1.1: Script to plot results for papers and presentations
# both Bayesian model results and stage-fate estimates

################################################################################

#### Set up ####

# load packages

library(tidyverse)

# source script

source("./Scripts/theme_script.R")

# load all summary data

load("./Data files/2x2/summary_results_2x2.RData")
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
summary_results_2x2 <- summary_results
load("./Data files/3x3/summary_results_3x3.RData")
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
summary_results_3x3 <- summary_results
load("./Data files/5x5/summary_results_5x5.RData")
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
summary_results_5x5 <- summary_results

summary_results_all <- bind_rows(summary_results_2x2,
                                 summary_results_3x3,
                                 summary_results_5x5) %>%
  mutate(matrix_size = c(rep(2, 17500),
                         rep(3, 25000),
                         rep(5, 40000)))



# load all true in ci data
true_ci_2x2 <- read.csv("./Data files/2x2/true_in_ci_2x2.csv")
true_ci_3x3 <- read.csv("./Data files/3x3/true_in_ci_3x3.csv")
true_ci_5x5 <- read.csv("./Data files/5x5/true_in_ci_5x5.csv")

true_ci_all <-  bind_rows(true_ci_2x2,
                          true_ci_3x3,
                          true_ci_5x5) %>%
  mutate(matrix_size = c(rep(2, 162),
                         rep(3, 250),
                         rep(5, 400))) %>%
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

# STAGE-FATE FILES

full_results_2x2_direct <- read.csv("./Data files/2x2/full_direct_est_results_2x2.csv")
full_results_3x3_direct <- read.csv("./Data files/3x3/full_direct_est_results_3x3.csv")
full_results_5x5_direct <- read.csv("./Data files/5x5/full_direct_est_results_5x5.csv")

full_results_direct <- bind_rows(full_results_2x2_direct,
                                 full_results_3x3_direct,
                                 full_results_5x5_direct) %>%
  mutate(matrix_size = c(rep(2, 12500),
                         rep(3, 17500),
                         rep(5, 27500))) %>%
  dplyr::mutate(scenario = as.factor(scenario),
                scenario = fct_relevel(scenario, 
                                       "baseline",
                                       "random \nerror",
                                       "random \nmissing",
                                       "adult \nmissing",
                                       "juvenile \nmissing"))

true_ci_2x2_direct <- read.csv("./Data files/2x2/direct_est_coverage_2x2.csv")
true_ci_3x3_direct <- read.csv("./Data files/3x3/direct_est_coverage_3x3.csv")
true_ci_5x5_direct <- read.csv("./Data files/5x5/direct_est_coverage_5x5.csv")

true_ci_all_direct <- bind_rows(true_ci_2x2_direct,
                                true_ci_3x3_direct,
                                true_ci_5x5_direct) %>%
  mutate(matrix_size = c(rep(2, 125),
                         rep(3, 175),
                         rep(5, 275)))%>%
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

##############################################################################

# set up colour scheme for plots

colours_scenario <- c("#FFFFFF", "#feff54", "#30666B", "#134263", "#1E2E39")

colfunc<-colorRampPalette(c("white", "orangered"))

colours_size <- colfunc(5)

################################################################################

#### PUBLICATION ####
#### FIGURE 1: mean error in lambda estimate (Bayesian) 3x3 ####

# plot of the mean absolute error in lambda
mean_error_Bayesian <- summary_results_all %>%
  filter(parameter == "lambda") %>%
  group_by(scenario, matrix_size, matrix_number) %>%
  summarise(mean_error = mean(error),
            variance_error = var(error))


Figure1_publication <- ggplot(data = 
                                filter(mean_error_Bayesian,
                                       matrix_size == 3),
       aes(y = mean_error,
           x = scenario, fill = scenario,
           colour = scenario)) +
  geom_col() +
  scale_fill_manual(values = colours_scenario) +
  scale_color_manual(values = c("grey90", colours_scenario[2:5]))+
  plain_theme() +
  facet_grid(rows = vars(matrix_number))+
  labs(x = "", y = "Error in posterior mean estimate",
       title = "Mean accuracy error of lambda") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0))

Figure1_publication

ggsave("./Figures/publication_Figure1.png", Figure1_publication, 
       width = 20, height = 15, units = "cm")

#### FIGURE 2: precision of lambda estimate (Bayesian) 3x3 ####

# calculate mean credible interval widths across all simulations
CI_width <- summary_results_all %>%
  group_by(matrix_size, matrix_number, scenario) %>%
  summarise(mean_CI = mean(CI_width))

Figure2_publication <- ggplot(data = filter(CI_width,
                                            matrix_size == 3),
                              aes(y = mean_CI,
                                  x = scenario, fill = scenario,
                                  colour = scenario)) +
  geom_col() +
  scale_fill_manual(values = colours_scenario) +
  scale_color_manual(values = c("grey90", colours_scenario[2:5]))+
  plain_theme() +
  facet_grid(rows = vars(matrix_number))+
  labs(x = "", y = "Mean credible interval width",
       title = "Mean precision of lambda estimates") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0))

Figure2_publication

ggsave("./Figures/publication_Figure2.png", Figure2_publication, 
       width = 20, height = 15, units = "cm")

#### FIGURE 3: coverage for lambda estimate (Bayesian) 3x3 ####

Figure3_publication <- ggplot(data = filter(true_ci_all, 
                                            parameter == "lambda",
                                            matrix_size == 3),
                               aes(x = scenario,
                                   y = percent*100, fill = scenario, 
                                   colour = scenario)) +
       aes(x = scenario,
           y = percent*100, fill = scenario, colour = scenario) +
  geom_col() +
  geom_hline(yintercept = 95) +
  scale_fill_manual(values = colours_scenario) +
  scale_color_manual(values = c("grey90", 
                                colours_scenario[2:5]))+
  plain_theme() +
  facet_grid(rows = vars(matrix_number))+
  labs(x = "", y = "Percentage true in CI",
       title = "Coverage of lambda") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0)) +
  ylim(0,100)

Figure3_publication

ggsave("./Figures/publication_Figure3.png", Figure3_publication, 
       width = 20, height = 15, units = "cm")

#### SUPPORTING INFORMATION ####
#### SOM FIGURE 1: variance of error in lambda estimate (Bayesian) ####

# plot of the mean absolute error in lambda
mean_error_Bayesian <- summary_results_all %>%
  filter(parameter == "lambda") %>%
  group_by(scenario, matrix_size, matrix_number) %>%
  summarise(mean_error = mean(error),
            variance_error = var(error))


SOM_Figure1_publication <- ggplot(data = 
                                filter(mean_error_Bayesian),
                              aes(y = variance_error,
                                  x = scenario, fill = scenario,
                                  colour = scenario)) +
  geom_col() +
  scale_fill_manual(values = colours_scenario) +
  scale_color_manual(values = c("grey90", colours_scenario[2:5]))+
  plain_theme() +
  facet_grid(rows = vars(matrix_number),
             cols = vars(matrix_size))+
  labs(x = "", y = "Variance of posterior estimates",
       title = "Variance of error in lambda") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))

SOM_Figure1_publication

ggsave("./Figures/SOM_Figure1.png", SOM_Figure1_publication, 
       width = 25, height = 15, units = "cm")

#### SOM FIGURE 2: mean error in lambda estimate (Bayesian) 2x2 and 5x5 ####

# plot of the mean absolute error in lambda
mean_error_Bayesian <- summary_results_all %>%
  filter(parameter == "lambda") %>%
  group_by(scenario, matrix_size, matrix_number) %>%
  summarise(mean_error = mean(error),
            variance_error = var(error)) %>%
  mutate(matrix_size = case_when(matrix_size == 2 ~ "2x2 matrix",
                                 matrix_size == 5 ~ "5x5 matrix",
                                 TRUE ~ "3"))


SOM_Figure2_publication <- ggplot(data = 
                                    filter(mean_error_Bayesian,
                                           matrix_size != "3"),
                                  aes(y = mean_error,
                                      x = scenario, fill = scenario,
                                      colour = scenario)) +
  geom_col() +
  scale_fill_manual(values = colours_scenario) +
  scale_color_manual(values = c("grey90", colours_scenario[2:5]))+
  plain_theme() +
  facet_grid(rows = vars(matrix_number),
             cols = vars(matrix_size))+
  labs(x = "", y = "Error in posterior mean estimate",
       title = "Mean accuracy error of lambda") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))

SOM_Figure2_publication

ggsave("./Figures/SOM_Figure2.png", SOM_Figure2_publication, 
       width = 20, height = 15, units = "cm")

#### SOM FIGURE 3: precision of lambda estimate (Bayesian) 2x2 and 5x5 ####

# calculate mean credible interval widths across all simulations
CI_width <- summary_results_all %>%
  group_by(matrix_size, matrix_number, scenario) %>%
  summarise(mean_CI = mean(CI_width)) %>%
  mutate(matrix_size = case_when(matrix_size == 2 ~ "2x2 matrix",
                                 matrix_size == 5 ~ "5x5 matrix",
                                 TRUE ~ "3"))

SOM_Figure3_publication <- ggplot(data = filter(CI_width,
                                                matrix_size != "3"),
                                  aes(y = mean_CI,
                                      x = scenario, fill = scenario,
                                      colour = scenario)) +
  geom_col() +
  scale_fill_manual(values = colours_scenario) +
  scale_color_manual(values = c("grey90", colours_scenario[2:5]))+
  plain_theme() +
  facet_grid(rows = vars(matrix_number),
             cols = vars(matrix_size))+
  labs(x = "", y = "Mean credible interval width",
       title = "Mean precision of lambda estimates") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))

SOM_Figure3_publication

ggsave("./Figures/SOM_Figure3.png", SOM_Figure3_publication, 
       width = 20, height = 15, units = "cm")

#### SOM FIGURE 4: coverage of lambda estimate (Bayesian) 2x2 and 5x5 ####

true_ci_all <- true_ci_all %>%
  mutate(matrix_size = case_when(matrix_size == 2 ~ "2x2 matrix",
                                 matrix_size == 5 ~ "5x5 matrix",
                                 TRUE ~ "3"))

SOM_Figure4_publication <- ggplot(data = filter(true_ci_all, 
                                                parameter == "lambda",
                                                matrix_size != "3"),
                                  aes(x = scenario,
                                      y = percent*100, fill = scenario, 
                                      colour = scenario)) +
  aes(x = scenario,
      y = percent*100, fill = scenario, colour = scenario) +
  geom_col() +
  geom_hline(yintercept = 95) +
  scale_fill_manual(values = colours_scenario) +
  scale_color_manual(values = c("grey90", 
                                colours_scenario[2:5]))+
  plain_theme() +
  facet_grid(rows = vars(matrix_number),
             cols = vars(matrix_size))+
  labs(x = "", y = "Percentage true in CI",
       title = "Coverage of lambda") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))+
  ylim(0,100)

SOM_Figure4_publication

ggsave("./Figures/SOM_Figure4.png", SOM_Figure4_publication, 
       width = 20, height = 15, units = "cm")


#### SOM FIGURE 5: stage-fate mean error in lambda ####

mean_error_size_stage_fate <- full_results_direct %>%
  filter(parameter == "lambda") %>%
  group_by(scenario, matrix_size, matrix_number) %>%
  summarise(mean_error = mean(error),
            variance_error = var(error)) %>%
  mutate(matrix_size = case_when(matrix_size == 2 ~ "2x2 matrix",
                                 matrix_size == 5 ~ "5x5 matrix",
                                 matrix_size == 3 ~ "3x3 matrix"))

SOM_Figure5_publication <- ggplot(data = mean_error_size_stage_fate,
       aes(y = mean_error,
           x = scenario, fill = scenario,
           colour = scenario)) +
  geom_col() +
  scale_fill_manual(values = colours_scenario) +
  scale_color_manual(values = c("grey90", 
                                colours_scenario[2:5]))+
  plain_theme() +
  facet_grid(cols = vars(matrix_size),
             rows = vars(matrix_number))+
  labs(x = "", y = "Error in mean estimate",
       title = "Mean accuracy error of lambda \n(stage fate)") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))

SOM_Figure5_publication

ggsave("./Figures/SOM_Figure5.png", SOM_Figure5_publication, 
       width = 20, height = 15, units = "cm")

#### SOM FIGURE 6: stage-fate variance of error in lambda ####

mean_error_size_stage_fate <- full_results_direct %>%
  filter(parameter == "lambda") %>%
  group_by(scenario, matrix_size, matrix_number) %>%
  summarise(mean_error = mean(error),
            variance_error = var(error)) %>%
  mutate(matrix_size = case_when(matrix_size == 2 ~ "2x2 matrix",
                                 matrix_size == 5 ~ "5x5 matrix",
                                 matrix_size == 3 ~ "3x3 matrix"))

SOM_Figure6_publication <- ggplot(data = mean_error_size_stage_fate,
                                  aes(y = variance_error,
                                      x = scenario, fill = scenario,
                                      colour = scenario)) +
  geom_col() +
  scale_fill_manual(values = colours_scenario) +
  scale_color_manual(values = c("grey90", 
                                colours_scenario[2:5]))+
  plain_theme() +
  facet_grid(cols = vars(matrix_size),
             rows = vars(matrix_number))+
  labs(x = "", y = "Variance of error of lambda",
       title = "Variance of error of lambda \n(stage fate)") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))

SOM_Figure6_publication

ggsave("./Figures/SOM_Figure6.png", SOM_Figure6_publication, 
       width = 20, height = 15, units = "cm")

#### SOM FIGURE 7: stage-fate precision of lambda estimate ####

# calculate mean credible interval widths across all simulations
CI_width_stage_fate <- full_results_direct %>%
  group_by(matrix_size, matrix_number, scenario, parameter) %>%
  summarise(mean_CI = mean(CI_width)) %>% 
  mutate(matrix_size = case_when(matrix_size == 2 ~ "2x2 matrix",
                                 matrix_size == 5 ~ "5x5 matrix",
                                 matrix_size == 3 ~ "3x3 matrix"))

SOM_Figure7_publication <- ggplot(data = filter(CI_width_stage_fate, 
                                                parameter == "lambda"), 
       aes(y = mean_CI, x = scenario, 
           fill = scenario,
           color = scenario)) +
  geom_col() +
  scale_fill_manual(values = colours_scenario) +
  scale_color_manual(values = c("grey90", 
                                colours_scenario[2:5]))+
  plain_theme() +
  facet_grid(rows = vars(matrix_number), 
             cols = vars(matrix_size))+
  labs(x = "", y = "Mean confidence interval width",
       title = "Mean precision of lambda estimates") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))

SOM_Figure7_publication

ggsave("./Figures/SOM_Figure7.png", SOM_Figure7_publication, 
       width = 20, height = 15, units = "cm")

#### SOM FIGURE 8: stage-fate coverage of lambda estimate ####
  
true_ci_all_direct <- true_ci_all_direct %>%
  mutate(matrix_size = case_when(matrix_size == 2 ~ "2x2 matrix",
                                 matrix_size == 5 ~ "5x5 matrix",
                                 matrix_size == 3 ~ "3x3 matrix"))

SOM_Figure8_publication <- ggplot(data = filter(true_ci_all_direct, 
                                                parameter == "lambda"),
                                  aes(x = scenario,
                                      y = percentage, fill = scenario, 
                                      colour = scenario)) +
  geom_col() +
  geom_hline(yintercept = 95) +
  scale_fill_manual(values = colours_scenario) +
  scale_color_manual(values = c("grey90", 
                                colours_scenario[2:5]))+
  plain_theme() +
  facet_grid(rows = vars(matrix_number),
             cols = vars(matrix_size))+
  labs(x = "", y = "Percentage true in CI",
       title = "Coverage of lambda") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))+
  ylim(0,100)

SOM_Figure8_publication

ggsave("./Figures/SOM_Figure8.png", SOM_Figure8_publication, 
       width = 20, height = 15, units = "cm")
