#### T1.1: Data simulation for scenario 2: different sample sizes #

# this script will generate data including parametric bootstrap confidence intervals


################################################################################

#### Set up ####

# load packages

library(tidyverse)
library(HMMpa)

# load any data

load("./Data files/baseline_simulation_observations.RData")

# source functions

source("./Functions/parametric_bootstrap_function.R")

#### Simulation 3: reduced sample ####

# these are direct calculations so need to save out
output_reduced_sample <- data.frame(scenario = rep(1:100, 4),
                                    sample = rep(c("all",
                                                   "50%",
                                                   "1year",
                                                   "50%1year"), each = 100),

                                    CI_lower = NA,
                                    CI_upper = NA)

# add means and variances
i <- as.list(1:100)

# function to calculate means and variances
output_reduced_sample_means <- map2_df(.x = baseline_observations,
                                 .y = i, ~{
                                   
# define datasets for each scenario to ensure mean and var calc on same data                                   
set.seed(.y)
half <- sample(filter(.x, Age > 1)$Offspring_obs, 
               length(which(.x$Age > 1))/2)
set.seed(.y)
one_year <- filter(.x, Age > 1 &
                     Year == sample(1:10, 1))$Offspring_obs
set.seed(.y)
marker <- sample(1:10, 1)
set.seed(.y)
half_one_year <- sample(filter(.x, Age > 1 &
                                 Year == marker)$Offspring_obs,
                        length(filter(.x, Age > 1 &
                                 Year == marker)$Offspring_obs)/2)
                                   
mean_adult <- c(mean(filter(.x, Age > 1)$Offspring_obs),
                mean(half),
                mean(one_year),
                mean(half_one_year))

var_adult <- c(var(filter(.x, Age > 1)$Offspring_obs),
                var(half),
                var(one_year),
                var(half_one_year))

# define datasets for each scenario to ensure mean and var calc on same data                                   
set.seed(.y)
half <- sample(filter(.x, Age == 1)$Offspring_obs, 
               length(which(.x$Age == 1))/2)
set.seed(.y)
one_year <- filter(.x, Age == 1 &
                     Year == sample(1:10, 1))$Offspring_obs
set.seed(.y)
half_one_year <- sample(filter(.x, Age == 1 &
                                 Year == marker)$Offspring_obs,
                        length(filter(.x, Age == 1 &
                                 Year == marker)$Offspring_obs)/2)

mean_juvenile <- c(mean(filter(.x, Age == 1)$Offspring_obs),
                mean(half),
                mean(one_year),
                mean(half_one_year))

var_juvenile <- c(var(filter(.x, Age == 1)$Offspring_obs),
               var(half),
               var(one_year),
               var(half_one_year))

return(data.frame(sample = c("all", "50%", 
                             "1year", "50%1year"),
                  mean_adult = mean_adult,
                  mean_juvenile = mean_juvenile,
                  var_adult = var_adult,
                  var_juvenile = var_juvenile,
                  scenario = .y))


})


# then combine into the output dataframe
output_reduced_sample <- inner_join(output_reduced_sample,
           output_reduced_sample_means, 
           by = c("scenario", "sample"))


# save
save(output_reduced_sample, 
     file = "./Data files/reduced_sample_no_CI.RData")


################################################################################

# run bootstrap

#### bootstrap ####

filenames <- as.list(list.files("./Data files/Baseline_results/"))

baseline_summary_results <- map(.x = filenames, ~{
  load(paste("./Data files/Baseline_results/", .x, sep = ""))
  summary <- MCMCsummary(model_result, round = 2)
  return(summary)
})


list_output_reduced_sample <- split(output_reduced_sample,
                                    seq(nrow(output_reduced_sample)))

##### SOME YEARS THERE ARE NO ADULTS SAMPLED: NEED TO ADD CODE TO BOOTSTRAP TO IGNORE THIS

results <- map2_df(.x = list_output_reduced_sample[311],
                  .y = rep(baseline_summary_results,4)[311], ~{
                    
  output <- rerun(100, parametric_bootstrap_function(.x, .y)) %>%
    bind_rows()
  
  summary_output <- data.frame(lower_CI_r_juv = sort(output$r_juv)[0.025*100],
                               upper_CI_r_juv = sort(output$r_juv)[0.975*100],
                               lower_CI_r_ad = sort(output$r_ad)[0.025*100],
                               upper_CI_r_ad = sort(output$r_ad)[0.975*100],
                               lower_CI_s_juv = sort(output$s_juv)[0.025*100],
                               upper_CI_s_juv = sort(output$s_juv)[0.975*100],
                               lower_CI_s_ad = sort(output$s_ad)[0.025*100],
                               upper_CI_s_ad = sort(output$s_ad)[0.975*100],
                               lower_CI_lambda = sort(output$lambda)[0.025*100],
                               upper_CI_lambda = sort(output$lambda)[0.975*100])
                    
})

results

