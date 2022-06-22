# SCRIPT TO ASSESS CHARACTERISTICS OF DATA #

#### Set up ####

# load packages

library(tidyverse)

#install.packages("Rcompadre")

library(Rcompadre)

library(dplyr)
library(popdemo)
library(maps)     # for plotting world map

# load data

load("working_comadre.RData")

#### check data ####

# how many populations have density dependence? None - all deterministic. 
# but can look at MatrixTreatment and MatrixObservation columns

Treatments <- comadre_reduced %>% 
  filter(MatrixTreatment != "Unmanipulated") %>%
  group_by(MatrixTreatment) %>% 
  summarize(n_populations = length(unique(MatrixPopulation))) %>% 
  arrange(desc(n_populations))

# could explore all those that were manipulated and see what can be added?

Observations <- comadre_reduced %>% 
  filter(MatrixTreatment != "Unmanipulated") %>%
  group_by(Observations) %>% 
  summarize(n_populations = length(unique(MatrixPopulation))) %>% 
  arrange(desc(n_populations))

# how many populations include environment or stochasticity?
# will be those that have > 6 years of MPMs
# NEED TO ASSESS WHICH HAVE YEARLY MPMS OF THE SAME LENGTH AS STUDY

comadre_reduced_env <- comadre_reduced %>% 
  filter(as.numeric(StudyDuration) > 6) %>%
  group_by(MatrixPopulation) %>% 
  summarize(n_populations = length(unique(MatrixID)),
            StudyDuration = max(StudyDuration)) %>% 
  arrange(desc(n_populations))
