# SCRIPT TO PULL MATRICES FROM COMADRE #

#### Set up ####

# load packages

library(tidyverse)

#install.packages("Rcompadre")

library(Rcompadre)

library(dplyr)
library(popdemo)
library(maps)     # for plotting world map

#fetch the comadre database

comadre <- cdb_fetch('comadre')

#### Subset to useful matrices ####

# criteria: at least 5 years of data
# criteria: published since 2010

comadre_reduced <- comadre %>% filter(as.numeric(YearPublication) > 2010,
                              as.numeric(StudyDuration) > 4,
                              !is.na(DOI_ISBN),
                              DOI_ISBN != "NA")

# remove any matrices that are flagged with issues

comadre_reduced <- comadre_reduced %>% cdb_flag() %>%
                               filter(check_NA_A == FALSE)

#### Summarise ####

# sample size of 10+ years for T1.2

# number of DOIs - 45 papers to look at

DOI_summary <- comadre_reduced %>% 
  filter(as.numeric(StudyDuration) > 9) %>%
  group_by(DOI_ISBN) %>% 
  summarize(n_populations = length(unique(MatrixPopulation))) %>% 
  arrange(desc(n_populations))

# number of species - 50 species

comadre_reduced %>% 
  filter(as.numeric(StudyDuration) > 9) %>%
  group_by(SpeciesAccepted) %>% 
  summarize(n_populations = length(unique(MatrixPopulation))) %>% 
  arrange(desc(n_populations))

#### Pull out DOIs ####

write.csv(DOI_summary, "DOIs.csv")

#### Save working data set of COMADRE ####

save(comadre_reduced, file = "working_comadre.RData")

#### Want the DOIs for all other populations since 2010 (<10 years) ####

# criteria: at least 5 years of data
# criteria: published since 2016

comadre_short <- comadre %>% filter(as.numeric(YearPublication) > 2010,
                                      as.numeric(StudyDuration) < 10,
                                      !is.na(DOI_ISBN),
                                      DOI_ISBN != "NA")

DOI_summary2 <- comadre_short %>% 
  group_by(DOI_ISBN) %>% 
  summarize(n_populations = length(unique(MatrixPopulation))) %>% 
  arrange(desc(n_populations))

# 61 extra DOIs

write.csv(DOI_summary2, "DOIs_short_studies.csv")
