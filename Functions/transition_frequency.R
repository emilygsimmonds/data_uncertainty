# FUNCTION to create a transition frequency table from census data #

################################################################################

## Function cleans previous output data to become new input data

## INPUT :
#
# - census data, with ID, Year, Survival, Offspring, Age, Trait

## OUTPUT = transition frequency table:
#
# - ID, Year1, Stage1, Year2, Stage2, Offspring

## load packages

library(tidyverse)

## source scripts

source("./Functions/stage_fate.R")

################################################################################

#### FUNCTION ####

create_transition_frequency_table <- function(census_data,
                                              max_year){
  
  # split data into list based on ID
  split_data <- census_data %>% 
    group_by(ID) %>% group_split()
  
  # then use this as input to fill_stage_fate function using map_df
  transition_table <- map_df(.x = split_data, ~{
    fill_stage_fate(.x, max_year = max_year)
  })
  
  colnames(transition_table) <- c("ID", "year", "stage",
                                  "year2", "fate", "juvenile")
  
  return(transition_table)
  
}