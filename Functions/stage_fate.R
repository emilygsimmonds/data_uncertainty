# FUNCTION fill in stage fate #

################################################################################

## Function fills in all stage fates for each individual

## INPUT :
#
# - single individual's census data: ID, Year, Survival, Offspring, Stage, Trait

## OUTPUT = transition frequency table for individual:
#
# - ID, Year1, Stage1, Year2, Stage2, Offspring

#### load packages

library(tidyverse)

################################################################################

#### FUNCTION ####

fill_stage_fate <- function(census_data,
                            max_year){
  
  # order by Year
  sorted_census_data <- arrange(census_data, Year) 
  number_years <- length(min(sorted_census_data$Year):
                           max(sorted_census_data$Year))
  
  # include max year of the data as a caveat - do not mark individuals
  # dead if they were seen in final year of data
  
  # set up output dataframe
  if(max(sorted_census_data$Year == max_year)){
    output <- data.frame(ID = census_data$ID[1],
                         Year1 = seq(sorted_census_data$Year[1], 
                                     max(sorted_census_data$Year),
                                     1), # all years that individual was observed AND any gaps - assume alive
                         Stage1 = c(sorted_census_data$Stage[1],
                                    rep("adult", number_years-1)), # start with first age seen, all others will be adult or dead
                         Year2 = seq(sorted_census_data$Year[1], 
                                     max(sorted_census_data$Year),
                                     1)+1,
                         Stage2 = rep("adult", number_years),
                         Offspring = NA)
  }else{
  output <- data.frame(ID = census_data$ID[1],
                       Year1 = seq(sorted_census_data$Year[1], 
                                   max(sorted_census_data$Year),
                                   1), # all years that individual was observed AND any gaps - assume alive
                       Stage1 = c(sorted_census_data$Stage[1],
                                  rep("adult", number_years-1)), # start with first age seen, all others will be adult or dead
                       Year2 = seq(sorted_census_data$Year[1], 
                                   max(sorted_census_data$Year),
                                   1)+1,
                       Stage2 = c(rep("adult", number_years-1),
                                  "dead"),
                       Offspring = NA)}
  
  # fill in Offspring as a loop
  for(i in 1:number_years){
    marker <- which(sorted_census_data$Year == output$Year1[i])
    if(length(marker) == 0){output$Offspring[i] <- 0}else{
    output$Offspring[i] <- sorted_census_data$Offspring[marker]}
  }
  
  return(output)
}