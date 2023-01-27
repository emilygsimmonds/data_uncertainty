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
                            max_year,
                            stages){
  
  # order by Year
  sorted_census_data <- arrange(census_data, Year) 
  number_years <- length(min(sorted_census_data$Year):
                           max(sorted_census_data$Year))
  
  # include max year of the data as a caveat - do not mark individuals
  # dead if they were seen in final year of data
  
  # set up vector of stages and their fates
  marker_stages <- c(stages, rep(stages[length(stages)], 5)) # rep final stage 5 times (max in data)
  marker_fates <- c(marker_stages[-1], stages[(length(stages))]) # remove juvenile and repeat final stage
  
  # set up output dataframe
  if(max(sorted_census_data$Year) == max_year){ # IF observed in final year - assume survived
    output <- data.frame(ID = census_data$ID[1],
                         Year1 = seq(sorted_census_data$Year[1], 
                                     max(sorted_census_data$Year),
                                     1), # all years that individual was observed AND any gaps - assume alive
                         Stage1 = c(marker_stages[(which(marker_stages == sorted_census_data$Stage[1])[1]):
                                                    (which(marker_stages == sorted_census_data$Stage[1])[1]+(number_years-1))]), # start with first age seen, all others will be adult or dead
                         Year2 = seq(sorted_census_data$Year[1], 
                                     max(sorted_census_data$Year),
                                     1)+1,
                         Stage2 = c(marker_fates[(which(marker_stages == sorted_census_data$Stage[1])[1]):
                                                   (which(marker_stages == sorted_census_data$Stage[1])[1]+(number_years-1))]),
                         Offspring = NA)
  }else{ # otherwise, include 'dead' as final fate
  output <- data.frame(ID = census_data$ID[1],
                       Year1 = seq(sorted_census_data$Year[1], 
                                   max(sorted_census_data$Year),
                                   1), # all years that individual was observed AND any gaps - assume alive
                       Stage1 = c(marker_stages[(which(marker_stages == sorted_census_data$Stage[1])[1]):
                                           (which(marker_stages == sorted_census_data$Stage[1])[1]+(number_years-1))]), # start with first age seen, all others will be adult or dead
                       Year2 = seq(sorted_census_data$Year[1], 
                                   max(sorted_census_data$Year),
                                   1)+1,
                       Stage2 = c(marker_fates[(which(marker_stages == sorted_census_data$Stage[1])[1]):
                                          (which(marker_stages == sorted_census_data$Stage[1])[1]+(number_years-2))],
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