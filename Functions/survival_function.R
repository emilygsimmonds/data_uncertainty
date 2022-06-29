# FUNCTION to apply survival to a population vector #

################################################################################

## Function takes a dataframe of individuals at t and applies survival and
# recapture probabilities to give a new dataframe of 'survivors' at t+1

## INPUT :
#
# - input_data =a dataframe with column names:
# ID (num), Year (num), Surv (0/1), Recap (0/1), Offspring (num), 
# Age (num), Trait (num)
#
# - parameters = a transition matrix of size max_age X max_age 
#
# - p = vector of recapture probabilities length of max_age
# 
# - max_age = maximum age species can get to
# 
# - inc_trait = TRUE or FALSE if you want to include a trait as well
#
# - i = index to say which year is focal year
#
# - defined_seed = if repeatable analysis is desired, specify a numeric seed

## OUTPUT = a new dataframe with individuals that survived to the next year
# added to the end of the input data

#### FUNCTION ####
survival_function <- function(input_data, 
                              parameters = matrix(c(rep(1.6, 5),
                                                    0.5, 0, 0, 0, 0,
                                                    0, 0.5, 0, 0, 0,
                                                    0, 0, 0.5, 0, 0,
                                                    0, 0, 0, 0.5, 0), 
                                                  byrow = TRUE, 
                                                  ncol = 5),
                              p = rep(0.6, 5),
                              max_age = 5,
                              inc_trait = FALSE,
                              defined_seed = NULL, i) {
  
## Load packages
  
library(tidyverse)
  
################################################################################
  
### INITIAL CHECKS ###
  
# max_age is specified and a number
  if(is.numeric(max_age) != TRUE){stop("max_age must be a number")}
  
# no age in input data exceeds max_age
  if(length(which(input_data$Age > max_age)) > 0){stop("cannot have individuals
                                                       older than max_age")}
  
# i is specified and a number
  if(is.null(i)){stop("i must be supplied")}
  if(is.numeric(i) != TRUE){stop("i must be a number")}
  
# input data is correct format i.e. has the correct columns of correct format

  # first - check that all columns expected are present
  if(is.null(input_data$ID)){stop("ID column missing")}
  if(is.null(input_data$Year)){stop("Year column missing")}
  if(is.null(input_data$Surv)){stop("Surv column missing")}
  if(is.null(input_data$Recap)){stop("Recap column missing")}
  if(is.null(input_data$Offspring)){stop("Offspring column missing")}
  if(is.null(input_data$Age)){stop("Age column missing")}
  if(is.null(input_data$Trait)){stop("Trait column missing")}
  
  # then check their format
  if(!is.numeric(input_data$Year)){stop("Year should be numeric")}
  if(!is.numeric(input_data$ID)){stop("ID should be numeric")}
  if(!is.numeric(input_data$Surv)){stop("Surv should be numeric")}
  if(!is.numeric(input_data$Recap)){stop("Recap should be numeric")}
  if(!is.numeric(input_data$Offspring)){stop("Offspring should be numeric")}
  if(!is.numeric(input_data$Age)){stop("Age should be numeric")}
  if(!is.numeric(input_data$Trait)){stop("Trait should be numeric")}
  
  # then check limits for Surv (0/1), Recap (0/1) and Age (>0<max_age)
  if(length(which(input_data$Surv < 0)|which(input_data$Surv > 1)) > 0){
    stop("Surv must be 0 or 1")
  }
  if(length(which(input_data$Recap < 0)|which(input_data$Recap > 1)) > 0){
    stop("Recap must be 0 or 1")
  }
  if(length(which(input_data$Age < 1)|which(input_data$Age > max_age)) > 0){
    stop("Age must be between 1 and max_age")
  }
  
# parameters is a matrix of size max_age by max_age
  if(class(parameters)[1] != "matrix"){stop("parameters must be a matrix")}
  if(length(which(dim(parameters) != 
  c(max_age, max_age)) == FALSE)>0){stop("parameters must have dim 
                                                  = max_age by max_age")}
  
# p is a vector of length max_age
  if(length(p) != max_age){stop("length of p must equal max_age")}
  
# IF a seed is defined, it is a number
  if(!is.null(defined_seed)){if(!is.numeric(defined_seed)){stop("seed 
  must be a number")}}

################################################################################
  
## Only work on the focal year

old_data <- input_data %>% filter(Year < i)
  
input_data <- input_data %>% filter(Year == i)
  
## Fill in the survival column of the input_data

## Fill in survival values using probabilities in parameter matrix
  
# make a vector of probabilities based on age of individuals
# remove first row of parameter matrix as this is reproduction
# then add a final 0 as oldest individuals all die
# then index based on age
phi <- c(diag(parameters[-1,]),0)[input_data[ ,"Age"]]

# get survival values using rbinom using the phi vector
# need to ensure column Surv remains numeric
if(!is.null(defined_seed)){set.seed(defined_seed)}
input_data$Surv <- as.numeric(rbinom(n = length(input_data$Surv), 
                           size = 1, 
                           prob = phi))

## Fill in recapture values using probabilities in parameter matrix

# make a vector of probabilities based on age of individuals
# use the vector of p
p <- p[input_data[ ,"Age"]]

# scale by survival - all that die are not recaptured
p[which(input_data$Surv == 0)] <- 0

# get recapture values using rbinom using the phi vector
# need to ensure column Recap remains numeric
if(!is.null(defined_seed)){set.seed(defined_seed)}
input_data$Recap <- as.numeric(rbinom(n = length(input_data$Recap), 
                                     size = 1, 
                                     prob = p))
  
## Return new input_data file
return(rbind(old_data, input_data))
  
}  
