# FUNCTION to expand a 2x2 matrix to larger size #

################################################################################

## Function takes rates from 2x2 matrix and expands to 3x3 or 5x5

## INPUT = 2x2 matrix

## OUTPUT = 3x3 or 5x5 stage matrix

#### load packages

library(tidyverse)

################################################################################

#### FUNCTION ####

expand_matrix <- function(input_matrix,
                          size){
  
  # first make the output matrix
  output_matrix <- matrix(0, nrow = size, ncol = size)
  
  # fill in fecundity
  if(size == 3){output_matrix[1,] <- c(0, input_matrix[1,])}
  else{output_matrix[1,] <- c(0, input_matrix[1,c(1,2,2,2)])}# 5x5 has 4 breeding stages
  
  # now survival
  if(size == 3){
  output_matrix[2,1] <- input_matrix[2,1]
  output_matrix[3,2] <- input_matrix[2,2]
  output_matrix[3,3] <- input_matrix[2,2]}
  else{
  output_matrix[2,1] <- input_matrix[2,1]
  output_matrix[3,2] <- input_matrix[2,2]
  output_matrix[4,3] <- input_matrix[2,2]
  output_matrix[5,4] <- input_matrix[2,2]
  output_matrix[5,5] <- input_matrix[2,2]}
  
  # checks - if lambda < 1 boost final stages survival and recruitment by 10%
  if(as.numeric(eigen(output_matrix)$values[1]) < 1){
    output_matrix[size, (size-1):size] <- output_matrix[size,(size-1):size]*1.1
    output_matrix[1, (size-1):size] <- output_matrix[1,(size-1):size]*1.1
  }
  
  return(output_matrix)
  
}