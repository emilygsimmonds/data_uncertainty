# FUNCTION to calculate proportions for a pop matrix #

################################################################################

## Function fills in the proportions for a matrix

## INPUT = transition frequency table, vector of stages
# needs to have named columns: stage, fate, and fertility

## OUTPUT = stage matrix

#### load packages

library(tidyverse)

################################################################################

#### FUNCTION ####

make_matrix <- function(frequency_table,
                        stages = c("juvenile", "adult")){
  
  # want to fill in a table with absolute numbers of transitions
  
  # first set up blank matrix
  output_matrix <- matrix(data = rep(0,9), nrow = length(stages)+1,
                          ncol = length(stages)+1)
  rownames(output_matrix) <- c(stages, "dead")
  colnames(output_matrix) <- c(stages, "dead")
  
  # now fill in each cell
  stage1 <- which(frequency_table$stage == stages[1])
  stage2 <- which(frequency_table$stage == stages[2])
  
  # any stage1-stage1 stasis
  output_matrix[1,1] <-  length(which(frequency_table[stage1,"fate"] == stages[1]))
  # survival of stage1 to stage2
  output_matrix[2,1] <- length(which(frequency_table[stage1,"fate"] != "dead"))
  # survival of stage2
  output_matrix[2,2] <- length(which(frequency_table[stage2,"fate"] != "dead"))
  # dead - only overwrite those that should not be 0
  output_matrix[3,1] <- length(which(frequency_table[stage1,"fate"] == "dead"))
  output_matrix[3,2] <- length(which(frequency_table[stage2,"fate"] == "dead"))
  
  # now calculate proportions
  transition_matrix <- prop.table(output_matrix, 2)
  
  # mean fertility of stage1
  transition_matrix[1,1] <- transition_matrix[1,1]+sum(frequency_table[stage1,"juvenile"])/
    length(frequency_table[stage1,"juvenile"])
  # fertility of stage2
  transition_matrix[1,2] <- sum(frequency_table[stage2,"juvenile"])/
    length(frequency_table[stage2,"juvenile"])
  
# return transition matrix - dead column and row
  
  return(transition_matrix[-3,-3])
}