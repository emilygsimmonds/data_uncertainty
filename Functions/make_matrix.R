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
  output_matrix <- matrix(data = rep(0, (length(stages)+1)^2), 
                          nrow = length(stages)+1,
                          ncol = length(stages)+1)
  rownames(output_matrix) <- c(stages, "dead")
  colnames(output_matrix) <- c(stages, "dead")
  
  # now fill in each cell: loop across the stages
  
for(i in stages){
    
    marker <- which(stages == i) # reference point in matrix
    
    stage_marker <- which(frequency_table$stage == i) # where in table is that stage?
    
    # any stasis
    output_matrix[marker,marker] <- length(which(frequency_table[stage_marker,"fate"] == i))
    # survival to next stage
    output_matrix[marker+1,marker] <- length(which(frequency_table[stage_marker,"fate"] == 
                                                     rownames(output_matrix)[marker+1]))
    # dead
    output_matrix["dead",marker] <- length(which(frequency_table[i,"fate"] == "dead"))
    
}
  
  # now calculate proportions
  transition_matrix <- prop.table(output_matrix, 2)[-(length(stages)+1),
                                                    -(length(stages)+1)]
  
  # then loop for fertility
  
for(i in stages){
  
  marker <- which(stages == i) # reference point in matrix
  
  # mean fertility of stage i
  transition_matrix[1,marker] <- transition_matrix[1,marker]+
    sum(frequency_table[i,"fertility"])/
    length(frequency_table[i,"fertility"])

 }
  
# return transition matrix - dead column and row
  
  return(transition_matrix)
}