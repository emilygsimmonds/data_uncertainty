# SCRIPT TO PULL MATRICES FROM COMADRE #

#### Set up ####

# load packages

library(tidyverse)

#install.packages("Rcompadre")

library(Rcompadre)
library(dplyr)
library(popdemo)
library(popbio)
library(maps)     # for plotting world map

#### CHOOSE DATA SIMULATION MATRICES ####

load("./Data files/working_comadre.RData")

## subset to right size: size 2 x 2 for data uncertainty simulations

# which matrices have size = 2?
comadre_2 <- comadre_combined %>% filter(MatrixDimension == 2)

# extract matrices
matrices_data <- matA(comadre_2)

# then calculate lambda for all
# while also flagging any without juvenile reproduction

summary_matrices <- map2_df(.x = matrices_data,
                            .y = as.list(seq(1, length(matrices_data), 1)), ~{ 
                              
                              lambda <- eigen(.x)$value[1]
                              index <- .y
                              juvenile_check <- .x[1,1] > 0
                              sum_fecundity <- sum(.x[1,])
                              
                              return(data.frame(lambda = lambda, 
                                                matrix_number = index,
                                                juvenile_check = juvenile_check,
                                                sum_fecundity = sum_fecundity))
                            })

# sort to get only matrices with lambda >1 but < 1.2
summary_matrices <- filter(summary_matrices, 
                           lambda >= 1 & lambda <= 1.2 &
                             juvenile_check == TRUE)

# gave 10 matrices, now order by sum of fecundity
arrange(summary_matrices, sum_fecundity)

# take every other matrix
matrices_22 <- arrange(summary_matrices, sum_fecundity)$matrix_number[seq(1,10,2)]

#### extract the matrices

output_matrices <- matrices_data[matrices_22]

save(output_matrices, file = "twobytwo_matrices.RData")

################################################################################

