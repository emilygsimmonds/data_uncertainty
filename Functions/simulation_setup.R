# FUNCTION to set up simulation #

################################################################################

## takes matrix of vital rates, and stages and gives input_data for simulation

## OUTPUT :
#
# - input_data =a dataframe with column names:
# ID (factor), Year (factor), Surv (0/1), Offspring (num), 
# Stage (num), Trait (num)
#

#### FUNCTION ####

simulation_setup <- function(parameter_matrix, 
                             stages,
                             i){

# set up clause to edit starting population size if lambda < 0.8
  
start_size <- 200

# calculate stable stage distribution
stage_prop <- round((eigen(parameter_matrix)$vectors[,1]/
  sum(eigen(parameter_matrix)$vectors[,1]))*100)
stages_sample <- rep(stages, stage_prop)

# name parameter matrix rows and cols
rownames(parameter_matrix) <- stages
colnames(parameter_matrix) <- stages

set.seed(i)  
Stage <- sample(stages_sample, start_size, replace = TRUE)

set.seed(i) 
Offspring <- rpois(start_size, parameter_matrix[1, Stage])



set.seed(i) 
# as there will be no two survival elements per column - sum them
# this way you can select by stage regardless of how many stages
Surv <- rbinom(start_size, 1, 
               prob = sum(parameter_matrix[-1, Stage]))

set.seed(i)
input_data <- data.frame(ID = sample(1:start_size, start_size, replace = FALSE),
                          Year = 1,
                          Surv = Surv,
                          Offspring = Offspring,
                          Stage = Stage,
                          Trait = rnorm(start_size, 20, 5))


return(input_data)

}

