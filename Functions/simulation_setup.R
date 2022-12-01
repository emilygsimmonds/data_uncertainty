# FUNCTION to set up simulation #

################################################################################

## takes matrix of vital rates, and max age and gives input_data for simulation

## OUTPUT :
#
# - input_data =a dataframe with column names:
# ID (factor), Year (factor), Surv (0/1), Offspring (num), 
# Age (num), Trait (num)
#

#### FUNCTION ####

simulation_setup <- function(parameter_matrix, max_age,
                             i){

set.seed(i)  
Age <- sample(1:max_age, 200, replace = TRUE)

Offspring <- rpois(200, parameter_matrix[1,Age])

if(max_age > 2){
phi <- c(diag(parameter_matrix[-1,]),0)[Age]}else{phi <- parameter_matrix[2, Age]}

set.seed(i)
input_data <- data.frame(ID = sample(1:200, 200, replace = FALSE),
                          Year = 1,
                          Surv = rbinom(200, 1, 
                                        prob = phi),
                          Offspring = Offspring,
                          Age = Age,
                          Trait = rnorm(200, 20, 5))

# make sure Surv = 0 for all of max age

input_data[which(input_data$Age == max_age), c("Surv")] <- 0
  

return(input_data)

}

