# FUNCTION to run the simulation #

################################################################################

## Function applies the survival and reproduction functions to input data
## Runs as a loop

## INPUT :
#
# - input_data =a dataframe with column names:
# ID (factor), Year (factor), Surv (0/1), Offspring (num), 
# Age (num), Trait (num)
#
# - parameters = matrix of parameter values (transition matrix) inc phi, f
# 
# - max_age = maximum age species can get to
# 
# - inc_trait = TRUE or FALSE if you want to include a trait as well
#
# - IDs = vector of IDs to be used for new individuals (should be longer than 
# ßneeded)
#
# - defined_seed = if repeatable analysis is desired, specify a numeric seed
#
# - start_i and end_i = start and end points of loop


## OUTPUT = filled in dataframe for this year

#### FUNCTION ####

run_simulation_state <- function(input_data_old, 
                             parameters = matrix(data = c(0.5, 0.5, 0.3, 0.7),
                                                 nrow = 2, byrow = TRUE), 
                             stages = c("juvenile", 
                                        "adult"),
                             inc_trait = FALSE,
                             start_i, end_i, IDs,
                             defined_seed = NULL) {
  
## Source necessary functions
source("./Functions/survival_function.R")
source("./Functions/reproduction_function.R")
source("./Functions/process_input_data.R")
  
for(i in start_i:end_i){

## Edit the previously output data to be new input data
input_data <- process_input_data(output_data = input_data_old,
                                   i = i, IDs = IDs,
                                 stages = stages)
  
## Take the input data and apply the survival and reproduction functions
  
output_data <- input_data %>% 
  survival_function(parameters = parameters,
                    stages = stages, 
                    inc_trait = inc_trait,
                    defined_seed = defined_seed, i = i) %>%
  reproduction_function(parameters = parameters, 
                        stages = stages,
                        inc_trait = inc_trait,
                        defined_seed = defined_seed, i = i)
  
## Clean output_data
  
# remove all with recap = 0 DON'T as will look like they've died 
# output_data <- output_data %>% filter(Recap == 1)

input_data_old <- output_data

}
  
## Duplicate check
  
check <- output_data %>% group_by(ID,Year) %>% summarise(count = n())

marker <- which(check$count > 1)

if(length(marker) > 0){stop("duplicate individuals produced")}
  
## Output simulated data 

return(output_data)
  
}