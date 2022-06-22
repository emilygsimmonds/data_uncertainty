# FUNCTION to process input data

## Function cleans previous output data to become new input data

## INPUT :
#
# - output_data =a dataframe with column names:
# ID (factor), Year (factor), Surv (0/1), Recap (0/1), Offspring (num), 
# Age (num), Trait (num)
#
# - i = index for year
#
# - IDs = vector of IDs that can be used for individuals

## OUTPUT = a new dataframe with individuals for next year

#### FUNCTION ####

process_input_data <- function(output_data, 
                             i, IDs) {
  
## Take the previous output data and clean up ready to be input data
  
# remove all with recap = 0
  
output_data <- output_data %>% filter(Recap == 1)
  
## Create new data frame for the next time step
  
# remove all those with surv = 0 and age by 1
# restrict to the focal year
  
input_data_new <- output_data %>% filter(Surv == 1,
                                         Year == i-1) %>%
    mutate(Age = Age + 1,
           Year = Year + 1)
  
# add new individuals based on offspring numbers - all with age = 1

# marker so only consider focal year
marker <- which(output_data$Year == i-1)

# assign IDs, want to remove any that have previously been used 
IDs_new <- sample(setdiff(IDs, output_data$ID), 
                  sum(output_data$Offspring[marker]))
  
offspring <- data.frame(ID = IDs_new,
                        Year = i,
                        Surv = NA,
                        Recap = NA,
                        Offspring = NA,
                        Age = 1,
                        Trait = NA)
  
# assign group and trait values
  
offspring <- offspring %>% mutate(Trait = rnorm(length(offspring$Trait),
                                                  20, 5))
  
## Make final output
  
# combine offspring and surviving adults
  
input_data_new <- rbind(output_data, input_data_new, offspring)
  
## Output new input data file
  
return(input_data_new)
  
}