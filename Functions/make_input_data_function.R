# T1.1: Function to make data input for nimble model #

# Takes raw simulated data and reformats into a capture history, observed offspring
# numbers etc

# OUTPUT is input data, inits and constants

################################################################################

make_input_data <- function(simulations,
                            n_occasions){
  
#### Step 1: recode age ####
  
# re-code raw data so that adult = 2, juvenile = 1
output_data <- simulations %>%
  filter(Recap == 1) %>% # only keep those that were recaptured
  mutate(Surv = case_when(Age > 1 ~ 2, 
                          TRUE ~ Recap), 
         Age = case_when(Age == 1 ~ 1,
                         Age > 1 ~ 2)) # change age to just juv (1) and adult (2)

#### Step 2: make a capture history ####

# capture history
capture_history <- output_data %>%
  # spread out data. The fill = 3 fills in 3s when combo was not observed (dead/non detected)
  pivot_wider(id_cols = ID, names_from = Year, values_from = Surv,
              values_fill = 3) %>%
  as.matrix()

#### Step 3: set up state and define 'first' ####

# first is the first capture occasion
# set up initial values for survival states (1 on first occasion and 2 after)
surv_state_init <- capture_history[,2:(n_occasions+1)]
# create a vector of first capture occasions (THIS WILL BE A CONSTANT)
first <- apply(surv_state_init, 1, function(x) which(x<3)[1])
for(i in 1:nrow(surv_state_init)){
  if(first[i] < (n_occasions-1))
    surv_state_init[i, (first[i]+1):n_occasions] <- 2}

#### Step 4: assign offspring and age vectors ####

# need to make a capture history and save out age and offspring columns
offspring_obs <- output_data$Offspring
age <- output_data$Age

#### Step 5: create list of data inputs ####

# store ready for model
data_input <- list(surv_obs = capture_history[,2:(n_occasions+1)],
                   age = age,
                   offspring_obs = offspring_obs)

#### Step 6: create list of constants ####

# number of occasions (occasions) and number of individuals (N) and first 
# capture occasion (first)
constants <- list(N = nrow(data_input$surv_obs), 
                  occasions = n_occasions,
                  first = first)

#### Step 7: remove all individuals first seen on final occasion ####

# for hmm remove any individuals first seen in the final capture occasion
data_input$surv_obs<- data_input$surv_obs[-which(first > (n_occasions-2)), ]

# number of occasions (Occasions) and number of individuals (N)
constants$first <-  first[-which(first > (n_occasions-2))]
constants$N <- nrow(data_input$surv_obs) # redefine N to make new nrow()

#### Step 8: set up inits ####

inits <- list(mean_phi_juv = runif(1, 0, 1),
              mean_phi_adult = runif(1, 0, 1),
              mean_p = runif(1, 0, 1),
              alpha = rnorm(1, 0, 0.1),
              beta_age = rnorm(1, 0, 0.1),
              offspring_state = offspring_obs,
              fecundity_rate = rep(1, length(offspring_obs)))

#### RETURN: list of data, constants, and inits ####

output <- list(data_input, constants, inits)

names(output) <- c("data_input", "constants", "inits")

return(output)
  
}