# T1.1: Function to make data input for nimble model #

# Takes raw simulated data and reformats into a capture history, observed offspring
# numbers etc

# need to define if there is fecundity error or not - determines which offspring
# column to use

# OUTPUT is input data, inits, parameters to save, and constants

################################################################################

make_input_data <- function(simulations,
                            n_occasions,
                            fecundity_error = FALSE,
                            stages,
                            reproduction_data = NULL){
  
#### Step 1: recode age - should all be as stage now ####
   
# re-code raw data so that all stages are numbers
output_data <- simulations %>%
  mutate(Age = factor(as.factor(Stage),
                      level = stages)) %>% # make sure it is factor in order of stages
  mutate(Age = as.numeric(Age))

if(!is.null(reproduction_data)){
  # re-code raw data so that all stages are numbers
  reproduction_data <- reproduction_data %>%
    mutate(Age = factor(as.factor(Stage),
                        level = stages)) %>% # make sure it is factor in order of stages
    mutate(Age = as.numeric(Age))
}

#### Step 2: make a capture history ####

# capture history
capture_history <- output_data %>%
  # spread out data. The fill = 1 number above number of stages when combo was not observed (dead/non detected)
  pivot_wider(id_cols = ID, names_from = Year, values_from = Age,
              values_fill = (length(stages)+1)) %>%
  as.matrix()

#### Step 3: set up state and define 'first' ####

# ages
ages <- unique(output_data$Age)

# first is the first capture occasion
# set up initial values for survival states (1 on first occasion and 2 after)
surv_state_init <- capture_history[,2:(n_occasions+1)]
# create a vector of first capture occasions (THIS WILL BE A CONSTANT)
first <- apply(surv_state_init, 1, function(x) which(x<(length(stages)+1))[1]) 
# loop to fill in stages after first one
for(i in 1:nrow(surv_state_init)){
  if(first[i] < (n_occasions-1)){ # if first observation was before the penultimate occasion
    start_age <- surv_state_init[i,first[i]] # take the first age that an individual is recorded
    # check whether number of cells between first obs and end of study > number of ages left
    if((n_occasions - first[i]) < length(ages)-start_age)
      {end_age <- length(ages) - ((length(ages)-start_age)-(n_occasions - first[i]))} 
    else{end_age <- length(ages)
    surv_state_init[i, 
                    (first[i]+(end_age-start_age)):n_occasions] <- ages[end_age]} # then fill max age to the end
    surv_state_init[i,
                    first[i]:(first[i]-start_age+end_age)] <- ages[start_age:end_age] # fill in the next ages up to max age 
}} 

#### Step 4: assign offspring and age vectors ####

# if independent reproduction data is supplied then use that for offspring model
if(is.null(reproduction_data)){
# need to make a capture history and save out age and offspring columns
offspring_obs <- output_data$Offspring
age <- output_data$Age}else{
  offspring_obs <- reproduction_data$Offspring
  age <- reproduction_data$Age 
}

if(length(stages) == 3){
subadult <- which(age == 2) #index of subadults
adult <- which(age == 3) #index of adults
}

if(length(stages) == 5){
  subadult <- which(age == 2) #index of subadults
  adult1 <- which(age == 3) #index of adult1
  adult2 <- which(age == 4) #index of adult2
  adult3 <- which(age == 5) #index of adult3
}

# IF fecundity error is present, use offspring_obs column
if(fecundity_error == TRUE){offspring_obs <- output_data$Offspring_obs}

#### Step 5: create list of data inputs ####

# store ready for model
data_input <- list(surv_obs = capture_history[,2:(n_occasions+1)],
                   age = age,
                   offspring_obs = offspring_obs)

if(length(stages) == 3){
data_input <- list(surv_obs = capture_history[,2:(n_occasions+1)],
                   age = age,
                   offspring_obs_sa = offspring_obs[subadult],
                   offspring_obs_a = offspring_obs[adult]) 
}

if(length(stages) == 5){
  data_input <- list(surv_obs = capture_history[,2:(n_occasions+1)],
                     age = age,
                     offspring_obs_sa = offspring_obs[subadult],
                     offspring_obs_a1 = offspring_obs[adult1],
                     offspring_obs_a2 = offspring_obs[adult2],
                     offspring_obs_a3 = offspring_obs[adult3]) 
}

#### Step 6: create list of constants ####

# number of occasions (occasions) and number of individuals (N) and first 
# capture occasion (first)
constants <- list(N = nrow(data_input$surv_obs), 
                  occasions = n_occasions,
                  first = first,
                  O_N = length(offspring_obs))

if(length(stages) == 3){
constants <- list(N = nrow(data_input$surv_obs), 
                  occasions = n_occasions,
                  first = first,
                  O_N_sa = length(subadult),
                  O_N_a = length(adult))  
}

if(length(stages) == 5){
  constants <- list(N = nrow(data_input$surv_obs), 
                    occasions = n_occasions,
                    first = first,
                    O_N_sa = length(subadult),
                    O_N_a1 = length(adult1),
                    O_N_a2 = length(adult2),
                    O_N_a3 = length(adult3))  
}

#### Step 7: remove all individuals first seen on final occasion ####

# for hmm remove any individuals first seen in the final capture occasion
data_input$surv_obs<- data_input$surv_obs[-which(first > (n_occasions-2)), ]

# number of occasions (Occasions) and number of individuals (N)
constants$first <-  first[-which(first > (n_occasions-2))]
constants$N <- nrow(data_input$surv_obs) # redefine N to make new nrow()
constants$stage_length <- length(stages)

#### Step 8: set up inits ####

inits <- list(mean_phi = runif(length(stages), 0, 1),
              mean_p = runif(length(stages), 0, 1),
              alpha = rnorm(1, 0, 0.1),
              beta_age = rnorm(1, 0, 0.1),
              fecundity_rate = rep(1, length(offspring_obs)))

if(length(stages) == 3){
  inits <- list(mean_phi = runif(length(stages), 0, 1),
                mean_p = runif(length(stages), 0, 1),
                beta_age = c(rnorm(1, 0, 0.1), 
                             rnorm(1, 0, 0.1)),
                fecundity_rate_sa = rep(1, length(offspring_obs[subadult])),
                fecundity_rate_a = rep(1, length(offspring_obs[adult])))  
}

if(length(stages) == 5){
  inits <- list(mean_phi = runif(length(stages), 0, 1),
                mean_p = runif(length(stages), 0, 1),
                beta_age = c(rnorm(1, 0, 0.1), 
                             rnorm(1, 0, 0.1),
                             rnorm(1, 0, 0.1),
                             rnorm(1, 0, 0.1)),
                fecundity_rate_sa = rep(1, length(offspring_obs[subadult])),
                fecundity_rate_a1 = rep(1, length(offspring_obs[adult1])),
                fecundity_rate_a2 = rep(1, length(offspring_obs[adult2])),
                fecundity_rate_a3 = rep(1, length(offspring_obs[adult3])))  
}

#### Step 9: edit parameters to save

parameters_to_save <- c("mean_phi",
                        "mean_p",
                        "alpha",
                        "beta_age",
                        "transition_matrix",
                        "lambda" 
)

if(length(stages) == 3|
   length(stages) == 5){
  parameters_to_save <- c("mean_phi",
                          "mean_p",
                          "beta_age",
                          "transition_matrix",
                          "lambda" 
  ) 
}

for(j in stages){
  parameters_to_save <- c(parameters_to_save,
                          paste0("reproduction_", j),
                          paste0("recapture_", j),
                          paste0("survival_", j))
}

#### RETURN: list of data, constants, and inits ####

output <- list(data_input, constants, inits, parameters_to_save)

names(output) <- c("data_input", "constants", "inits", 
                   "parameters_to_save")

return(output)
  
}