# T1.1: Function to make constants and inits for nimble model #

# Takes formatted data and creates inits and constants

# OUTPUT is inits and constants

################################################################################


make_constants_inits <- function(data_input,
                            n_occasions){
  
#### Step 1: create constant 'first' ####

# first is the first capture occasion
# set up initial values for survival states (1 on first occasion and 2 after)
surv_state_init <- data_input$surv_obs
# create a vector of first capture occasions (THIS WILL BE A CONSTANT)
first <- apply(surv_state_init, 1, function(x) which(x<3)[1])
for(i in 1:nrow(surv_state_init)){
  if(first[i] < (n_occasions-1))
    surv_state_init[i, (first[i]+1):n_occasions] <- 2}

#### Step 2: create list of constants ####

# number of occasions (occasions) and number of individuals (N) and first 
# capture occasion (first)
constants <- list(N = nrow(data_input$surv_obs), 
                  occasions = n_occasions, 
                  first = first)

#### Step 3: set up inits ####

inits <- list(mean_phi_juv = runif(1, 0, 1),
              mean_phi_adult = runif(1, 0, 1),
              mean_p = runif(1, 0, 1),
              alpha = rnorm(1, 0, 0.1),
              beta_age = rnorm(1, 0, 0.1),
              offspring_state = data_input$offspring_obs,
              fecundity_rate = rep(1, length(data_input$offspring_obs)))

#### RETURN: list of data, constants, and inits ####

return(list(constants, inits))

}