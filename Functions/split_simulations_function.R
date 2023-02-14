# FUNCTION to split simulation files from a list of 100 to 100 files #

################################################################################

# input = filename of the 100 simulations

# output = 100 individual files

################################################################################

#### FUNCTION ####

split_simulations <- function(filename,
                              location){
  
  simulations <- get(load(filename))
  
  simulation_name <- str_sub(basename(filename), 1, -7)
  
  map2(.x = simulations,
       .y = as.list(1:100), ~{
    saveRDS(.x, file = paste0(location, simulation_name, .y, ".RDS"))
  })
  
}