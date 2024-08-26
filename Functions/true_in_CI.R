# short extra function for calculating if true in CI

# input = row of data
# output = TRUE/FALSE

true_in_CI <- function(input){
  
  input <- round(as.numeric(input),2)
  
  between(input[2], 
          input[4], 
          input[5])
}
