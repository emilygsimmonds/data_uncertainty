# find probability of at least 1 success in 5 
# given probability of recap and survival

phi_j = 0.3
phi_a = 0.5

p = 0.8

# captured year 1
year1 <- dbinom(1, 1, 0.8) # probability of initial capture at 1 year

# not captured year 1 but captured year 2
year2 <- prod(dbinom(0, 1, 0.8),
  dbinom(1, 1, (0.8*0.3))) # probability of surviving to year 2 and capture

# not captured year 1 or 2 but captured year 3-5
year3 <- prod(dbinom(0, 1, 0.8),
              dbinom(0, 1, (0.8*0.3)),
              sum(dbinom(1:3, 3, (0.8*0.5)))) # probability of surviving beyond year 2 and capture

total_prob <- sum(year1, year2, year3) # 0.97   
