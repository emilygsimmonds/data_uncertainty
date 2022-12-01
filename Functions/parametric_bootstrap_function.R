#### T1.1: Function to parametric bootstrap ####

# Takes estimated mean and bootstraps to get a confidence interval

################################################################################

#### load packages ####

library(popbio)
library(MCMCvis)
library(HMMpa)

# input is dataframe of means and scenarios, and a baseline model output

# output is confidence intervals for reproduction and lambda

# need to take the estimated mean and credible interval for baseline survival

# use Beta for survival (betaval in popbio) and log norm for reproduction

parametric_bootstrap_function <- function(reproduction_data,
                                          baseline_model_results){
  
  r_juv <- lnorms(1, reproduction_data$mean_juvenile, reproduction_data$var_juvenile)
  r_ad <- lnorms(1, reproduction_data$mean_adult, 
                 reproduction_data$var_adult)
  
  s_juv <- betaval(baseline_model_results["mean_phi_juv", "mean"],
                   baseline_model_results["mean_phi_juv", "sd"])
  s_ad <- betaval(baseline_model_results["mean_phi_adult", "mean"],
                   baseline_model_results["mean_phi_adult", "sd"])
  
  transition_matrix <- matrix(c(r_juv, r_ad, s_juv, s_ad),
                              nrow = 2,
                              ncol = 2,
                              byrow = TRUE)

  lambda <- eigen(transition_matrix)$values[1]
  
  return(data.frame(r_juv = r_juv, 
                    r_ad = r_ad,
                    s_juv = s_juv,
                    s_ad = s_ad,
                    lambda = lambda))
}


