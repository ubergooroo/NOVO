###############################################
# This performes IPW reweighting
###############################################
ipw.weighting<-function(sample){
  
#   sample<-one.sample() #this to test function
  
  # Estimation of ip weights via a logistic model
  fit <- glm(GROUP ~ YEAR + FEMALE + COST + AGE + BMI + I(AGE^2) + I(BMI^2) + I(YEAR^2), weights =wt, family = binomial(), data = sample)
  
  # now this is my selection probability
  PRselection <- ifelse(sample$GROUP == 0, 1 - predict(fit, type = "response"), predict(fit, type = "response"))
  
  # now this is the inverse probability weight which is the inverse of the selection probability
  sample$new_wt <- 1/PRselection
  
  #dplyr here to update the new_wt 
  
  source("setup_asd-fun.R")
  setup.asd(sample) #call the absolute difference set up function
  
}

