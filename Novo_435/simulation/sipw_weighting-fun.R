###############################################
# This performes stabalized IPW reweighting
###############################################
sipw.weighting<-function(sample){
  
#   sample<-one.sample() #this to test function
  
  # estimation of denominator of ip weights  
  denom.fit <- glm(GROUP ~ YEAR + FEMALE +  AGE + BMI + I(AGE^2) + I(BMI^2) +I(YEAR^2), family = binomial(), data = sample)
  # now this is my selection probability               
  denom.p <- predict(denom.fit, type = "response")
  
  
  
  # estimation of numerator of ip weights
  numer.fit <- glm(GROUP~1, family = binomial(), data = sample)
  # now this is my selection probability  
  numer.p <- predict(numer.fit, type = "response")
  
  sample$new_wt <- ifelse(sample$GROUP == 0, ((1-numer.p)/(1-denom.p)), (numer.p/denom.p))
                     
  #dplyr here to update the new_wt 
  
  source("setup_asd-fun.R")
  setup.asd(sample) #call the absolute difference set up function
  
}







