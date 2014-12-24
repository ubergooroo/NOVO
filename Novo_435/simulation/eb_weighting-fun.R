###############################################
# This performes Entropy Balancing reweighting
###############################################

eb.weighting<-function(sample){
  
#   sample<-one.sample() #this to test function
  
  # construct treated target (I tried to circumvent the problem by creating one fake treated
  #-- which is weighted average of the treatment group )
  treated<-sample[which(sample$GROUP==1),]
  target<-t(as.matrix(apply(treated,2,weighted.mean,treated$wt))) # 1*8 the weighted treated means
  
  # the control data
  control<-sample[which(sample$GROUP==0),]
  
  # put the fake treated (target) and the control together
  d<-rbind(target,target,control) # the reason I replicate the target is because there is a bug when there is only one treated unit
  head(d)
  table(d$GROUP)
  
  # define covariates to be matched on
  X<-c("YEAR","YEAR2","FEMALE","AGE","AGE2","BMI","BMI2")
  
  out<-ebalance(d$GROUP,d[,X])
  #plot(density(out$w),xlim=c(0,0.002))
  
  
  #dplyr here to update the new_wt 
  
  treated$new_wt <-treated$wt
  control$new_wt <-out$w

  rbind(treated,control)
  
  source("setup_asd-fun.R")
  setup.asd(rbind(treated,control)) #call the absolute difference set up function
  
}

