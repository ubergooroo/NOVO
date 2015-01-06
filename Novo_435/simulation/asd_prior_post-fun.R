##################################################
# this function runs the prior and post balance
# standardized difference in means
##################################################


asd.prior_post<-function(values,values2){
  
  #values <-means_vars #this to test function
  
  #this is for the dichotomous variable (female in this example) code is terrible but works.                                                                            
  numerator_priorbal<-mapply(function(val, val1){ val - val1 }, values$txd[2], values$cntlpriorbal[2])
  numerator_postbal<-mapply(function(val, val1){ val - val1 }, values$txd[2], values$cntlpostbal[2])
  
  prior_pooledsd<-mapply(function(val, val1){ sqrt((val*(1-val) + val1*(1-val1) )/2) }, values$txdvar[2], values$cntlpriorbalvar[2])
  post_pooledsd<-mapply(function(val, val1){ sqrt((val*(1-val) + val1*(1-val1) )/2) }, values$txdvar[2], values$cntlpostbalbalvar[2])
  
  asd_priorbal_dict<-mapply(function(val, val1){ abs(val)/val1 }, numerator_priorbal, prior_pooledsd)
  asd_postbal_dict <-mapply(function(val, val1){ abs(val)/val1 }, numerator_postbal, post_pooledsd)
  
  
  #this is for the continuous and ordinal variables
  numerator_priorbal<-mapply(function(val, val1){ val - val1 }, values$txd, values$cntlpriorbal)
  numerator_postbal<-mapply(function(val, val1){ val - val1 }, values$txd, values$cntlpostbal)
  
  prior_pooledsd<-mapply(function(val, val1){ sqrt((val + val1 )/2) }, values$txdvar, values$cntlpriorbalvar)
  post_pooledsd<-mapply(function(val, val1){ sqrt((val + val1 )/2) }, values$txdvar, values$cntlpostbalbalvar)
  
  asd_priorbal<-mapply(function(val, val1){ abs(val)/val1 }, numerator_priorbal, prior_pooledsd)
  asd_postbal <-mapply(function(val, val1){ abs(val)/val1 }, numerator_postbal, post_pooledsd)
  
  asd_priorbal[2]<-asd_priorbal_dict #now put the dichotomous result back into the vector of results
  asd_postbal[2] <-asd_postbal_dict  #now put the dichotomous result back into the vector of results
  
  
#   return<-c(asd_priorbal, asd_postbal)
  
  return<-c(asd_priorbal, asd_postbal,as.vector(c(values2[1,2:7], values2[2,2:7], values2[3,2:7], values2[4,2:7] 
                                                  , values2[5,2:7], values2[6,2:7], values2[7,2:7]) , mode='numeric'))


  
}