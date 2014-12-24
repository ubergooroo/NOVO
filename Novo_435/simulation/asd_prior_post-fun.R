##################################################
# this function runs the prior and post balance
# standardized difference in means
##################################################


asd.prior_post<-function(values){
  
  #values <-means_vars #this to test function
  
  if(rownames(values)==2) {
    
    numerator_priorbal<-mapply(function(val, val1){ val - val1 }, values$txd, values$cntlpriorbal)
    numerator_postbal<-mapply(function(val, val1){ val - val1 }, values$txd, values$cntlpostbal)
    
    prior_pooledsd<-mapply(function(val, val1){ sqrt((val*(1-val) + val1*(1-val1) )/2) }, values$txdvar, values$cntlpriorbalvar)
    post_pooledsd<-mapply(function(val, val1){ sqrt((val*(1-val) + val1*(1-val1) )/2) }, values$txdvar, values$cntlpostbalbalvar)
    
    asd_priorbal<-mapply(function(val, val1){ abs(val)/val1 }, numerator_priorbal, prior_pooledsd)
    asd_postbal <-mapply(function(val, val1){ abs(val)/val1 }, numerator_postbal, post_pooledsd)
    
  }
  
  else {
  
  
  numerator_priorbal<-mapply(function(val, val1){ val - val1 }, values$txd, values$cntlpriorbal)
  numerator_postbal<-mapply(function(val, val1){ val - val1 }, values$txd, values$cntlpostbal)
  
  prior_pooledsd<-mapply(function(val, val1){ sqrt((val + val1 )/2) }, values$txdvar, values$cntlpriorbalvar)
  post_pooledsd<-mapply(function(val, val1){ sqrt((val + val1 )/2) }, values$txdvar, values$cntlpostbalbalvar)
  
  asd_priorbal<-mapply(function(val, val1){ abs(val)/val1 }, numerator_priorbal, prior_pooledsd)
  asd_postbal <-mapply(function(val, val1){ abs(val)/val1 }, numerator_postbal, post_pooledsd)
  
  }
  
  return<-c(asd_priorbal, asd_postbal)
  
}