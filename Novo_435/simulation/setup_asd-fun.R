#############################################################
# function to run the absolute standardized difference
#############################################################

setup.asd<-function(values) {
  
  source("asd_prior_post-fun.R")
  
  #     values <-rbind(treated,control) #this to test function
  
  SIM_NHANES<-values[which(values$GROUP==1),]
  SIM_HUM   <-values[which(values$GROUP==0),]
  
  design_NHANES <-svydesign(id=~PSU,  weights=~wt, nest=TRUE,data=SIM_NHANES)
  design_HUM    <-svydesign(id=~PSU,  weights=~new_wt, nest=TRUE,data=SIM_HUM)
  design_RAWHUM <-svydesign(id=~PSU,  weights=~wt, nest=TRUE,data=SIM_HUM)
  
  # weighted means in treatment group data
  txd<-svymean(~BMI+FEMALE+AGE+YEAR+AGE2+BMI2+YEAR2,design_NHANES)
  
  # weighted variance in treatment group data
  txdvar<-svyvar(~BMI+FEMALE+AGE+YEAR+AGE2+BMI2+YEAR2,design_NHANES)
  
  # means in reweighted control group data
  cntlpostbal<-svymean(~BMI+FEMALE+AGE+YEAR+AGE2+BMI2+YEAR2,design_HUM)
  
  # variance in post balance control group data
  cntlpostbalbalvar<-svyvar(~BMI+FEMALE+AGE+YEAR+AGE2+BMI2+YEAR2,design_HUM)
  
  # means in raw data control group data
  cntlpriorbal<-svymean(~BMI+FEMALE+AGE+YEAR+AGE2+BMI2+YEAR2,design_RAWHUM)
  
  # variance in raw data control group data
  cntlpriorbalvar<-svyvar(~BMI+FEMALE+AGE+YEAR+AGE2+BMI2+YEAR2,design_RAWHUM)
  
  rownames(cntlpriorbalvar)
  
  means_vars<-data.frame(xcov=rownames(cntlpriorbalvar), txd=txd[1:7], txdvar=sqrt(diag(txdvar)[1:7]), cntlpostbal=cntlpostbal[1:7], cntlpostbalbalvar=sqrt(diag(cntlpostbalbalvar)[1:7]),
                         cntlpriorbal=cntlpriorbal[1:7],cntlpriorbalvar=sqrt(diag(cntlpriorbalvar)[1:7]))
  
  means_stders<-data.frame(xcov=rownames(cntlpriorbalvar), txd=txd, cntlpostbal=cntlpostbal, cntlpriorbal=cntlpriorbal)
  
  asd.prior_post(means_vars,means_stders) #call the prior weighting asd 
  

}
  