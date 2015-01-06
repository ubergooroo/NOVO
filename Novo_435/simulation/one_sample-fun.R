###########################################
# this function pulls one sample from the 
# superpopulation that you define.
###########################################

expit<-function(eta) exp(eta)/(1+exp(eta)) #this is the inverse of the exponential function

population<-data.frame(x=rnorm(10000))
population$BMI<-round(abs(rnorm(10000)*10)+18.5,2); mean(population$BMI)
population$FEMALE<-ifelse(rbinom(10000, 1, 0.53)>=.50, 1, 0)
population$AGE<-round(rnorm(10000)*10+47,0);mean(population$AGE)
population$YEAR<-round(abs(rnorm(10000))+5,2);mean(population$YEAR)

superpop<-data.frame(BMI=population$BMI,FEMALE=population$FEMALE,AGE=population$AGE, YEAR=population$YEAR, AGE2=population$AGE2<-(population$AGE)^2, 
                  BMI2=population$BMI2<-(population$BMI)^2, YEAR2=population$YEAR2<-(population$YEAR)^2)

population$GROUP<-rbinom(10000, 1, expit(population$x-1))
population$wt <-ifelse(population$GROUP==0, 1, (10000-sum(population$GROUP))/3000)
population$BMI<-ifelse(population$GROUP==0, population$BMI+3.5, population$BMI)
population$AGE<-ifelse(population$GROUP==0, population$AGE+3.0, population$AGE)
population$AGE2<-(population$AGE)^2
population$BMI2 <-(population$BMI)^2
population$YEAR2<-(population$YEAR)^2
population$PSU<-as.numeric(rownames(population))


one.sample<-function(){
  target<-which(population$GROUP==1)
  controls<-sample(which(population$GROUP==0),3000)
  population[c(target, controls),]
}


the.population<-function(){
  return<-t(as.matrix(apply(superpop ,2,mean))) # 1*8 the weighted treated means
}



