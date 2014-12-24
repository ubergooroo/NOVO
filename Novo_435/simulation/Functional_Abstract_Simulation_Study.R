##############################################################
#
# EB Example Simulate a Target Population
#
##############################################################
options(scipen=999)
options(digits=10)
library(ebal)
library(survey)
library(dplyr)

setwd('C:/Users/ubergooroo/Documents/GitHub/NOVO/Novo_435/simulation')

source("one_sample-fun.R")
source("eb_weighting-fun.R")
source("ipw_weighting-fun.R")
source("sipw_weighting-fun.R")
source("meta_data-fun.R")

# source("ipw_weighting-fun.R")
# source("sopw_weighting-fun.R")

#replicate<(3, {d<-one.sample(); c( eb.weighting(d), IPW(d), sIPW(d) )}) # this is to call the functions once they are created.


out<-replicate(4000, {d<-one.sample();c(eb.weighting(d), ipw.weighting(d), 
                                     sipw.weighting(d), meta.data(d))}) # this is to call the functions once they are created.


eb_prebalance<-out[1:7,]
eb_postbalance<-out[8:14,]

ipw_prebalance<-out[15:21,]
ipw_postbalance<-out[22:28,]

sipw_prebalance<-out[29:35,]
sipw_postbalance<-out[36:42,]

meta_data<-out[43:46,1:3]



##############################################################
# Entropy Balance Results
#############################################################

eb_lessthanten <-NULL
for (i in 1:dim(eb_prebalance)[1]){
  eb_lessthanten[i]<-sum(as.integer(eb_prebalance[i,] < 0.10))
}

eb_prebalance_pct<-format(round(eb_lessthanten / dim(eb_prebalance)[2] * 100,2), nsmall = 2)

eb_lessthanten <-NULL
for (i in 1:dim(eb_postbalance)[1]){
  eb_lessthanten[i]<-sum(as.integer(eb_postbalance[i,] < 0.10))
}

eb_postbalance_pct<-format(round(eb_lessthanten / dim(eb_postbalance)[2] * 100,2), nsmall = 2)



eb_prebalance_medians<-apply(eb_prebalance,1,median)
eb_postbalance_medians<-apply(eb_postbalance,1,median)

eb_prebalance_mads<-apply(eb_prebalance,1,mad)
eb_postbalance_mads<-apply(eb_postbalance,1,mad)


eb_final_output<-data.frame(xcov=names(one.sample())[c(2:5,8:10)], eb_prebalance_medians, eb_postbalance_medians, 
                            eb_prebalance_mads, eb_postbalance_mads, eb_prebalance_pct, eb_postbalance_pct)

eb_final_output

#####################################################################################################################

##############################################################
# IPW Balance Results
#############################################################

ipw_lessthanten <-NULL
for (i in 1:dim(ipw_prebalance)[1]){
  ipw_lessthanten[i]<-sum(as.integer(ipw_prebalance[i,] < 0.10))
}

ipw_prebalance_pct<-format(round(ipw_lessthanten / dim(ipw_prebalance)[2] * 100,2), nsmall = 2)

ipw_lessthanten <-NULL
for (i in 1:dim(ipw_postbalance)[1]){
  ipw_lessthanten[i]<-sum(as.integer(ipw_postbalance[i,] < 0.10))
}

ipw_postbalance_pct<-format(round(ipw_lessthanten / dim(ipw_postbalance)[2] * 100,2), nsmall = 2)



ipw_prebalance_medians<-apply(ipw_prebalance,1,median)
ipw_postbalance_medians<-apply(ipw_postbalance,1,median)

ipw_prebalance_mads<-apply(ipw_prebalance,1,mad)
ipw_postbalance_mads<-apply(ipw_postbalance,1,mad)


ipw_final_output<-data.frame(xcov=names(one.sample())[c(2:5,8:10)], ipw_prebalance_medians, ipw_postbalance_medians, 
                             ipw_prebalance_mads, ipw_postbalance_mads, ipw_prebalance_pct, ipw_postbalance_pct)

ipw_final_output

#####################################################################################################################


##############################################################
# IPW Balance Results
#############################################################

sipw_lessthanten <-NULL
for (i in 1:dim(sipw_prebalance)[1]){
  sipw_lessthanten[i]<-sum(as.integer(sipw_prebalance[i,] < 0.10))
}

sipw_prebalance_pct<-format(round(sipw_lessthanten / dim(sipw_prebalance)[2] * 100,2), nsmall = 2)

sipw_lessthanten <-NULL
for (i in 1:dim(sipw_postbalance)[1]){
  sipw_lessthanten[i]<-sum(as.integer(sipw_postbalance[i,] < 0.10))
}

sipw_postbalance_pct<-format(round(sipw_lessthanten / dim(sipw_postbalance)[2] * 100,2), nsmall = 2)



sipw_prebalance_medians<-apply(sipw_prebalance,1,median)
sipw_postbalance_medians<-apply(sipw_postbalance,1,median)

sipw_prebalance_mads<-apply(sipw_prebalance,1,mad)
sipw_postbalance_mads<-apply(sipw_postbalance,1,mad)


sipw_final_output<-data.frame(xcov=names(one.sample())[c(2:5,8:10)], sipw_prebalance_medians, sipw_postbalance_medians, 
                             sipw_prebalance_mads, sipw_postbalance_mads, sipw_prebalance_pct, sipw_postbalance_pct)

sipw_final_output

#####################################################################################################################
  

  
  
  
  
  


                                  
          
         




