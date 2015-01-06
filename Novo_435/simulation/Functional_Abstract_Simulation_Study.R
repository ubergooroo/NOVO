##############################################################
#
# EB Example Simulate a Target Population
#
##############################################################
  
options(scipen=999)
library(ebal)
library(survey)
library(dplyr)

set.seed(143)

setwd('C:/Users/ubergooroo/Documents/GitHub/NOVO/Novo_435/simulation')

source("one_sample-fun.R")
source("eb_weighting-fun.R")
source("ipw_weighting-fun.R")
source("sipw_weighting-fun.R")
source("meta_data-fun.R")


out<-replicate(4000, {d<-one.sample();c(eb.weighting(d), ipw.weighting(d), 
                                        sipw.weighting(d), meta.data(d))}) # this is to call the functions once they are created.


eb_prebalance<-out[1:7,]
eb_postbalance<-out[8:14,]

eb_bmi<-out[15:20,] #bmi mean and standard error from (target, control post balance, control pre balance)
eb_female<-out[21:26,] #female mean and standard error from (target, control post balance, control pre balance)
eb_age<-out[27:32,] #age mean and standard error from (target, control post balance, control pre balance)
eb_year<-out[33:38,] #year mean and standard error from (target, control post balance, control pre balance)
eb_age2<-out[39:44,] #age2 mean and standard error from (target, control post balance, control pre balance)
eb_bmi2<-out[45:50,] #bmi2 mean and standard error from (target, control post balance, control pre balance)
eb_year2<-out[51:56,] #year2 mean and standard error from (target, control post balance, control pre balance)

ipw_prebalance<-out[57:63,]
ipw_postbalance<-out[64:70,]

ipw_bmi<-out[71:76,] #bmi mean and standard error from (target, control post balance, control pre balance)
ipw_female<-out[77:82,] #female mean and standard error from (target, control post balance, control pre balance)
ipw_age<-out[83:88,] #age mean and standard error from (target, control post balance, control pre balance)
ipw_year<-out[89:94,] #year mean and standard error from (target, control post balance, control pre balance)
ipw_age2<-out[95:100,] #age2 mean and standard error from (target, control post balance, control pre balance)
ipw_bmi2<-out[101:106,] #bmi2 mean and standard error from (target, control post balance, control pre balance)
ipw_year2<-out[107:112,] #year2 mean and standard error from (target, control post balance, control pre balance)

sipw_prebalance<-out[113:119,]
sipw_postbalance<-out[120:126,]

sipw_bmi<-out[127:132,] #bmi mean and standard error from (target, control post balance, control pre balance)
sipw_female<-out[133:138,] #female mean and standard error from (target, control post balance, control pre balance)
sipw_age<-out[139:144,] #age mean and standard error from (target, control post balance, control pre balance)
sipw_year<-out[145:150,] #year mean and standard error from (target, control post balance, control pre balance)
sipw_age2<-out[151:156,] #age2 mean and standard error from (target, control post balance, control pre balance)
sipw_bmi2<-out[157:162,] #bmi2 mean and standard error from (target, control post balance, control pre balance)
sipw_year2<-out[163:168,] #year2 mean and standard error from (target, control post balance, control pre balance)

meta_data<-out[169:172,]; 

meta_data[1:4]



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


eb_final_output<-data.frame(xcov=names(one.sample())[c(2:8)], eb_prebalance_medians, eb_postbalance_medians, 
                            eb_prebalance_mads, eb_postbalance_mads, eb_prebalance_pct, eb_postbalance_pct)


# This is the median and mad of the distribution of the mean EB standardized absolute differences
eb_final_output 



#now this is the median and the mad of the distribution of the estimated means and standard errors.

eb_bmi_medians<-apply(eb_bmi,1,median)
eb_female_medians<-apply(eb_female,1,median)
eb_age_medians<-apply(eb_age,1,median)
eb_year_medians<-apply(eb_year,1,median)
eb_age2_medians<-apply(eb_age2,1,median)
eb_bmi2_medians<-apply(eb_bmi2,1,median)
eb_year2_medians<-apply(eb_year2,1,median)


eb_bmi_mads<-apply(eb_bmi,1,mad)
eb_female_mads<-apply(eb_female,1,mad)
eb_age_mads<-apply(eb_age,1,mad)
eb_year_mads<-apply(eb_year,1,mad)
eb_age2_mads<-apply(eb_age2,1,mad)
eb_bmi2_mads<-apply(eb_bmi2,1,mad)
eb_year2_mads<-apply(eb_year2,1,mad)


pop.means<-the.population()

eb_final_estimates<-data.frame(eb_bmi_medians, eb_female_medians, 
                               eb_age_medians, eb_year_medians, eb_age2_medians, eb_bmi2_medians, eb_year2_medians )

eb_final_estimates<-t(eb_final_estimates)

eb_final_estimates<-data.frame(t(pop.means), eb_final_estimates)

names(eb_final_estimates) <- c( "pop.mean","target.mean", "target.se", "control_postbal.mean", "control_postbal.se","control_prebal.mean", "control_prebal.se")


# This is the median and the mad of the distribution of the estimated means and standard errors.
eb_final_estimates


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


ipw_final_output<-data.frame(xcov=names(one.sample())[c(2:8)], ipw_prebalance_medians, ipw_postbalance_medians, 
                             ipw_prebalance_mads, ipw_postbalance_mads, ipw_prebalance_pct, ipw_postbalance_pct)

# This is the median and mad of the distribution of the mean IPW standardized absolute differences
ipw_final_output

#now this is the median and the mad of the distribution of the estimated means and standard errors.

ipw_bmi_medians<-apply(ipw_bmi,1,median)
ipw_female_medians<-apply(ipw_female,1,median)
ipw_age_medians<-apply(ipw_age,1,median)
ipw_year_medians<-apply(ipw_year,1,median)
ipw_age2_medians<-apply(ipw_age2,1,median)
ipw_bmi2_medians<-apply(ipw_bmi2,1,median)
ipw_year2_medians<-apply(ipw_year2,1,median)


ipw_bmi_mads<-apply(ipw_bmi,1,mad)
ipw_female_mads<-apply(ipw_female,1,mad)
ipw_age_mads<-apply(ipw_age,1,mad)
ipw_year_mads<-apply(ipw_year,1,mad)
ipw_age2_mads<-apply(ipw_age2,1,mad)
ipw_bmi2_mads<-apply(ipw_bmi2,1,mad)
ipw_year2_mads<-apply(ipw_year2,1,mad)


pop.means<-the.population()

ipw_final_estimates<-data.frame(ipw_bmi_medians, ipw_female_medians, 
                               ipw_age_medians, ipw_year_medians, ipw_age2_medians, ipw_bmi2_medians, ipw_year2_medians )

ipw_final_estimates<-t(ipw_final_estimates)

ipw_final_estimates<-data.frame(t(pop.means), ipw_final_estimates)

names(ipw_final_estimates) <- c( "pop.mean","target.mean", "target.se", "control_postbal.mean", "control_postbal.se","control_prebal.mean", "control_prebal.se")

# This is the median and the mad of the distribution of the estimated means and standard errors.
ipw_final_estimates

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


sipw_final_output<-data.frame(xcov=names(one.sample())[c(2:8)], sipw_prebalance_medians, sipw_postbalance_medians, 
                              sipw_prebalance_mads, sipw_postbalance_mads, sipw_prebalance_pct, sipw_postbalance_pct)

# This is the median and mad of the distribution of the mean sIPW standardized absolute differences
sipw_final_output

#now this is the median and the mad of the distribution of the estimated means and standard errors.

sipw_bmi_medians<-apply(sipw_bmi,1,median)
sipw_female_medians<-apply(sipw_female,1,median)
sipw_age_medians<-apply(sipw_age,1,median)
sipw_year_medians<-apply(sipw_year,1,median)
sipw_age2_medians<-apply(sipw_age2,1,median)
sipw_bmi2_medians<-apply(sipw_bmi2,1,median)
sipw_year2_medians<-apply(sipw_year2,1,median)


sipw_bmi_mads<-apply(sipw_bmi,1,mad)
sipw_female_mads<-apply(sipw_female,1,mad)
sipw_age_mads<-apply(sipw_age,1,mad)
sipw_year_mads<-apply(sipw_year,1,mad)
sipw_age2_mads<-apply(sipw_age2,1,mad)
sipw_bmi2_mads<-apply(sipw_bmi2,1,mad)
sipw_year2_mads<-apply(sipw_year2,1,mad)


pop.means<-the.population()

sipw_final_estimates<-data.frame(sipw_bmi_medians, sipw_female_medians, 
                               sipw_age_medians, sipw_year_medians, sipw_age2_medians, sipw_bmi2_medians, sipw_year2_medians )

sipw_final_estimates<-t(sipw_final_estimates)

sipw_final_estimates<-data.frame(t(pop.means), sipw_final_estimates)

names(sipw_final_estimates) <- c( "pop.mean","target.mean", "target.se", "control_postbal.mean", "control_postbal.se","control_prebal.mean", "control_prebal.se")

# This is the median and the mad of the distribution of the estimated means and standard errors.
sipw_final_estimates


#####################################################################################################################
meta_data



# note to self: I would like to add into here later the shannon entropy for each of the weights coming out of EB, IPW and SIPW 
# to show which of the methods has the least entropy.










