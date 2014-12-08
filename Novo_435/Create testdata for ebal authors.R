library(sampling)
library(car)
library(dplyr)

df<-readRDS("C:/Users/ubergooroo/Documents/GitHub/NOVO/Novo_435/nhanes_data_clean.rds")

names(df)
dim(df)

# Add a group variable Group == 0 will end up those records needing to be reweighted and 
# Group == 1 will be those used to do the reweighting
df$GROUP <- 0

Tot=length(df$SEQN);Tot
name=df$SEQN
n=1000

#select a sample
s=srswor(n,Tot)
#the sample is
as.vector(name[s==1])


# Extraction of the information from the dataframe for the selected sample
sampled=getdata(df, s) 
sampled$MEC6YR <- 1
dim(sampled)

not_sampled=getdata(df, !s)
not_sampled$GROUP <- 1
dim(not_sampled)

# Make sampled BMI fatter
sampled$BMXBMI <- sampled$BMXBMI + 3.50

# Recode sampled BMI into categories
sampled$BMICAT <-recode(sampled$BMXBMI,"12:18.999='0';19:24.999='1';25:29.9999='2';30:34.999='3';35:39.999='4';40:100='5'")

summary(sampled$BMICAT)
table(sampled$BMICAT) / length(sampled$BMICAT)

# Recode not_sampled BMI into categories
not_sampled$BMICAT <-recode(not_sampled$BMXBMI,"12:18.999='0';19:24.999='1';25:29.9999='2';30:34.999='3';35:39.999='4';40:100='5'")

summary(not_sampled$BMICAT)
table(not_sampled$BMICAT) / length(not_sampled$BMICAT)

head(sampled)
head(not_sampled)



# Select columns by name
HUM<-select(sampled, ID_unit, SRVYR=SDDSRVYR, AGENDR=RIAGENDR, AGEYR=RIDAGEYR, SMPLWT=MEC6YR, BMICAT, GROUP)
NHANES<-select(not_sampled, ID_unit, SRVYR=SDDSRVYR, AGENDR=RIAGENDR, AGEYR=RIDAGEYR, SMPLWT=MEC6YR, BMICAT, GROUP)

summary(HUM)
summary(NHANES)

testdata<-rbind(HUM,NHANES)
dim(testdata)

head(testdata)
tail(testdata)
names(testdata)


# Save the clean data
saveRDS(testdata, "C:/Users/ubergooroo/Documents/GitHub/NOVO/Novo_435/ebal_testdata.rds")






