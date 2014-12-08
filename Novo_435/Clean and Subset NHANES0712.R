setwd("C:/Users/ubergooroo/Documents/GitHub/NOVO/Novo_435")

getwd()

# Read in the saved dataset
df<-readRDS("C:/Users/ubergooroo/Documents/GitHub/NOVO/Novo_435/nhanes_data.rds")

# STEP 1
# 
# To produce estimates appropriately adjusted for survey non-response, 
# it is important to check all of the variables in your analysis and 
# select the weight of the smallest analysis subpopulation. 

# We already established that because the BMI examination data is a subset
# of the interview data that we should use "wtmec2yr" as our weight.
# This is already included in the df above for each year of the sample.


# STEP 2
# 
# In order to produce estimates with greater statistical reliability, 
# it may be necessary to combine survey cycles to increase the survey 
# size. If survey cycles are combined, new weights are needed. When you 
# combine two or more 2-year cycles of the continuous NHANES for NHANES 2001-2002 
# beyond, you must construct sample weights before beginning any analyses.  

# 6 years formula:
# If sddsrvyr in (5,6,7) then MEC6YR = 1/3 * WTMEC2YR; 

# Create a variable to hold the new weight
df$MEC6YR <- 0

df$MEC6YR <- ((1/3) * df$WTMEC2YR)

# Check your results
head(df)


# STEP 3
# 
# In order to calculate the most accurate measure of the variance estimate, 
# it is important to properly create subsets of your data to reflect the subpopulation
# of interest before using weights in your analyses.

# First we will select only those units that were examined in the dataset.
# The ridstatr variable on your demographic file designates interviewed participants 
# with a value=1, and interviewed plus examined participants with a value = 2.

# Select only those rows where sample unit was interviewed and examined.
# http://wwwn.cdc.gov/nchs/nhanes/2007-2008/DEMO_E.htm#RIDSTATR

mec_df<-df[df$RIDSTATR == 2 & !is.na(df$RIDSTATR), ]

# How many records did we discard getting rid of those who were interviewed but not examined.

dim(df) - dim(mec_df)

# Select only those rows where sample unit had private insurance.
# http://wwwn.cdc.gov/nchs/nhanes/2007-2008/HIQ_E.htm#HIQ031A

ins_df<-mec_df[mec_df$HIQ031A == 14 & !is.na(mec_df$HIQ031A), ] 

# How many records did we discard getting rid of those who did not have private insurance.

dim(mec_df) - dim(ins_df)

# Select only those rows where sample unit was between 18 and 80 at interview 
# (any sampled unit over 80 is coded as 80 years in NHANES data.
# http://wwwn.cdc.gov/nchs/nhanes/2007-2008/DEMO_E.htm#RIDAGEYR

age_df<-ins_df[ins_df$RIDAGEYR >= 18  & !is.na(ins_df$RIDAGEYR), ] 

# How many records did we discard getting rid of those who were not between 18 and 80.

dim(ins_df) - dim(age_df)

# Select only those rows where sample unit had a non-missing BMI

bmi_df<-age_df[!is.na(age_df$BMXBMI), ] 

# How many records did we discard getting rid missing BMI.

dim(age_df) - dim(bmi_df)

# Confirm there are no missing Genders
table(is.na(bmi_df$RIAGENDR))

# Summaries of the dataframe

summary(bmi_df)

# Save the clean data
saveRDS(bmi_df, "C:/Users/ubergooroo/Documents/GitHub/NOVO/Novo_435/nhanes_data_clean.rds")







# empty workspace
rm(list=ls())
# this clears the console
cat("\014") 
