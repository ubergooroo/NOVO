setwd("C:/Users/ubergooroo/Documents/GitHub/NOVO/Novo_435")

getwd()

# Include Foreign Package To Read SAS Transport Files
library(foreign)

# READ NHANES 2007-2008 Demographic Data To Temporary File                  
demo_a <- read.xport("C:/Users/ubergooroo/Downloads/demo_e.xpt")[,c("SEQN","SDMVPSU","SDMVSTRA","WTMEC2YR","RIDSTATR", "SDDSRVYR", "RIAGENDR","RIDAGEYR")]

# READ NHANES 2009-2010 Demographic Data To Temporary File
demo_b <- read.xport("C:/Users/ubergooroo/Downloads/demo_f.xpt")[,c("SEQN","SDMVPSU","SDMVSTRA","WTMEC2YR","RIDSTATR", "SDDSRVYR", "RIAGENDR","RIDAGEYR")]

# READ NHANES 2011-2012 Demographic Data To Temporary File
demo_c <- read.xport("C:/Users/ubergooroo/Downloads/demo_g.xpt")[,c("SEQN","SDMVPSU","SDMVSTRA","WTMEC2YR","RIDSTATR", "SDDSRVYR", "RIAGENDR","RIDAGEYR")]


# Append Demographic Data
demo_6yr <- rbind(demo_a, demo_b, demo_c)
head(demo_6yr)

##############################################################################################################


# READ NHANES 2007-2008 Demographic Data To Temporary File   
bio_a <- read.xport("C:/Users/ubergooroo/Downloads/bmx_e.xpt")[,c("SEQN","BMXBMI")]

# READ NHANES 2009-2010 Demographic Data To Temporary File
bio_b <- read.xport("C:/Users/ubergooroo/Downloads/bmx_f.xpt")[,c("SEQN","BMXBMI")]

# READ NHANES 2011-2012 Demographic Data To Temporary File
bio_c <- read.xport("C:/Users/ubergooroo/Downloads/bmx_g.xpt")[,c("SEQN","BMXBMI")]


# Append Biometric Exam Data
bio_6yr <- rbind(bio_a, bio_b, bio_c)
head(bio_6yr)

##############################################################################################################

# READ NHANES 2007-2008 Demographic Data To Temporary File   
ins_a <- read.xport("C:/Users/ubergooroo/Downloads/hiq_e.xpt")[,c("SEQN","HIQ031A")]

# READ NHANES 2009-2010 Demographic Data To Temporary File   
ins_b <- read.xport("C:/Users/ubergooroo/Downloads/hiq_f.xpt")[,c("SEQN","HIQ031A")]

# READ NHANES 2011-2012 Demographic Data To Temporary File   
ins_c <- read.xport("C:/Users/ubergooroo/Downloads/hiq_g.xpt")[,c("SEQN","HIQ031A")]

# Append Insurance Data
ins_6yr <- rbind(ins_a, ins_b, ins_c)
head(ins_6yr)


# Merge Files Together Two At A Time
nhanes_0712 <- merge(demo_6yr, bio_6yr, by="SEQN", all=TRUE)
nhanes_0712 <- merge(nhanes_0712, ins_6yr, by="SEQN", all=TRUE)

# List the names of the variables
names(nhanes_0712)


# Display Structure Of The Data Frame & First 10 Observations 
str(nhanes_0712)

# Display Unweighted Summary Statistics
summary(nhanes_0712)

# Display the dimensions of the dataframe
dim(nhanes_0712)

# Save the data to the working directory
saveRDS(nhanes_0712, "C:/Users/ubergooroo/Documents/GitHub/NOVO/Novo_435/nhanes_data.rds")
