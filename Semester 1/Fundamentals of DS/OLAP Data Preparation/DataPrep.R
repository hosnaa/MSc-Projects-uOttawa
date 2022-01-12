############################## (1) #############################
# Import the dataset
data <- read.csv('D:/DEBI_Outtawa/DS/Tut3-EDA_Data_Preparation/bank-additional-full.csv', sep = ';')

# get head of data
head(data)

#get some info about columns and their types
str(data)

# Size of data
dim(data)

#Take specific part of data--> target= y
data_part <- subset(data, select = c("age","education","previous","pdays","y"))

############################# (2-3-4) ##############################
# Replace 999 to na

#install.packages("dplyr")
#library(dplyr)

# Before excluding nulls
hist(data_part$pdays, xlab ="# days since last contact", main ="Number of days")

data_part$pdays[data_part$pdays==999]<-NA

#data_part_NA <- na_if(data_part$pdays, 999)
#data_part <- subset(data_part, select = -c(pdays))
#full_join(data_part, data_part_NA, by = "Code")
#data_part <- revalue(x = data_part$pdays, replace= c(999 = 'NA'))

# As most of the data is null so we have to deal with it to be able to see the distribution of the valid points that were hidden by the 999 value
#dplyr::count(data_part$pdays, x, sort = TRUE)

# After excluding nulls 
hist(data_part$pdays, xlab ="# days since last contact", main ="Number of days")

######################## (5) ##################################

data_part$education[data_part$education=='basic.4y']<-4
data_part$education[data_part$education=='high.school']<-12
data_part$education[data_part$education=='basic.6y']<-6
data_part$education[data_part$education=='basic.9y']<-9
data_part$education[data_part$education=='professional.course']<-14
data_part$education[data_part$education=='unknown']<-NA
data_part$education[data_part$education=='university.degree']<-16
data_part$education[data_part$education=='illiterate']<-0

data_part$education <- as.numeric(data_part$education)

################## (6) #################
#install.packages("modeest")
library(modeest)

mean(data_part$age)
median(data_part$age)
mfv(data_part$age)

boxplot(data_part$age, main="Box_Age", xlab="Distribution", ylab="Age values")

# 5-Number summary
min(data_part$age)
#First quartile:
quantile(data_part$age, 0.25)

#Second quartile or median:
quantile(data_part$age, 0.5)

#median(data_part$age)

#Third quartile:
quantile(data_part$age, 0.75)

max(data_part$age)

install.packages("car")
library("car")
qqPlot(data_part$age)

################################ (7) #########################
age_z <- scale(x =data_part$age)

############################## (8) ###########################
boxplot(age_z, main="Box_Age", xlab="Distribution", ylab="Age values")

outliers <-  age_z[ which(age_z < -3 | age_z > 3), ]

out <- list(outliers)
