#################################################
#  Company    : Stevens Institute of Technology
#  Project    : CS-513 A_Homework 2 
#  Purpose    : Perform EDA Analysis
#  First Name  : Sanchita
#  Last Name  : Rastogi
#  Id			    : 10439951
#  Date       : March-14-2019
#  Comments   : 
#################################################

rm(list=ls())

#################### Reading the data #############################

file_name <- file.choose()
Breast_Cancer<- read.csv(file_name, na.strings="?")
View(Breast_Cancer)
is.data.frame(Breast_Cancer)

#################### Calulating min, max and mean for each column  ############################# 

summary(Breast_Cancer)

###################  Identifying missing values  ##############################

length(Breast_Cancer)
nrow(Breast_Cancer)

missing_values <- sum(is.na(Breast_Cancer))
missing_values


####################### Replacing the missing values with the “mode” (most frequent value) of the column.###########################

install.packages('modeest',repos='http://cran.us.r-project.org')

library(modeest)
?mlv()
F6_mlv<-mlv(Breast_Cancer$F6,method = "mfv",na.rm = TRUE)
F6_mlv
Breast_Cancer$F6[is.na(Breast_Cancer$F6)] <- F6_mlv
View(Breast_Cancer)


##################### Displaying the frequency table of “Class” vs. F6############################

ftable(Class=Breast_Cancer$Class,F6=Breast_Cancer$F6)

####################  Displaying the scatter plot of F1 to F6, one pair at a time  ###################

pairs(Breast_Cancer[2:7])

#### Show histogram box plot for columns F7 to F9 #######

?hist()

boxplot(Breast_Cancer[8:10])
F7 <- hist(Breast_Cancer$F7)
F8 <- hist(Breast_Cancer$F8)
F9 <- hist(Breast_Cancer$F9)


####################  Remove any row with a missing value in any of the columns. ##################

rm(list=ls())

file_name <- file.choose()
Breast_Cancer<- read.csv(file_name, na.strings="?")
View(Breast_Cancer)
omit_missing_values <- na.omit(Breast_Cancer)
omit_missing_values
