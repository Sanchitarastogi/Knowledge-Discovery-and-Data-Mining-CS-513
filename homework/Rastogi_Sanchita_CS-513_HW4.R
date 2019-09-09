#################################################
#  Company    : Stevens Institute of Technology
#  Project    : CS-513 A_Homework 4
#  Purpose    : Naïve Bayes methodology to develop a classification model for the Diagnosis
#  First Name  : Sanchita
#  Last Name  : Rastogi
#  Id			    : 10439951
#  Date       : March-14-2019
#  Comments   : 
#################################################

rm(list=ls())

#################### Reading the data #############################
file_name <- file.choose()
Breast_Cancer <- read.csv(file_name, na.strings="?")
View(Breast_Cancer)

#################### Deleting the rows with missing values #############################
omit_missing_rows <- na.omit(Breast_Cancer)
omit_missing_rows

#################### Changing to the factor #############################
omit_missing_rows$Class <- as.factor(omit_missing_rows$Class)
View(omit_missing_rows$Class)
is.factor(omit_missing_rows$Class)

#################### Applying Naive Bayes #############################
library(class)
install.packages('e1071')
library(e1071)
nBayes_class <- naiveBayes(as.factor(Class) ~Class, data = omit_missing_rows)
category_class <- predict(nBayes_class,omit_missing_rows)

data_class <- cbind(omit_missing_rows, category_class)
table(Class =omit_missing_rows$Class, Class=omit_missing_rows$Class )
table(Class =omit_missing_rows$Class, NBayes = category_class)


table(NBayes=category_class,Class=omit_missing_rows$Class)
?prop.table


nBayes_class <- naiveBayes(Class ~Class, data=Breast_Cancer)
category_class <- predict(nBayes_class,Breast_Cancer)

#################### Comparing predicted values to the actual #############################
nBayes_all<-naiveBayes(Class ~., data=omit_missing_rows)

category_all<-predict(nBayes_all,omit_missing_rows)
table(nBayes_all=category_all,Class=omit_missing_rows$Class)

#################### Calculating error rate #############################
NB_wrong<-sum(category_all!=omit_missing_rows$Class)
NB_wrong

NB_error_rate<-NB_wrong/length(category_all)
NB_error_rate
