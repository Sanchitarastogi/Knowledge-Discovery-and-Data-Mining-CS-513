#################################################
#  Company    : Stevens
#  Project    : CS-513 A_Homework 3
#  Purpose    : Use the knn methodology to develop a classification model for the Diagnosis
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

#################### Deleting the rows with missing values #############################
omit_missing_rows <- na.omit(Breast_Cancer)
omit_missing_rows

#################### Changing to the factor #############################
omit_missing_rows$Class<- factor(omit_missing_rows$Class, levels = c("2", "4"))
is.factor(omit_missing_rows$Class)

#################### Applying KNN #############################
library(kknn)
?kknn

index <-sort(sample(nrow(omit_missing_rows),as.integer(.70*nrow(omit_missing_rows))))
training<-omit_missing_rows[index,]
test<-omit_missing_rows[-index,]

predict_knn <-kknn(formula=as.factor(Class)~., training[,-1], test, k=3, kernel = "triangular" )
predict_knn <-kknn(formula=as.factor(Class)~., training, test, k=4, kernel = "triangular" )
predict_knn <-kknn(formula=as.factor(Class)~., training, test, k=5, kernel = "triangular" )
predict_knn <-kknn(formula=as.factor(Class)~., training, test, k=6, kernel = "triangular" )
fit <- fitted(predict_knn)
table(test$Class,fit)

knn_error_rate =sum(fit!=test$Class)/length(test$Class)
print(knn_error_rate)

