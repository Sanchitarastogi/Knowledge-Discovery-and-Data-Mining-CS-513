#################################################
#  Company    : Stevens
#  Project    : CS-513 A_Homework 6
#  Purpose    : Use the C5.0 methodology to develop a classification model for the Diagnosis
#  First Name  : Sanchita
#  Last Name  : Rastogi
#  Id			    : 10439951
#  Date       : April-8-2019
#  Comments   : 
#################################################

rm(list=ls())

#################### Reading the data #############################
file_name <- file.choose()
Breast_Cancer<- read.csv(file_name,colClasses=c("Sample"="character",
                                                                "F1"="factor","F2"="factor","F3"="factor",
                                                                "F4"="factor","F5"="factor","F6"="factor",
                                                                "F7"="factor","F8"="factor","F9"="factor",
                                                                "Class"="factor"))
View(Breast_Cancer)

#################### Applying C5.0 #############################

index <- sort(sample(nrow(Breast_Cancer), round(.25*nrow(Breast_Cancer))))
training <- Breast_Cancer[-index,]
test <- Breast_Cancer[index,]

#install.packages('C50')
library('C50')
?C5.0
####################  C5.0 Classification #############################
C50_class <- C5.0( Class~.,data=training[,-1] )

summary(C50_class )
plot(C50_class)
C50_predict<-predict( C50_class ,test , type="class" )
table(actual=test[,11],C50=C50_predict)

####################  Calculating error rate #############################

wrong<- (test[,11]!=C50_predict)
c50_rate<-sum(wrong)/length(test[,11])
c50_rate
#Accuracy
accuracy<-1-c50_rate
accuracy
