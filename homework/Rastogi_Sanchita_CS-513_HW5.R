#################################################
#  Company    : Stevens
#  Project    : CS-513 A_Homework 5
#  Purpose    : Use the CART methodology to develop a classification model for the Diagnosis
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

####################  Checking CART libraries #############################
#install.packages("rpart")
#install.packages("rpart.plot")     # Enhanced tree plots
#install.packages("rattle")         # Fancy tree plot
#install.packages("RColorBrewer")   # colors needed for rattle

library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle


index <- sort(sample(nrow(Breast_Cancer), round(.25*nrow(Breast_Cancer))))
training <- Breast_Cancer[-index,]
test <- Breast_Cancer[index,]

?rpart
dev.off()
CART_class <- rpart(Class~., data = training[,-1])
rpart.plot(CART_class)

CART_predict2 <- predict(CART_class, test, type = "class")
table(Actual = test[,11], CART= CART_predict2)

CART_predict <- predict(CART_class, test)
str(CART_predict)

library(rpart.plot)
prp(CART_class)


# much fancier graph
fancyRpartPlot(CART_class)


#################### Checking error rate #############################

CART_predict2<-predict(CART_class,test, type="class")
CART_wrong2<-sum(test[,11]!=CART_predict2)
CART_error_rate2<-CART_wrong2/length(test[,11])
CART_error_rate2 







