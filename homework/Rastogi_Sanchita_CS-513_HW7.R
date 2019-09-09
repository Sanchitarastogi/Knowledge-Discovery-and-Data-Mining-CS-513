#################################################
#  Company    : Stevens
#  Project    : CS-513 A_Homework 7
#  Purpose    : Use the ANN methodology 
#  First Name  : Sanchita
#  Last Name  : Rastogi
#  Id			    : 10439951
#  Date       : April-8-2019
#  Comments   : 
#################################################

rm(list=ls())

#################### Reading the data #############################
file_name <- file.choose()
IBM<- read.csv(file_name)
View(IBM)

IBM_2 <- data.frame(lapply(na.omit(IBM), as.numeric))
index <- seq (1,nrow(IBM_2),by=5)
test<- IBM_2[index,]
training<-IBM_2[-index,]


library("neuralnet")
?neuralnet()
class(training$Attrition)
net_IBM2<- neuralnet( Attrition~. ,training, hidden=10, threshold=0.01)


#Plot the neural network
plot(net_IBM2)

## test should have only the input colum
ann <-compute(net_IBM2 , test[,])
ann$net.result 

ann_cat<-ifelse(ann$net.result <1.5,1,2)
length(ann_cat)
table(Actual=test$Attrition,predition=ann_cat)
##### Error rate ############
wrong<- (test$Attrition!=ann_cat)
error_rate<-sum(wrong)/length(wrong)
error_rate
