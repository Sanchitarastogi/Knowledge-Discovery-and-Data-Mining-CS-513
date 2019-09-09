#  First Name      : Sanchita
#  Last Name       : Rastogi
#  Id              : 10439951
#  purpose         : Midterm_Question6_Naive_Bayes

remove(list=ls())

#################### Loading the data #############################
file_name <- file.choose()
IBM_rows <- read.csv(file_name)
View(IBM_rows)
str(IBM_rows)

#################### Deleting the rows with missing values #############################
ibm_missing_rows <- na.omit(IBM_rows)
ibm_missing_rows

#################### Normalizing the dataset #############################
mnnorm <-function(x,minx,maxx) {z<-((x-minx)/(maxx-minx))
return(z) 
}

ibm_normalized<-as.data.frame (         
  cbind(Age= mnnorm(ibm_missing_rows[,1],min(ibm_missing_rows[,1]),max(ibm_missing_rows[,1])),
        JobSatisfaction= mnnorm(ibm_missing_rows[,2],min(ibm_missing_rows[,2]),max(ibm_missing_rows[,2])),
        MonthlyIncome= mnnorm(ibm_missing_rows[,3],min(ibm_missing_rows[,3]),max(ibm_missing_rows[,3])),
        YearsAtCompany= mnnorm(ibm_missing_rows[,4],min(ibm_missing_rows[,4]),max(ibm_missing_rows[,4])),
        Single = ibm_missing_rows[,5],
        Gender = ibm_missing_rows[,7],
        Attrition = ibm_missing_rows[,6]
  ))
View(ibm_normalized)
ibm_normalized$Attrition<- as.factor(ibm_normalized$Attrition)

#################### Selecting every third row as test dataset #############################
test_dataset <-seq(1,nrow(ibm_normalized),by=3)
test<-ibm_normalized[test_dataset,]
training<- ibm_normalized[-test_dataset,]

#################### Performing Naive Bayes #############################
library(class) 
library(e1071)

nBayes <- naiveBayes(Attrition~JobSatisfaction+Single+Gender, data =training)

##################### Score the test dataset #############################
category_all<-predict(nBayes,test[,c(2,5,6,7)])
table(Attrition=category_all,test$Attrition)


#################### Calculating error rate #############################
NB_wrong<-sum(category_all!=test$Attrition )
NB_error_rate<-NB_wrong/length(category_all)
NB_error_rate

