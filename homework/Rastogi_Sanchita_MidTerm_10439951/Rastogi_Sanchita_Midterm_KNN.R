#  First Name      : Sanchita
#  Last Name       : Rastogi
#  Id              : 10439951
#  purpose         : Midterm_Question5_KNN

remove(list=ls())

#################### Loading the data #############################
file_name <- file.choose()
IBM_rows <- read.csv(file_name)
View(IBM_rows)

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


#################### Selecting every third row as test dataset #############################
test_dataset <-seq(1,nrow(ibm_normalized),by=3)
test<-ibm_normalized[test_dataset,]
training<- ibm_normalized[-test_dataset,]

#################### Performing KNN #############################
library(kknn)
?kknn()

predict_knn <- kknn(formula=factor(Attrition)~. ,  training, test  , kernel="rectangular", k=3)

##################### Score the test dataset #############################
fit <- fitted(predict_knn)
table(kknn=fit,test$Attrition)

#################### Calculating error rate #############################
knn_error_rate=sum(fit!=test$Attrition)/length(test$Attrition)
print(knn_error_rate)

