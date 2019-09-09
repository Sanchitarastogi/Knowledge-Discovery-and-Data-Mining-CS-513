#################################################
#  Company    : Stevens
#  Project    : CS-513 A_Homework 8
#  Purpose    : Use Clustering Hclust and knn
#  First Name  : Sanchita
#  Last Name  : Rastogi
#  Id			    : 10439951
#  Date       : April-8-2019
#  Comments   : 
#################################################

rm(list=ls())

#################### Reading the data #############################
file_name <- file.choose()
IBM_data<- read.csv(file_name)

View(IBM_data)
summary(IBM_data)

#Remove any row with missing values
IBM_data<-na.omit(IBM_data)

#Minmax Normalization function
mnnorm <-function(x,minx,maxx) {z<-((x-minx)/(maxx-minx))
return(z) 
}
# Normalize the data using Minmax Normalization function
IBM_data_normalized <- as.data.frame(
  cbind(Education= mnnorm(IBM_data[,5],min(IBM_data[,5]),max(IBM_data[,5])),
        Gender = factor(IBM_data[,8]),
        TotalWorkingYears= mnnorm(IBM_data[,13],min(IBM_data[,13]),max(IBM_data[,13])),
        Attrition = factor(IBM_data[,2])
  )
)

##Cluster-1 >> Using hclust, categorizing IBM data into two (2) clusters based on “TotalWorkingYears”, “Gender” and “Education” feature
IBM_data_dist<-dist(IBM_data_normalized[,-4])
hclust_resutls<-hclust(IBM_data_dist)
plot(hclust_resutls)
hclust_2<-cutree(hclust_resutls,2)

#Tabulate the clustered rows against the “Attrition” column.
table(hclust_2,IBM_data_normalized[,4])

##Cluster-2 >> Same clusters as Cluster1 but using K-means 

?kmeans
kmeans_2<- kmeans(IBM_data_normalized[,c(-4)],2,nstart = 10)
str(kmeans_2)
kmeans_2$cluster
table(kmeans_2$cluster,IBM_data_normalized[,4])

