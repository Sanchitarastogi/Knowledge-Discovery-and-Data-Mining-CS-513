#  First Name      : Sanchita
#  Last Name       : Rastogi
#  Id              : 10439951
#  purpose         : Midterm_Question4

remove(list=ls())

################## Creating R vector ##############
vector <-c(45, 48, 6, 42, 49, 63, 81, 56, 21, 75, 25, 48, 56, 24, 73, 82, NA, 80, 86, 88 )

################## Finding min, max, median, mean and standard deviation ##############
min(vector, na.rm = TRUE)

max(vector, na.rm = TRUE)

mean(vector, na.rm = TRUE)

median(vector, na.rm = TRUE)

sd(vector, na.rm = TRUE)

################## Replace the missing values with mean ##############

vector_mean<-as.integer( ifelse(is.na(vector), mean(vector, na.rm=TRUE), vector))
vector_mean

################## to develop a box plot for these numbers ##############
boxplot(vector, na.rm = TRUE)
