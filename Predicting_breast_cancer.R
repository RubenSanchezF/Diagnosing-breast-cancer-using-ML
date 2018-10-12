## PREDICTING BREAST CANCER

#Author: Rubén Sánchez Fernández

##----------------------------------------------------------------------------##

#libraries
library(class) #k-nn function
library(gmodels) #Cross-Table function

##----------------------------------------------------------------------------##

#DATA OVERVIEW

#importing data
ds<-read.csv("C:/Users/ruben/OneDrive/Escritorio/MASTER/Semestre 2/MACHINE LEARNING/Ejercicio 2", stringsAsFactors = FALSE)

#dimensions
print(paste0("The dataset has ", nrow(ds), " examples and ", ncol(ds), " features"))

#summary
summary(ds)

#internal structure
str(ds)


##---------------------------------------------------------------------------##

#PREPROCESSING

#removing 1st column
ds<-ds[-1]

#converting target feature to factor and changing labels
ds$diagnosis<-factor(ds$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))

#creating a function to normalize data
normalize<-function(x){
  return(( x-min(x)) / (max(x - min(x))))
}

#applying the function to our data
ds_n<-as.data.frame(lapply(ds[2:31], normalize))

#let's check data is normalized
summary(ds_n)


##-------------------------------------------------------------------------##

#TRAINING MODEL

#training set
ds_train<-ds_n[1:469,]
#test set
ds_test<-ds_n[470:569,]

#training labels
ds_train_labels<-ds[1:469,1]
#test labels
ds_test_labels<-ds[470:569,1]


##-------------------------------------------------------------------------##

#TESTING
#knn
ds_test_pred<-knn(train=ds_train, test=ds_test, cl=ds_train_labels, k=21) #k=21

##--------------------------------------------------------------------------##

#PERFORMANCE EVALUATION
#crosstable
CrossTable(x = ds_test_labels, y = ds_test_pred, prop.chisq=FALSE)

