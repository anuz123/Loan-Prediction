setwd("D:/machine_learning/AV")
library(tidyverse)
library(C50)
library(gmodels)

# Reading Train Set and Test Set
bank_train<- read.csv("train_u6lujuX_CVtuZ9i.csv") 
bank_test<- read.csv("test_Y3wMUE5_7gLdaTN.csv")
table(bank_train$Loan_Status)     # To check the target variable
prop.table(table(bank_train$Loan_Status))  # To check the probability of the status of acceptance of loans in train set


# Missing Value Imputation of Training Set

table(is.na(bank_train$LoanAmount))
table(is.na(bank_train$Loan_Amount_Term))
table(is.na(bank_train$Credit_History))

bank_train[9:10]<- apply(bank_train[9:10],2,function(x){
  ifelse(is.na(x),mean(x,na.rm=TRUE),x)
})
bank_train$Credit_History[which(is.na(bank_train$Credit_History))] = "Unknown"
bank_train[,c(2:4,6)]<- apply(bank_train[,c(2:4,6)],2,function(x){
  sub("^$","Unknown",x)
})



# Missing Value Imputation of Test Set

bank_test[9:10]<- apply(bank_test[9:10],2,function(x){
  ifelse(is.na(x),mean(x,na.rm=TRUE),x)
})
bank_test$Credit_History[which(is.na(bank_test$Credit_History))] = "Unknown"
bank_test[,c(2:4,6)]<- apply(bank_test[,c(2:4,6)],2,function(x){
  sub("^$","Unknown",x)
})


# Randomization
set.seed(123)
bank_train <- bank_train[sample(nrow(bank_train)), ]
bank_test <- bank_test[sample(nrow(bank_test)), ]
bank_test_labels<- bank_test[,1]
bank_train<- bank_train[,-1]


# Decision Tree Classification Algorithm
loan_model<- C5.0(bank_train[-12],bank_train$Loan_Status,trials=10)
loan_model
summary(loan_model)
bank_pred<- predict(loan_model,bank_test)
prop.table(table(bank_pred))


# Feature Selection
# Credit_History,Married,LoanAmount,Property_Area
c<- bank_train[,c(2,8,10,11,12)]
library(C50)
loan_model<- C5.0(c[-5],c$Loan_Status)
loan_model
summary(loan_model)


# Final Submission Preparation
bank_test$Loan_Status<- as.character(bank_pred)
bank_test$Loan_ID<- as.character(bank_test_labels)
mi<- cbind(bank_test$Loan_ID,bank_test$Loan_Status)
mi<- as.data.frame(mi)
colnames(mi)<- c("Loan_ID","Loan_Status")
write.csv(mi,"Sample_Submissions.csv")

