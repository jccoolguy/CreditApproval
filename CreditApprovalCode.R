rm(list=ls())

library(readxl)
library(ggplot2)
library(here)
#install.packages("ggthemes")
library(ggthemes)
library(tidyverse)
library(MASS)
#install.packages("mltools")
library(mltools)
#install.packages("caret")
library(caret)
library(data.table)
#install.packages("caTools")
#install.packages("ROCR")
library(caTools)
library(ROCR)
#install.packages("randomForest")
library('randomForest')
library(glmnet)
library(class)
#install.packages("ROCit")
library(ROCit)

library(pROC)
#Summary of Data
CreditData <- read.table("C:/Users/Jack Cunningham/Desktop/BigData/CreditData.data", header=FALSE,
                     sep=",")

typeof(CreditData)
colnames(CreditData)<-c('Gender','Age','Debt','MartialStatus','BankCustomer','EducationLevel','Ethnicity',
                        'YearsEmployed','PriorDefault','Employed','CreditScore','DriversLicense',
                        'Citizen','ZipCode','Income','Approved')
summary(CreditData)
#as.data.frame(CreditData)


#Pre-processing Data

#Gender
table(CreditData['Gender'])
#Converting ? Values to NA values
CreditData$Gender[CreditData$Gender=='?']<-NA
table(CreditData['Gender'])
#Will Perform One Hot Encoding

#Age
table(CreditData$Age)
CreditData$Age[CreditData$Age=='?']<-NA
CreditData$Age<-as.numeric(CreditData$Age)
table(CreditData$Age)
#Will Standardize Later

#Debt
table(CreditData$Debt)
#will standardize later

#MartialStatus
table(CreditData$MartialStatus)
#Converting ? values to NA values
CreditData$MartialStatus[CreditData$MartialStatus=='?']<-NA
table(CreditData$MartialStatus)
#Will Perform One Hot Encoding

#BankCustomer
table(CreditData$BankCustomer)
#Converting ? values to NA values
CreditData$BankCustomer[CreditData$BankCustomer=='?']<-NA
table(CreditData$BankCustomer)
#Will Perform One Hot Encoding


#EducationLevel
table(CreditData$EducationLevel)
CreditData$EducationLevel[CreditData$Ethnicity=='?']<-NA
#Will perform one hot encoding

#Ethnicity
table(CreditData$Ethnicity)
CreditData$Ethnicity[CreditData$Ethnicity=='?']<-NA
#will perform one hot encoding

#YearsEmployed
table(CreditData$YearsEmployed)
#Will Standardize later

#PriorDefault
table(CreditData$PriorDefault)
#Changing Prior Default into binary variable
#Changing f/t to 0/1
CreditData$PriorDefault[CreditData$PriorDefault=='f']<-0
CreditData$PriorDefault[CreditData$PriorDefault=='t']<-1
table(CreditData$PriorDefault)

#Employed
table(CreditData$Employed)
#Changing Employed to binary variable
table(CreditData$PriorDefault)
#Will Perform One Hot Encoding

#CreditScore
table(CreditData$CreditScore)
#Will Standardize later

#DriversLicense
table(CreditData$DriversLicense)
#Changing DriversLicense to binary variable
#Changing f/t to 0/1
CreditData$DriversLicense[CreditData$DriversLicense=='f']<-0
CreditData$DriversLicense[CreditData$DriversLicense=='t']<-1
table(CreditData$DriversLicense)

#Citizen
table(CreditData$Citizen)
#Will perform one hot encoding later

#ZipCode
table(CreditData$ZipCode)
CreditData$ZipCode[CreditData$ZipCode=='?']<-NA
CreditData$ZipCode<-as.numeric(CreditData$ZipCode)
#Will attempt to find a way to organize values

#Income
table(CreditData$Income)
#Will standardize Later

#Approved
table(CreditData$Approved)
#Changing Approved to binary variable 
#Changing -/+ to 0/1
CreditData$Approved[CreditData$Approved=='-']<-0
CreditData$Approved[CreditData$Approved=='+']<-1
table(CreditData$Approved)

#Generating Visualizations
#Age
age_histo<-ggplot(CreditData,aes(x=Age))+
                    geom_histogram(color="black",fill="#192841")+ scale_y_continuous("")

                    
age<-age_histo+theme_economist()+scale_color_economist()+scale_fill_economist()+ggtitle("Age Distribution")
age

#YearsEmployed

Years_histo<-ggplot(CreditData,aes(x=YearsEmployed))+
  geom_histogram(color="black",fill="#192841")+ scale_y_continuous("")

Years<-Years_histo+theme_economist()+scale_color_economist()+scale_fill_economist()+ggtitle("Years Employed Distribution")
Years

Years
#Income
Income_histo<-ggplot(CreditData,aes(x=Income))+
  geom_histogram(color="black",fill="#192841")+ scale_y_continuous("")

Income<-Income_histo+theme_economist()+scale_color_economist()+scale_fill_economist()+ggtitle("Income Distribution")
Income
summary(CreditData$Income)

#Gender
gender_bar<-ggplot(data=subset(CreditData,!is.na(CreditData$Gender)),aes(x="",y=Gender,fill=Gender))+
            geom_col()+theme(axis.text.x=element_blank(),
                            axis.ticks.x=element_blank())

gender_bar+theme_economist()+scale_color_economist()+scale_fill_economist()+ggtitle("Gender Distribution")

#Approved
approved_bar<-ggplot(CreditData,aes(x="",y=Approved,fill=Approved))+
  geom_col()

approved_bar+theme_economist()+scale_color_economist()+scale_fill_economist()+ggtitle("Approved Distribution")

#Standardizing numeric values
ind<-sapply(CreditData,is.numeric)
CreditData[ind]<-lapply(CreditData[ind],scale)
#Now all numeric values are standardized


sum(is.na(CreditData))
#Next we must deal with NA values, for numeric values we will replace with mean, for categorical we will replace with median
CreditData$Gender[is.na(CreditData$Gender)]<-'b'
CreditData$Age[is.na(CreditData$Age)]<-mean(CreditData$Age,na.rm=TRUE)
CreditData$MartialStatus[is.na(CreditData$MartialStatus)]<-'u'
CreditData$BankCustomer[is.na(CreditData$BankCustomer)]<-'g'
CreditData$EducationLevel[is.na(CreditData$EducationLevel)]<-'c'
CreditData$Ethnicity[is.na(CreditData$Ethnicity)]<-'v'
CreditData$ZipCode[is.na(CreditData$ZipCode)]<-mean(CreditData$ZipCode,na.rm=TRUE)

any(is.na(CreditData))
sum(is.na(CreditData))

#Turning variables we want to use one hot encoding technique on into factors for MLtools one_hot function
CreditData$Gender<-as.factor(CreditData$Gender)
CreditData$MartialStatus<-as.factor(CreditData$MartialStatus)
CreditData$BankCustomer<-as.factor(CreditData$BankCustomer)
CreditData$EducationLevel<-as.factor(CreditData$EducationLevel)
CreditData$Ethnicity<-as.factor(CreditData$Ethnicity)
CreditData$PriorDefault<-as.factor(CreditData$PriorDefault)
CreditData$Employed<-as.factor(CreditData$Employed)
CreditData$DriversLicense<-as.factor(CreditData$DriversLicense)
CreditData$Citizen<-as.factor(CreditData$Citizen)

#Converting Approved to Binary
CreditData$Approved<-as.numeric(CreditData$Approved)

#Using one_hot function 
OneHotCreditData<-one_hot(as.data.table(CreditData))
summary(OneHotCreditData)


any(is.na(OneHotCreditData))


#Logistic Regression
split<-sample.split(OneHotCreditData$Approved,SplitRatio=0.8)
split
train_reg<-subset(OneHotCreditData,split=="TRUE")
test_reg<-subset(OneHotCreditData,split=="FALSE")

logistic_model<-glm(Approved~.,family=binomial(link="logit"),data=train_reg)

predict_reg<-predict(logistic_model,test_reg,type='response')
predict_reg<- ifelse(predict_reg>.5,1,0)
table(test_reg$Approved,predict_reg)
missing_classer<-mean(predict_reg!=test_reg$Approved)
print(paste('Accuracy=',1-missing_classer))

auc(test_reg$Approved,predict_reg)

ROC_it_log<-rocit(score=predict_reg,class=test_reg$Approved)
plot(ROC_it_log)

#RandomForest
rf<-randomForest(Approved~.,data=train_reg, proximity=TRUE)

rf_predict<-predict(rf,test_reg)
rf_predict<-ifelse(rf_predict>.5,1,0)
missing_classer_rf<-mean(rf_predict!=test_reg$Approved)
table(test_reg$Approved,rf_predict)
print(paste('Accuracy=',1-missing_classer_rf))
auc(test_reg$Approved,rf_predict)

Roc_it_rf<-rocit(score=rf_predict,class=test_reg$Approved)
plot(Roc_it_rf)


#LASSO
Lasso_X_train<-model.matrix(~.-Approved,data=train_reg)
Lasso_X_test<-model.matrix(~.-Approved,data=test_reg)


glmod<-cv.glmnet(Lasso_X_train,y=train_reg$Approved,family="binomial",alpha=1,standardize=TRUE,nfolds=5)
lasso_best_lambda<-glmod$lambda.min
lasso_best_lambda

lasso_model<-glmnet(Lasso_X_train,train_reg$Approved,s=lasso_best_lambda,standardize=TRUE)

glm_prob<-predict(lasso_model,newx=Lasso_X_test,type='response',s=lasso_best_lambda)
print(glm_prob)
  
lasso_predict<-ifelse(glm_prob>.5,1,0)
print(lasso_predict)
table(test_reg$Approved,lasso_predict)
missing_classer_lasso<-mean(lasso_predict!=test_reg$Approved)
print(paste('Accuracy=',1-missing_classer_lasso))

lasso_predict<-as.numeric(lasso_predict)
auc(test_reg$Approved,lasso_predict)
Roc_it_lasso<-rocit(score=lasso_predict,class=test_reg$Approved)
plot(Roc_it_lasso)

#K Nearest Neighbors
NROW(train_reg)
#23/24 K Values

train_x1<-model.matrix(~.-Approved,data=train_reg)
test_X1<-model.matrix(~.-Approved,data=test_reg)


K_Nearest_23<-knn(train=train_x1,test=test_X1,cl=train_reg$Approved,k=23)
K_Nearest_24<-knn(train=train_x1,test=test_X1,cl=train_reg$Approved,k=24)

accuracy_23<-100*sum(test_reg$Approved==K_Nearest_23)/NROW(test_reg$Approved)
accuracy_24<-100*sum(test_reg$Approved==K_Nearest_24)/NROW(test_reg$Approved)


table(test_reg$Approved,K_Nearest_23)
print(accuracy_23)

K_Nearest_23_pred<-as.numeric(K_Nearest_23)
auc(test_reg$Approved,K_Nearest_23_pred)
Roc_it_KNN<-rocit(score=K_Nearest_23_pred,class=test_reg$Approved)
plot(Roc_it_KNN)

#Ridge Regression
X<-model.matrix(~.-Approved,data=train_reg)
Y<-train_reg$Approved

test_X<-model.matrix(~.-Approved,data=test_reg)


lambdas <- 10^seq(2, -2, by = -.1)
Ridge_regression<-cv.glmnet(X,Y,alpha=0,lambda=lambdas,family='gaussian')
best_lambda<-Ridge_regression$lambda.min
print(best_lambda)

ridge_predict<-predict(Ridge_regression,newx=test_X,s=best_lambda)

print(ridge_predict)
ridge_predict<-ifelse(ridge_predict>.5,1,0)

table(test_reg$Approved,ridge_predict)
accuracy_ridge<-100*sum(test_reg$Approved==ridge_predict)/NROW(test_reg$Approved)
print(accuracy_ridge)
ridge_predict<-as.numeric(ridge_predict)
auc(test_reg$Approved,ridge_predict)
Roc_it_Ridge<-rocit(score=ridge_predict,class=test_reg$Approved)
plot(Roc_it_Ridge)


#Ensemble Model

ensemble_prediction<-vector("numeric",138)

i<-1
while (i <138){
  count_0<-0
  count_1<-0
  if (predict_reg[i]==1){
    count_1<-count_1+1
  }
  else{
    count_0<-count_0+1
  }
  if(rf_predict[i]==1){
    count_1<-count_1+1
  }
  else{
    count_0<-count_0+1
  }
  if(lasso_predict[i]==1){
    count_1<-count_1+1
  }
  else{
    count_0<-count_0+1
  }
  if(K_Nearest_23_pred[i]==1){
    count_1<-count_1+1
  }
  else{
    count_0<-count_0+1
  }
  if(ridge_predict[i]==1){
    count_1<-count_1+1
  }
  else{
    count_0<-count_0+1
  }
  print(count_1)
  print(count_0)
  if(count_1>count_0){
    ensemble_prediction[i]=1
  }
  else{
    ensemble_prediction[i]=0
  }
  i<-i+1
}
length(ensemble_prediction)

print(ensemble_prediction)
accuracy_ensemble_prediction<-100*sum(test_reg$Approved==ensemble_prediction)/NROW(ensemble_prediction)
table(test_reg$Approved,ensemble_prediction)
print(accuracy_ensemble_prediction)
auc(test_reg$Approved,ensemble_prediction)
Roc_it_Ensemble<-rocit(score=ensemble_prediction,class=test_reg$Approved)
plot(Roc_it_Ensemble)
