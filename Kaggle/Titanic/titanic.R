setwd("E:/Kaggle/Titanic")

library(dplyr)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(e1071)
library(caret)
library(xgboost)

train <- read.csv("train.csv")
test <- read.csv("test.csv")


str(train)
#Selecting variables to predict survival 
#Response - Survived
#Predictor - PClass, Sex, Age, SibSp, Parch, Fare, Cabin, Embarked

#We left out the PassengerID, Name and Ticket since they may not really affect the rates of survival

###look for methods of:
#1. Filling in empty rows
#2. Using methods that can allow missing data

###trying out logistic regression
vals <- sample(1:nrow(train), 0.75*nrow(train))
tr_train <- train[vals,]
ts_train <- train[-vals,]
model_log <- glm(Survived~Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = tr_train, family = "binomial")
summary(model_log)
ts_train$pred_mod <- predict(model_log, newdata = ts_train, type = "response")
ts_train$pred_sur <- round(ts_train$pred_mod)
table(ts_train$Survived, ts_train$pred_sur)
ts_train$pred_sur2 <- ifelse(ts_train$pred_mod>=0.6,1,0)
table(ts_train$Survived, ts_train$pred_sur2)
###does not work well due to missing values

###methods that can use missing fields
##svm
model_svm <- svm(Survived~Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = tr_train)
summary(model_svm)
ts_train$pred_svm <- predict(model_svm, newdata = ts_train, type = "response")
ts_train$pred_sur3 <- ifelse(ts_train$pred_svm>=0.6,1,0)
table(ts_train$Survived, ts_train$pred_sur3)

#Making the model
model1 <- rpart(Survived~Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = tr_train, method = "class")
summary(model1)
fancyRpartPlot(model1)

#Testing
pred1 <- predict(model1,ts_train,type="class")
table_mat <- table(ts_train$Survived, pred1)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test
#accuracy_tune <- function(model1) {
#    predict_unseen <- predict(model1, ts_train, type = 'class')
#    table_mat <- table(ts_train$survived, predict_unseen)
#    accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
#    accuracy_Test
#}
control <- rpart.control(minsplit = 4,minbucket = round(5 / 3),maxdepth = 3,cp = 0)
tune_fit <- rpart(Survived~Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = tr_train, method = 'class', control = control)
#accuracy_tune(tune_fit)
predict_unseen <- predict(tune_fit, ts_train, type = 'class')
table_mat <- table(ts_train$Survived, predict_unseen)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test


#submission <- data.frame(test$PassengerId,pred1)
#table(submission$test.PassengerId, submission$pred1)
#names(submission) <- c("PassengerId","Survived")
#write.csv(submission,"submission.csv")

