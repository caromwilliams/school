######################
####DECISION TREE#####
######################

library(ROCR)
library(caret)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(dplyr)

######################
#######MUSHROOM#######
######################
set.seed(678)

#set working directory
setwd("/Users/carolinewilliams/Desktop/Machine Learning/Assignment 1")

#load data
mushroom <- read.csv("Mushroom.csv",  header=T, stringsAsFactors=FALSE, na.strings=c("?"))

#clean data
mushroom <- na.omit(mushroom)
#data set goes from 8124 to 5644 which is still robust enough for our purposes
mushroom = subset(mushroom, select = -c(veil.type))

#shuffle data
shuffle_index <- sample(1:nrow(mushroom))
mushroom <- mushroom[shuffle_index, ]

#create training and testing with 80% for training and 20% for testing
train <- mushroom[sample(nrow(mushroom), floor(nrow(mushroom) * 0.8)), ]
test <- mushroom[-sample(nrow(mushroom), floor(nrow(mushroom)) * 0.8), ]

#check dimensions to make sure data sets split well for modeling
dim(train)
#4515   22
dim(test)
#1129   22

#the function prop.table() combined with table() is another way to verify if the randomization process is correct
prop.table(table(train$class))
#e         p 
#0.6234773 0.3765227
prop.table(table(test$class))
#e         p 
#0.6076174 0.3923826  
#in both datasets the amount of poisonous (p) is about the same at ~40%

#plot decision tree
fit <- rpart(class~., data = train, method = 'class')
rpart.plot(fit, extra = 106)

#prediction of test set
predict <-predict(fit, test, type = 'class')

#confusion matrix creation
table_matrix <- table(test$class, predict)
table_matrix
#      e   p
#  e 686   0
#  p   2 441

#accuracy pre-pruning
accuracy_test <- sum(diag(table_matrix)) / sum(table_matrix)
print(accuracy_test)
#0.9982285

#ROC
roc <- prediction(predict(fit, test, type = "prob")[,2], test$class)
roc_performance <- performance(roc, "tpr", "fpr")
plot(roc_performance)

#AUC
auc <- performance(roc, "auc")
auc_performance <- as.numeric(auc@y.values)
auc_performance
#0.9977427

#pruning
printcp(fit)
plotcp(fit)

prune_fit <- prune(fit, cp= 0.19) 
rpart.plot(prune_fit, extra = 106)

#predict with pruned tree
predict2 <-predict(prune_fit, test, type = 'class')

#confusion matrix
table_matrix <- table(test$class, predict2)
table_matrix
#      e   p
#  e 686   0
#  p  11 432

#accuracy post-pruning
accuracy_test <- sum(diag(table_matrix)) / sum(table_matrix)
print(accuracy_test)
#0.9902569

#ROC
roc <- prediction(predict(prune_fit, test, type = "prob")[,2], test$class)
roc_performance <- performance(roc, "tpr", "fpr")
plot(roc_performance)

#AUC
auc <- performance(roc, "auc")
auc_performance <- as.numeric(auc@y.values)
auc_performance
#0.9875847

######################
#####CREDIT CARD######
######################
set.seed(678)

#set working directory
setwd("/Users/carolinewilliams/Desktop/Machine Learning/Assignment 1")

#load data
creditcard <- read.csv("UCI_Credit_Card.csv",  header=T, stringsAsFactors=FALSE, na.strings=c(","))

#clean data
creditcard <- na.omit(creditcard)

#shuffle data
shuffle_index <- sample(1:nrow(creditcard))
creditcard <- creditcard[shuffle_index, ]

#create training and testing with 80% for training and 20% for testing
train <- creditcard[sample(nrow(creditcard), floor(nrow(creditcard) * 0.8)), ]
test <- creditcard[-sample(nrow(creditcard), floor(nrow(creditcard)) * 0.8), ]

#check dimensions to make sure data sets split well for modeling
dim(train)
#24000    24
dim(test)
#6000   24

#check split of data target variable
prop.table(table(train$default.payment.next.month))
#0         1 
#0.7784583 0.2215417 
prop.table(table(test$default.payment.next.month))
#0         1 
#0.7681667 0.2318333  

#plot decision tree
fit <- rpart(default.payment.next.month~., data = train, method = 'class')
rpart.plot(fit, extra = 106)

#prediction of test set
predict <-predict(fit, test, type = 'class')

#confusion matrix creation
table_matrix <- table(test$default.payment.next.month, predict)
table_matrix
#      0   1
#  0 4421  188
#  1  926  465

#accuracy pre-pruning
accuracy_test <- sum(diag(table_matrix)) / sum(table_matrix)
print(accuracy_test)
#0.8143333

#ROC
roc <- prediction(predict(fit, test, type = "prob")[,2], test$default.payment.next.month)
roc_performance <- performance(roc, "tpr", "fpr")
plot(roc_performance)

#AUC
auc <- performance(roc, "auc")
auc_performance <- as.numeric(auc@y.values)
auc_performance
#0.6467511


