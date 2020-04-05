######################
#######BOOSTING#######
######################

library(ROCR)
library(rpart)	
library(rpart.plot)
library(caret)
library(ggplot2)
library(gbm)

set.seed(678)

#set working directory
setwd("/Users/carolinewilliams/Desktop/Machine Learning/Assignment 1")

######################
#######MUSHROOM#######
######################

#load data
mushroom <- read.csv("Mushroom.csv",  header=T, stringsAsFactors=FALSE, na.strings=c("?"))

#clean data
data <- na.omit(mushroom)
data = subset(mushroom, select = -c(veil.type))

#data$class <- as.factor(data$class)
#data$class <-as.numeric(data$class)

data$class[mushroom$class =='p'] <- 1
data$class[mushroom$class =='e'] <- 0

data$cap.shape <- as.factor(data$cap.shape)
data$cap.shape <- as.numeric(data$cap.shape)

data$cap.surface <- as.factor(data$cap.surface)
data$cap.surface <- as.numeric(data$cap.surface)

data$cap.color <- as.factor(data$cap.color)
data$cap.color <- as.numeric(data$cap.color)

data$bruises. <- as.factor(data$bruises.)
data$bruises. <- as.numeric(data$bruises.)

data$odor <- as.factor(data$odor)
data$odor <- as.numeric(data$odor)

data$gill.attachment <- as.factor(data$gill.attachment)
data$gill.attachment <- as.numeric(data$gill.attachment)

data$gill.spacing <- as.factor(data$gill.spacing)
data$gill.spacing <- as.numeric(data$gill.spacing)

data$gill.size <- as.factor(data$gill.size)
data$gill.size <- as.numeric(data$gill.size)

data$gill.color <- as.factor(data$gill.color)
data$gill.color <- as.numeric(data$gill.color)

data$stalk.shape <- as.factor(data$stalk.shape)
data$stalk.shape <- as.numeric(data$stalk.shape)

data$stalk.root <- as.factor(data$stalk.root)
data$stalk.root <- as.numeric(data$stalk.root)

data$stalk.surface.above.ring <- as.factor(data$stalk.surface.above.ring)
data$stalk.surface.above.ring <- as.numeric(data$stalk.surface.above.ring)

data$stalk.surface.below.ring <- as.factor(data$stalk.surface.below.ring)
data$stalk.surface.below.ring <- as.numeric(data$stalk.surface.below.ring)

data$stalk.color.above.ring <- as.factor(data$stalk.color.above.ring)
data$stalk.color.above.ring <- as.numeric(data$stalk.color.above.ring)

data$stalk.color.below.ring <- as.factor(data$stalk.color.below.ring)
data$stalk.color.below.ring <- as.numeric(data$stalk.color.below.ring)

data$veil.color <- as.factor(data$veil.color)
data$veil.color <- as.numeric(data$veil.color)

data$ring.number <- as.factor(data$ring.number)
data$ring.number <- as.numeric(data$ring.number)

data$ring.type <- as.factor(data$ring.type)
data$ring.type <- as.numeric(data$ring.type)

data$spore.print.color <- as.factor(data$spore.print.color)
data$spore.print.color <- as.numeric(data$spore.print.color)

data$population <- as.factor(data$population)
data$population <- as.numeric(data$population)

data$habitat <- as.factor(data$habitat)
data$habitat <- as.numeric(data$habitat)

#shuffle data
shuffle_index <- sample(1:nrow(data))
mushroom <- data[shuffle_index, ]

#train and test sets
train <- mushroom[sample(nrow(mushroom), floor(nrow(mushroom) * 0.8)), ]
test <- mushroom[-sample(nrow(mushroom), floor(nrow(mushroom)) * 0.8), ]

#the function prop.table() combined with table() is another way to verify if the randomization process is correct
prop.table(table(train$class))
#e         p 
#0.5234651 0.4765349  
prop.table(table(test$class))
#e         p 
#0.5341538 0.4658462

#creating boosting model
boost <- gbm.fit(train[,2:ncol(train)], train$class,
                 ,distribution ="bernoulli"
                 ,n.trees = iter
                 ,nTrain = nrow(train)
                 ,keep.data=FALSE
                 ,verbose = TRUE)

summary(boost)

#predict with test data
predict_mushroom <- predict.gbm(boost, test[,2:ncol(test)], type = 'response', n.trees=iter)
predict_mushroom
pred <- ifelse(predict_mushroom >.5, 1, 0)
pred

#create confusion matrix
table(test$class, pred)
#      0   1
#  0 850  18
#  1  29 728

#accuracy
accuracy <- sum(diag(table(test$class, pred))) / sum(table(test$class, pred)) 
accuracy
#0.9710769


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

#take a sample of the data
data <- creditcard[1:6000,]
str(data)

data$ID <- as.factor(data$ID)
data$ID <- as.numeric(data$ID)

data$LIMIT_BAL <- as.factor(data$LIMIT_BAL)
data$LIMIT_BAL <- as.numeric(data$LIMIT_BAL)

data$SEX <- as.factor(data$SEX)
data$SEX <- as.numeric(data$SEX)

data$EDUCATION <- as.factor(data$EDUCATION)
data$EDUCATION <- as.numeric(data$EDUCATION)

data$MARRIAGE <- as.factor(data$MARRIAGE)
data$MARRIAGE <- as.numeric(data$MARRIAGE)

data$AGE <- as.factor(data$AGE)
data$AGE <- as.numeric(data$AGE)

data$PAY_0 <- as.factor(data$PAY_0)
data$PAY_0 <- as.numeric(data$PAY_0)

data$PAY_2 <- as.factor(data$PAY_2)
data$PAY_2 <- as.numeric(data$PAY_2)

data$PAY_3 <- as.factor(data$PAY_3)
data$PAY_3 <- as.numeric(data$PAY_3)

data$PAY_4 <- as.factor(data$PAY_4)
data$PAY_4 <- as.numeric(data$PAY_4)

data$PAY_5 <- as.factor(data$PAY_5)
data$PAY_5 <- as.numeric(data$PAY_5)

data$PAY_6 <- as.factor(data$PAY_6)
data$PAY_6 <- as.numeric(data$PAY_6)

#train and test sets
train <- data[sample(nrow(data), floor(nrow(data) * 0.8)), ]
test <- data[-sample(nrow(data), floor(nrow(data)) * 0.8), ]

#check randomization
prop.table(table(train$default.payment.next.month))
#0         1 
#0.7725 0.2275  
prop.table(table(test$default.payment.next.month))
#0         1 
#0.7833333 0.2166667 

#create boosting model
boost <- gbm.fit(train[,2:ncol(train)], train$default.payment.next.month,
                 ,distribution ="bernoulli"
                 ,n.trees = iter
                 ,nTrain = nrow(train)
                 ,keep.data=FALSE
                 ,verbose = TRUE)

summary(boost)

#predict with test data
predict_cc <- predict.gbm(boost, test[,2:ncol(test)], type = 'response', n.trees=iter)
predict_cc
pred <- ifelse(predict_cc >.5, 1, 0)
pred

#create confusion matrix
table(test$default.payment.next.month, pred)
#      0   1
#  0 894  46
#  1 180  80

#accuracy
accuracy <- sum(diag(table(test$default.payment.next.month, pred))) / sum(table(test$default.payment.next.month, pred)) 
accuracy
#0.8116667
