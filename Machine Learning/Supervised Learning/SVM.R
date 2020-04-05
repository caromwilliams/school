######################
##########SVM#########
######################

set.seed(678)

######################
#######MUSHROOM#######
######################

#set working directory
setwd("/Users/carolinewilliams/Desktop/Machine Learning/Assignment 1")

#read in the data and remove NAs
data <- read.csv("Mushroom.csv",  header=T, stringsAsFactors=FALSE, na.strings=c("?"))
data <- na.omit(data)
data = subset(data, select = -c(veil.type))

data$class <- as.factor(data$class)
data$class <- as.numeric(data$class)

data$class[data$class == 1] <- 0
data$class[data$class == 2] <- 1

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

library("e1071")

x <- subset(data, select=-class)
y <- data$class

svm_model <- svm(class ~ ., data=data)
summary(svm_model)

#Run Prediction and you can measuring the execution time in R
pred <- predict(svm_model,x)
system.time(pred <- predict(svm_model,x))
#user  system elapsed 
#0.131   0.001   0.133  

#confusion matrix
pred2 <- ifelse(pred >.5, 1, 0)
pred2

table(y, pred2)
#     0    1
#0 3488    0
#1    0 2156

#accuracy
accuracy <- sum(diag(table(y, pred2))) / sum(table(y, pred2)) 
accuracy #1

#tune
svm_tune <- tune(svm, train.x=x, train.y=y, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

print(svm_tune)

#re-run with new cost and gamma values
svm_model_after_tune <- svm(class ~ ., data=data, kernel="radial", cost=10, gamma=0.5)
summary(svm_model_after_tune)

#new predictions
pred <- predict(svm_model_after_tune,x)
system.time(predict(svm_model_after_tune,x))

#confusion matrix
pred2 <- ifelse(pred >.5, 1, 0)
pred2

table(y, pred2)
#     0    1
#  0 3488    0
#  1    0 2156  

#accuracy
accuracy <- sum(diag(table(y, pred2))) / sum(table(y, pred2)) 
accuracy

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

data$default.payment.next.month <- as.numeric(data$default.payment.next.month)

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

library("e1071")

x <- subset(data, select=-default.payment.next.month)
y <- data$default.payment.next.month

svm_model <- svm(default.payment.next.month ~ ., data=data)
summary(svm_model)

#Run Prediction and you can measuring the execution time in R
pred <- predict(svm_model,x)
system.time(pred <- predict(svm_model,x))
#user  system elapsed 
#0.884   0.008   0.920   

#confusion matrix
pred2 <- ifelse(pred >.5, 1, 0)
pred2

table(y, pred2)
#     0    1
#  0 4472  194
#  1  855  479

#accuracy
accuracy <- sum(diag(table(y, pred2))) / sum(table(y, pred2)) 
accuracy #0.8251667

#tune
svm_tune <- tune(svm, train.x=x, train.y=y, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

print(svm_tune)

#re-run with new cost and gamma values
svm_model_after_tune <- svm(default.payment.next.month ~ ., data=data, kernel="radial", cost=1, gamma=0.5)
summary(svm_model_after_tune)

#new predictions
pred <- predict(svm_model_after_tune,x)
system.time(predict(svm_model_after_tune,x))

#confusion matrix
pred2 <- ifelse(pred >.5, 1, 0)
pred2

table(y, pred2)
#     0    1
#  0 4616   50
#  1  524  810  

#accuracy
accuracy <- sum(diag(table(y, pred2))) / sum(table(y, pred2)) 
accuracy #0.9043333
