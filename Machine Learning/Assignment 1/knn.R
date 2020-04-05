######################
##########KNN#########
######################
library(class)
library(gmodels)
set.seed(678)

#set working directory
setwd("/Users/carolinewilliams/Desktop/Machine Learning/Assignment 1")

#load data
mushroom <- read.csv("Mushroom.csv",  header=T, stringsAsFactors=FALSE, na.strings=c("?"))

#clean data
mushroom <- na.omit(mushroom)
mushroom = subset(mushroom, select = -c(veil.type))

#shuffle data
shuffle_index <- sample(1:nrow(mushroom))
data <- mushroom[shuffle_index, ]

data$class <- as.factor(data$class)

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

str(data)

#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

data_n <- as.data.frame(lapply(data[2:22], normalize))

#create training and testing data sets using the 80/20 split
train <- data_n[1:4515,]
test <- data_n[4516:5644,]

train_labels <- data[1:4515, 1]
test_labels <- data[4516:5644, 1]

#create knn model
data_test_pred <- knn(train = train, test = test,cl = train_labels, k=10)

#evaluate model
CrossTable(x = test_labels, y = data_test_pred, prop.chisq=FALSE)

#accuracy = (TN+TP)/1129 = (672+457)/1129 = 1

#test different k values
data_test_pred2 <- knn(train = train, test = test,cl = train_labels, k=5)
CrossTable(x = test_labels, y = data_test_pred2, prop.chisq=FALSE)

data_test_pred3 <- knn(train = train, test = test,cl = train_labels, k=30)
CrossTable(x = test_labels, y = data_test_pred3, prop.chisq=FALSE)

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

data$default.payment.next.month <- as.factor(data$default.payment.next.month)

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

#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

data_n <- as.data.frame(lapply(data[2:22], normalize))

#create training and testing data sets using the 80/20 split
train <- data_n[1:4800,]
test <- data_n[4801:6000,]

train_labels <- data[1:4800, 1]
test_labels <- data[4801:6000, 1]

#create knn model
data_test_pred <- knn(train = train, test = test,cl = train_labels, k=10)

#evaluate model
CrossTable(x = test_labels, y = data_test_pred, prop.chisq=FALSE)

#accuracy = (TN+TP)/1129 = (889+67)/1200 = 0.7966666

#test different k values
data_test_pred2 <- knn(train = train, test = test,cl = train_labels, k=20)
CrossTable(x = test_labels, y = data_test_pred2, prop.chisq=FALSE)

data_test_pred3 <- knn(train = train, test = test,cl = train_labels, k=30)
CrossTable(x = test_labels, y = data_test_pred3, prop.chisq=FALSE)
#80.25%
