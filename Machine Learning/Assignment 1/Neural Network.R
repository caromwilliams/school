######################
####NEURAL NETWORK####
######################


######################
#######MUSHROOM#######
######################
set.seed(678)

#set working directory
setwd("/Users/carolinewilliams/Desktop/Machine Learning/Assignment 1")

#read in the data and remove NAs
data <- read.csv("Mushroom.csv",  header=T, stringsAsFactors=FALSE, na.strings=c("?"))
data <- na.omit(data)
data = subset(data, select = -c(veil.type))

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

#create vector of column max and min values
maxs <- apply(data[,2:21], 2, max)
mins <- apply(data[,2:21], 2, min)

#use scale() and convert resulting matrix to a data frame
scaled.data <- as.data.frame(scale(data[,2:21],center = mins, scale = maxs - mins))

#convert class column from e/p to 1/0
class = as.numeric(data$class)-1
data = cbind(class,scaled.data)

#load library caTools
library(caTools)
set.seed(678)

#create split for testing and training
split = sample.split(data$class, SplitRatio = 0.80)

#split based off of split vector
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

feats <- names(scaled.data)

#concatenate strings
f <- paste(feats,collapse=' + ')
f <- paste('class ~',f)

#convert to formula for model
f <- as.formula(f)

#create neural net
library(neuralnet)
nn <- neuralnet(f,train,hidden=c(10,10,10),linear.output=FALSE)

#compute predictions off test set
predicted.nn.values <- compute(nn,test[2:21])

#check out net.result
print(head(predicted.nn.values$net.result))

predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)

#create confusion matrix
table(test$class,predicted.nn.values$net.result)
#       0    1
#  0 698    0
#  1   0  431

#plot the neural network
plot(nn)

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
data <- creditcard[1:2000,]

data$default.payment.next.month <- as.factor(data$default.payment.next.month)

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

#create vector of column max and min values
maxs <- apply(data[,2:21], 2, max)
mins <- apply(data[,2:21], 2, min)

#use scale() and convert resulting matrix to a data frame
scaled.data <- as.data.frame(scale(data[,2:21],center = mins, scale = maxs - mins))

#convert class column from e/p to 1/0
class = as.numeric(data$default.payment.next.month)-1
data = cbind(class,scaled.data)

#load library caTools
library(caTools)
set.seed(678)

#create split for testing and training
split = sample.split(data$class, SplitRatio = 0.80)

#split based off of split vector
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

feats <- names(scaled.data)

#concatenate strings
f <- paste(feats,collapse=' + ')
f <- paste('class ~',f)

#convert to formula for model
f <- as.formula(f)

#create neural net
library(neuralnet)
nn <- neuralnet(f,train,hidden=c(10,10,10),linear.output=FALSE)

#compute predictions off test set
predicted.nn.values <- compute(nn,test[2:21])

#check out net.result
print(head(predicted.nn.values$net.result))

predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)

#create confusion matrix
table(test$class,predicted.nn.values$net.result)
#       0    1
#  0  264  43
#  1   53  40

#plot neural network
plot(nn)
