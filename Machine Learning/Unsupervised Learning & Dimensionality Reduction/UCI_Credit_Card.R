set.seed(123)
require("caret")
require("cluster")
require("mclust")
require("fastICA")
require("neuralnet")
require("nFactors")
require("gmodels")
require("randomForest")
library(factoextra)

#########################
########Data Prep########
#########################

#set working directory
setwd("/Users/carolinewilliams/Desktop/Machine Learning/Assignment 3")

#load data
creditcard <- read.csv("UCI_Credit_Card.csv",  header=T, stringsAsFactors=FALSE, na.strings=c(","))

#clean data
creditcard <- na.omit(creditcard)

#shuffle data
shuffle_index <- sample(1:nrow(creditcard))
creditcard <- creditcard[shuffle_index, ]

#take a sample of the data
data <- creditcard[1:6000,]
vars <- data[,-1]

#scale variables
vars <- as.data.frame(scale(vars))
data <- data.frame(data$default.payment.next.month, vars)

#split the data into training and testing
set.seed(123)
#randomly sample 70% as training set
train <- sample(1:nrow(data), size = round(0.7*(nrow(data))))
d.train <- data[train,]
d.test <- data[-train,]

#########################
######## k-means ########
#########################
#euclidean distance 

# Compute and plot wss for k = 1 to k = 10.
k.max <- 10
wss <- sapply(1:k.max, 
              function(k){kmeans(d.train[,-1], k, nstart=50,iter.max = 15 )$tot.withinss})

#time it
for (i in (2:10)) {
  start <- Sys.time()
  kmeans(d.train[,-1], i, nstart=50,iter.max = 15 )
  print(Sys.time() - start)}
#Time difference of 0.2643402 secs
#Time difference of 0.3170531 secs
#Time difference of 0.4616539 secs
#Time difference of 0.6625409 secs
#Time difference of 0.6319189 secs
#Time difference of 0.8047719 secs
#Time difference of 0.722856 secs
#Time difference of 0.86921 secs
#Time difference of 0.9114201 secs


#plot elbow graph to see where bend occurs to choose optimal clusters
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
#elbow method: k = 3


# K-Means Cluster Analysis
fit <- kmeans(d.train[,-1], 2, nstart = 50) 
#with 2 clusters for comparison

table(fit$cluster, d.train$data.default.payment.next.month)
#       0    1
#1 2714  791
#2  537  158


#"As the final result of k-means clustering result is sensitive to the random starting 
#assignments, we specify nstart = 25. This means that R will try 25 different random 
#starting assignments and then select the best results corresponding to the one with the 
#lowest within cluster variation. The default value of nstart in R is one. But, it's strongly 
#recommended to compute k-means clustering with a large value of nstart such as 25 or 50, in order 
#to have a more stable result."

# append cluster assignment
cluster_data <- data.frame(d.train, fit$cluster)

#http://www.sthda.com/english/rpkgs/factoextra/reference/fviz_cluster.html
#plot actual clusters
#install.packages("factoextra")
library(factoextra)
fviz_cluster(fit, cluster_data[, -1])

#accuracy
sum(diag(table(fit$cluster, d.train$data.default.payment.next.month)))/sum(table(fit$cluster, d.train$data.default.payment.next.month))
#0.6838095

#export data for NN
write.csv(cluster_data, file = "kmeans_NN.csv")

#########################
########    EM   ########
#########################
em_result <- Mclust(as.matrix(d.train[,-1]))
summary(em_result)

#plots
plot(em_result)
fviz_mclust(em_result, "classification", geom = "point")

for (i in (1:9)) {
  start <- Sys.time()
  Mclust(as.matrix(d.train[,-1]), G=i)
  print(Sys.time() - start)}
#Time difference of 0.05161881 secs
#Time difference of 14.46553 secs
#Time difference of 39.21539 secs
#Time difference of 31.90489 secs
#Time difference of 7.452278 secs
#Time difference of 8.887229 secs
#Time difference of 10.75651 secs
#Time difference of 10.36855 secs
#Time difference of 16.72268 secs

cluster_data_em <- data.frame(d.train, em_result$classification)

#export data for NN
write.csv(cluster_data_em, file = "em_NN.csv")

##############################
########      PCA     ########
##############################
pca_result <- prcomp(d.train[,-1])
summary(pca_result)

var <- pca_result$sdev^2
propvar <- var/sum(var)

plot(propvar, xlab = "Principal Component", ylab = "Proportion of Variance Explained",ylim = c(0,1), type = "b")

plot(cumsum(propvar), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0,1), type = "b")

#combine PCA (1-40) and class
cc_train_PCA <- data.frame(class = d.train$data.default.payment.next.month, pca_result$x)
cc_train_PCA <- cc_train_PCA[,1:3]
#seemed to be that only 3 variables according to PCA were worthwhile to use

#export data for NN
write.csv(cc_train_PCA, file = "pca_NN.csv")

########  PCA K-MEANS ########

# Compute and plot wss for k = 2 to k = 10.
k.max <- 10
wss <- sapply(1:k.max, 
              function(k){kmeans(cc_train_PCA[,-1], k, nstart=50,iter.max = 15 )$tot.withinss})
#time it
for (i in (2:10)) {
  start <- Sys.time()
  kmeans(cc_train_PCA[,-1], i, nstart=50,iter.max = 15 )
  print(Sys.time() - start)}
#Time difference of 0.05432701 secs
#Time difference of 0.0739429 secs
#Time difference of 0.101665 secs
#Time difference of 0.1265721 secs
#Time difference of 0.1291258 secs
#Time difference of 0.1582191 secs
#Time difference of 0.1662729 secs
#Time difference of 0.2081919 secs
#Time difference of 0.1944749 secs

#plot elbow graph to see where bend occurs to choose optimal clusters
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

fit <- kmeans(cc_train_PCA[,-1], 2, nstart = 50) 

table(fit$cluster, cc_train_PCA$class)
#       0    1
#1  531  156
#2 2720  793

#append cluster data
cluster_data <- data.frame(cc_train_PCA, fit$cluster)

fviz_cluster(fit, cluster_data[, -1])

#accuracy
sum(diag(table(fit$cluster, cc_train_PCA$class)))/sum(table(fit$cluster, cc_train_PCA$class))
#0.3152381

########  PCA EM ########

em_result <- Mclust(as.matrix(cc_train_PCA[,-1]))
summary(em_result)

plot(em_result)
fviz_mclust(em_result, "classification", geom = "point")

for (i in (1:9)) {
  start <- Sys.time()
  Mclust(as.matrix(cc_train_PCA[,-1]), G=i)
  print(Sys.time() - start)}
#Time difference of 0.008083105 secs
#Time difference of 2.274306 secs
#Time difference of 2.405205 secs
#Time difference of 2.907936 secs
#Time difference of 3.65455 secs
#Time difference of 3.028519 secs
#Time difference of 3.990232 secs
#Time difference of 4.015421 secs
#Time difference of 4.853505 secs

##############################
########      ICA     ########
##############################

# Write CSV in R to move data to python for ICA analysis
write.csv(d.train, file = "cc_train.csv")

#load in ICA data
cc_train_ICA <- read.csv("cc_ICA.csv",header=F, stringsAsFactors=FALSE, na.strings=c("?"))

cc_train_ICA <- data.frame(d.train$data.default.payment.next.month, cc_train_ICA)

names(cc_train_ICA) <- c("data.default.payment.next.month","LIMIT_BAL","SEX","EDUCATION","MARRIAGE","AGE","PAY_0","PAY_2","PAY_3","PAY_4","PAY_5","PAY_6","BILL_AMT1","BILL_AMT2","BILL_AMT3","BILL_AMT4","BILL_AMT5","BILL_AMT6","PAY_AMT1","PAY_AMT2","PAY_AMT3","PAY_AMT4","PAY_AMT5","PAY_AMT6")

#export data for NN
write.csv(cc_train_ICA, file = "ica_NN.csv")

########  ICA K-MEANS ########
# Compute and plot wss for k = 2 to k = 10.
k.max <- 10
wss <- sapply(1:k.max, 
              function(k){kmeans(cc_train_ICA[,-1], k, nstart=50,iter.max = 15 )$tot.withinss})
#time it
for (i in (2:10)) {
  start <- Sys.time()
  kmeans(cc_train_ICA[,-1], i, nstart=50,iter.max = 15 )
  print(Sys.time() - start)}
#Time difference of 0.2969339 secs
#Time difference of 0.4168699 secs
#Time difference of 0.5380249 secs
#Time difference of 0.61027 secs
#Time difference of 0.654444 secs
#Time difference of 0.7196469 secs
#Time difference of 0.6424358 secs
#Time difference of 0.6988602 secs
#Time difference of 0.7862849 secs

#plot elbow graph to see where bend occurs to choose optimal clusters
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

fit <- kmeans(cc_train_ICA[,-1], 2, nstart = 50) 

table(fit$cluster, cc_train_ICA$data.default.payment.next.month)
#       0    1
#1 2042  550
#2 1209  399

cluster_data <- data.frame(cc_train_ICA, fit$cluster)

fviz_cluster(fit, cluster_data[, -1])

#accuracy
sum(diag(table(fit$cluster, cc_train_ICA$data.default.payment.next.month)))/sum(table(fit$cluster, cc_train_ICA$data.default.payment.next.month))
#0.5811905

########  ICA EM ########
em_result <- Mclust(as.matrix(cc_train_ICA[,-1]))
summary(em_result)

plot(em_result)
fviz_mclust(em_result, "classification", geom = "point")

for (i in (1:9)) {
  start <- Sys.time()
  Mclust(as.matrix(cc_train_ICA[,-1]), G=i)
  print(Sys.time() - start)}
#Time difference of 0.04743814 secs
#Time difference of 18.73856 secs
#Time difference of 22.61594 secs
#Time difference of 14.12729 secs
#Time difference of 5.7422 secs
#Time difference of 9.212874 secs
#Time difference of 8.32923 secs
#Time difference of 8.342838 secs
#Time difference of 8.659419 secs

##############################
####Randomized Projections####
##############################

#load in Rand Proj data
cc_train_RP <- read.csv("cc_RP.csv",header=F, stringsAsFactors=FALSE, na.strings=c("?"))

cc_train_RP <- data.frame(d.train$data.default.payment.next.month, cc_train_RP)

names(cc_train_RP) <- c("data.default.payment.next.month","LIMIT_BAL","SEX","EDUCATION","MARRIAGE","AGE","PAY_0","PAY_2","PAY_3","PAY_4","PAY_5","PAY_6","BILL_AMT1","BILL_AMT2","BILL_AMT3","BILL_AMT4","BILL_AMT5","BILL_AMT6","PAY_AMT1","PAY_AMT2","PAY_AMT3","PAY_AMT4","PAY_AMT5","PAY_AMT6")

#export data for NN
write.csv(cc_train_RP, file = "rp_NN.csv")

########  RP K-MEANS ########
# Compute and plot wss for k = 2 to k = 10.
k.max <- 10
wss <- sapply(1:k.max, 
              function(k){kmeans(cc_train_RP[,-1], k, nstart=50,iter.max = 15 )$tot.withinss})
#time it
for (i in (2:10)) {
  start <- Sys.time()
  kmeans(cc_train_RP[,-1], i, nstart=50,iter.max = 15 )
  print(Sys.time() - start)}
#Time difference of 0.287935 secs
#Time difference of 0.443027 secs
#Time difference of 0.77372 secs
#Time difference of 0.6203539 secs
#Time difference of 0.944221 secs
#Time difference of 0.753583 secs
#Time difference of 0.8765271 secs
#Time difference of 0.9907069 secs
#Time difference of 0.9853742 secs

#plot elbow graph to see where bend occurs to choose optimal clusters
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

fit <- kmeans(cc_train_RP[,-1], 2, nstart = 50) 

table(fit$cluster, cc_train_RP$data.default.payment.next.month)
#       0    1
#1 1923  704
#2 1328  245

cluster_data <- data.frame(cc_train_RP, fit$cluster)

fviz_cluster(fit, cluster_data[, -1])

#accuracy
sum(diag(table(fit$cluster, cc_train_RP$data.default.payment.next.month)))/sum(table(fit$cluster, cc_train_RP$data.default.payment.next.month))
#0.5161905

########  RP EM ########
em_result <- Mclust(as.matrix(cc_train_RP[,-1]))
summary(em_result)

plot(em_result)
fviz_mclust(em_result, "classification", geom = "point")

for (i in (1:9)) {
  start <- Sys.time()
  Mclust(as.matrix(cc_train_RP[,-1]), G=i)
  print(Sys.time() - start)}
#Time difference of 0.04196095 secs
#Time difference of 25.63577 secs
#Time difference of 31.6074 secs
#Time difference of 43.87648 secs
#Time difference of 5.838932 secs
#Time difference of 7.351763 secs
#Time difference of 10.03691 secs
#Time difference of 11.14271 secs
#Time difference of 12.60172 secs


##############################
####   Information Gain   ####
##############################
rf <- randomForest(factor(data.default.payment.next.month) ~., data=d.train, importance = TRUE)
imp <- importance(rf, type=2)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("Importance") +
  ylab("") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=14))

featureImportance$pct = featureImportance$Importance / sum(featureImportance$Importance)

ggplot(featureImportance, aes(x=reorder(Feature, pct), y=pct)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("Importance") +
  ylab("") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=14))

featureImportance <- featureImportance[order(-featureImportance$Importance),]
featureImportance$pct_sum = cumsum(featureImportance$pct)

ggplot(featureImportance, aes(x=reorder(Feature, -pct), y=pct_sum)) +
  geom_bar(stat="identity", fill="#53cfff") +
  theme_dark(base_size=20) +
  ylab("Cumulative % Var Imp") +
  xlab("") +
  theme(axis.text.x=element_text(angle=90, hjust=1))
ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=10))

#The second measure is the total decrease in node impurities from splitting on the variable, averaged over all trees. 
#For classification, the node impurity is measured by the Gini index.

cc_train_RF <- d.train[,c("data.default.payment.next.month","SEX","MARRIAGE","EDUCATION","PAY_6","PAY_3","PAY_4","PAY_5","PAY_AMT5","PAY_AMT4","PAY_2")]
#vars chosen over 75% of cumulative variance

#export data for NN
write.csv(cc_train_RF, file = "rf_NN.csv")

#######  IG K-MEANS  #######

# Compute and plot wss for k = 2 to k = 10.
k.max <- 10
wss <- sapply(1:k.max, 
              function(k){kmeans(cc_train_RF[,-1], k, nstart=50,iter.max = 15 )$tot.withinss})
#time it
for (i in (2:10)) {
  start <- Sys.time()
  kmeans(cc_train_RF[,-1], i, nstart=50,iter.max = 15 )
  print(Sys.time() - start)}
#Time difference of 0.1281919 secs
#Time difference of 0.165504 secs
#Time difference of 0.192169 secs
#Time difference of 0.2389941 secs
#Time difference of 0.2505851 secs
#Time difference of 0.2495949 secs
#Time difference of 0.267653 secs
#Time difference of 0.2668002 secs
#Time difference of 0.3292859 secs

#plot elbow graph to see where bend occurs to choose optimal clusters
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
#seems to be at 2

fit <- kmeans(cc_train_RF[,-1], 2, nstart = 50) 

table(fit$cluster, cc_train_RF$data.default.payment.next.month)
#       0    1
#1 2085  731
#2 1166  218

cluster_data <- data.frame(cc_train_RF, fit$cluster)

fviz_cluster(fit, cluster_data[, -1])

sum(diag(table(fit$cluster, cc_train_RF$data.default.payment.next.month)))/sum(table(fit$cluster, cc_train_RF$data.default.payment.next.month))
#0.5483333

#######    IG EM     #######

em_result <- Mclust(as.matrix(cc_train_RF[,-1]))
summary(em_result)

plot(em_result)
fviz_mclust(em_result, "classification", geom = "point")

for (i in (1:9)) {
  start <- Sys.time()
  Mclust(as.matrix(cc_train_RF[,-1]), G=i)
  print(Sys.time() - start)}
#Time difference of 0.0224719 secs
#Time difference of 3.469701 secs
#Time difference of 3.330845 secs
#Time difference of 3.235967 secs
#Time difference of 3.438811 secs
#Time difference of 4.435154 secs
#Time difference of 3.287427 secs
#Time difference of 3.030371 secs
#Time difference of 3.018831 secs



