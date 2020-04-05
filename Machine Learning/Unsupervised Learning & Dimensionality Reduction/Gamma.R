set.seed(12)
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
setwd("/Users/sarah/Desktop/GA Tech Fall 2018/Machine Learning/Homework 3")

#load data
gamma_train <- read.csv("Gamma train scaled.csv",  header=F, stringsAsFactors=FALSE, na.strings=c("?"))
gamma_valid <- read.csv("Gamma valid scaled.csv",  header=F, stringsAsFactors=FALSE, na.strings=c("?"))

#column names
names(gamma_train) <- c('fLength', 'fWidth', 'fSize', 'fConc', 'fConc1','fAsym','fM3Long','fM3Trans','fAlpha','fDist','class')

names(gamma_valid) <- c('fLength', 'fWidth', 'fSize', 'fConc', 'fConc1','fAsym','fM3Long','fM3Trans','fAlpha','fDist','class')

gamma_train<-gamma_train[,c(11, 1:10)]
gamma_valid<-gamma_valid[,c(11, 1:10)]

#########################
######## k-means ########
#########################
#euclidean distance 

# Compute and plot wss for k = 1 to k = 10.
k.max <- 10
wss <- sapply(1:k.max, 
              function(k){kmeans(gamma_train[,-1], k, nstart=50,iter.max = 15 )$tot.withinss})

#time it
for (i in (2:10)) {
  start <- Sys.time()
  kmeans(gamma_train[,-1], i, nstart=50,iter.max = 15 )
  print(Sys.time() - start)}
#Time difference of 0.7218001 secs
#Time difference of 1.6458 secs
#Time difference of 1.4336 secs
#Time difference of 1.3152 secs
#Time difference of 1.5998 secs
#Time difference of 1.834 secs
#Time difference of 2.1098 secs
#Time difference of 2.5412 secs
#Time difference of 3.0384 secs

#plot elbow graph to see where bend occurs to choose optimal clusters
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
#elbow method: k = 4 


# K-Means Cluster Analysis
fit <- kmeans(gamma_train[,-1], 2, nstart = 50) 

table(fit$cluster, gamma_train$class)



#"As the final result of k-means clustering result is sensitive to the random starting 
#assignments, we specify nstart = 25. This means that R will try 25 different random 
#starting assignments and then select the best results corresponding to the one with the 
#lowest within cluster variation. The default value of nstart in R is one. But, it's strongly 
#recommended to compute k-means clustering with a large value of nstart such as 25 or 50, in order 
#to have a more stable result."

# append cluster assignment
cluster_data <- data.frame(gamma_train, fit$cluster)

#http://www.sthda.com/english/rpkgs/factoextra/reference/fviz_cluster.html
#plot actual clusters
#install.packages("factoextra")
library(factoextra)
fviz_cluster(fit, cluster_data[, -1])

sum(diag(table(fit$cluster, gamma_train$class)))/sum(table(fit$cluster, gamma_train$class))

#########################
########    EM   ########
#########################
em_result <- Mclust(as.matrix(gamma_train[,-1]))
summary(em_result)
plot(em_result)
fviz_mclust(em_result, "classification", geom = "point")

for (i in (1:9)) {
  start <- Sys.time()
  Mclust(as.matrix(gamma_train[,-1]), G=i)
  print(Sys.time() - start)}
#Time difference of 0.0822351 secs
#Time difference of 5.133021 secs
#Time difference of 6.048845 secs
#Time difference of 9.991731 secs
#Time difference of 17.45612 secs
#Time difference of 19.93405 secs
#Time difference of 27.76381 secs
#Time difference of 32.78314 secs

##############################
########      PCA     ########
##############################
pca_result <- prcomp(gamma_train[,-1])

summary(pca_result)

var <- pca_result$sdev^2
propvar <- var/sum(var)

plot(propvar, xlab = "Principal Component", ylab = "Proportion of Variance Explained",ylim = c(0,1), type = "b")

plot(cumsum(propvar), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0,1), type = "b")


#combine PCA (1-40) and class
gamma_train_PCA <- data.frame(class = gamma_train$class, pca_result$x)
gamma_train_PCA <- gamma_train_PCA[,1:4]

########  PCA K-MEANS ########

# Compute and plot wss for k = 2 to k = 10.
k.max <- 10
wss <- sapply(1:k.max, 
              function(k){kmeans(gamma_train_PCA[,-1], k, nstart=50,iter.max = 15 )$tot.withinss})
#time it
for (i in (2:10)) {
  start <- Sys.time()
  kmeans(gamma_train_PCA[,-1], i, nstart=50,iter.max = 15 )
  print(Sys.time() - start)}
#Time difference of 0.2735701 secs
#Time difference of 0.5639629 secs
#Time difference of 0.5820701 secs
#Time difference of 0.670064 secs
#Time difference of 0.951957 secs
#Time difference of 1.0235 secs
#Time difference of 1.26124 secs
#Time difference of 1.356853 secs
#Time difference of 1.693255 secs

#plot elbow graph to see where bend occurs to choose optimal clusters
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

fit <- kmeans(gamma_train_PCA[,-1], 2, nstart = 50) 

table(fit$cluster, gamma_train_PCA$class)

cluster_data <- data.frame(gamma_train_PCA, fit$cluster)

fviz_cluster(fit, cluster_data[, -1])


sum(diag(table(fit$cluster, gamma_train_PCA$class)))/sum(table(fit$cluster, gamma_train_PCA$class))

########  PCA EM ########

em_result <- Mclust(as.matrix(gamma_train_PCA[,-1]))
summary(em_result)
plot(em_result)
fviz_mclust(em_result, "classification", geom = "point")

for (i in (1:9)) {
  start <- Sys.time()
  Mclust(as.matrix(gamma_train_PCA[,-1]), G=i)
  print(Sys.time() - start)}
#Time difference of 0.02918696 secs
#Time difference of 3.886263 secs
#Time difference of 6.135894 secs
#Time difference of 7.9893 secs
#Time difference of 14.5731 secs
#Time difference of 19.49392 secs
#Time difference of 26.88189 secs
#Time difference of 23.55421 secs
#Time difference of 41.3664 secs

##############################
########      ICA     ########
##############################

ica_result <- fastICA(as.matrix(gamma_train[,-1]), n.comp = 3)
ica_result2 <- sapply(1:10, 
                     function(i){fastICA(as.matrix(gamma_train[,-1]), n.comp = i)$S})
summary(ica_result2)
ica_result2

plot(summary(ica_result)[,1])

clusplot(ica_result$S, gamma_train[,1], 
         color=TRUE, shade=FALSE, 
         labels=0, lines=0, xlim=c(-5,5), ylim=c(-5,5),
         main=NULL,xlab='',ylab='',sub=NULL)


plot(ica_result$S[,1], type="l",xlab='',ylab='')
plot(ica_result$S[,2], type="l",xlab='',ylab='')
plot(ica_result$S[,3], type="l",xlab='',ylab='')
plot(ica_result$S[,4], type="l",xlab='',ylab='')


########  ICA K-MEANS ########


##############################
####Randomized Projections####
##############################


rca_result <- rca(gamma_train[,1])

plot(rca_result)

clusplot(rca_result, pimadata_use[,9], 
         color=TRUE, shade=FALSE, 
         labels=0, lines=0, xlim=c(-4,6), ylim=c(-3,5),
         main=NULL,xlab='',ylab='',sub=NULL)


########  RP K-MEANS ########


##############################
####   Information Gain   ####
##############################
rf <- randomForest(factor(class) ~., data=gamma_train, importance = TRUE)
imp <- importance(rf, type=2)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("Importance") +
  ylab("") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))

featureImportance$pct = featureImportance$Importance / sum(featureImportance$Importance)

ggplot(featureImportance, aes(x=reorder(Feature, pct), y=pct)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("Importance") +
  ylab("") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))


featureImportance <- featureImportance[order(-featureImportance$Importance),]
featureImportance$pct_sum = cumsum(featureImportance$pct)

ggplot(featureImportance, aes(x=reorder(Feature, -pct), y=pct_sum)) +
  geom_bar(stat="identity", fill="#53cfff") +
  theme_light(base_size=20) +
  ylab("Cumulative % Var Imp") +
  xlab("") +
  theme(axis.text.x=element_text(angle=90, hjust=1))
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))

#The second measure is the total decrease in node impurities from splitting on the variable, averaged over all trees. 
#For classification, the node impurity is measured by the Gini index.
  
gamma_train_RF <- gamma_train[,c("class","fAlpha","fSize","fWidth","fLength","fDist","fM3Long")]

#######  IG K-MEANS  #######

# Compute and plot wss for k = 2 to k = 10.
k.max <- 10
wss <- sapply(1:k.max, 
              function(k){kmeans(gamma_train_RF[,-1], k, nstart=50,iter.max = 15 )$tot.withinss})
#time it
for (i in (2:10)) {
  start <- Sys.time()
  kmeans(gamma_train_RF[,-1], i, nstart=50,iter.max = 15 )
  print(Sys.time() - start)}
#Time difference of 0.303622 secs
#Time difference of 0.5113389 secs
#Time difference of 0.7450838 secs
#Time difference of 0.7755032 secs
#Time difference of 1.226569 secs
#Time difference of 1.692114 secs
#Time difference of 2.106132 secs
#Time difference of 2.242109 secs
#Time difference of 2.145252 secs

#plot elbow graph to see where bend occurs to choose optimal clusters
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

fit <- kmeans(gamma_train_RF[,-1], 2, nstart = 50) 

table(fit$cluster, gamma_train_RF$class)

cluster_data <- data.frame(gamma_train_RF, fit$cluster)

fviz_cluster(fit, cluster_data[, -1])

sum(diag(table(fit$cluster, gamma_train_RF$class)))/sum(table(fit$cluster, gamma_train_RF$class))
#0.7265379

#######    IG EM     #######

em_result <- Mclust(as.matrix(gamma_train_RF[,-1]))
summary(em_result)
plot(em_result)
fviz_mclust(em_result, "classification", geom = "point")

for (i in (1:9)) {
  start <- Sys.time()
  Mclust(as.matrix(gamma_train_RF[,-1]), G=i)
  print(Sys.time() - start)}
#Time difference of 0.05097413 secs
#Time difference of 4.129824 secs
#Time difference of 6.00812 secs
#Time difference of 7.654957 secs
#Time difference of 7.243806 secs
#Time difference of 13.1914 secs
#Time difference of 16.16808 secs
#Time difference of 16.16808 secs
#Time difference of 23.14366 secs
