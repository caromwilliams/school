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

#read in the data and remove NAs
data <- read.csv("Mushroom.csv",  header=T, stringsAsFactors=FALSE, na.strings=c("?"))
data <- na.omit(data)
data = subset(data, select = -c(veil.type))

#shuffle data
shuffle_index <- sample(1:nrow(data))
data <- creditcard[shuffle_index, ]

#split the data into training and testing
set.seed(123)
#randomly sample 70% as training set
train <- sample(1:nrow(data), size = round(0.7*(nrow(data))))
d.train <- data[train,]
d.test <- data[-train,]

#change to dummary vars
library(caret)
dummy <- dummyVars(~ ., data = d.train, fullRank = TRUE)
d.train <- as.data.frame(predict(dummy, d.train))
d.test <- as.data.frame(predict(dummy, d.test))


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
#Time difference of 0.5768309 secs
#Time difference of 0.9476008 secs
#Time difference of 0.8647718 secs
#Time difference of 0.8607469 secs
#Time difference of 1.062956 secs
#Time difference of 1.085875 secs
#Time difference of 1.04951 secs
#Time difference of 1.088582 secs
#Time difference of 1.176258 secs


#plot elbow graph to see where bend occurs to choose optimal clusters
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
#elbow method: k = 2


# K-Means Cluster Analysis
fit <- kmeans(d.train[,-1], 2, nstart = 50) 
#with 2 clusters for comparison

table(fit$cluster, d.train$class)
#       0    1
#1 2436  578
#2    0  937


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
sum(diag(table(fit$cluster, d.train$class)))/sum(table(fit$cluster, d.train$class))
#0.8537079

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
#Time difference of 0.2411449 secs
#Time difference of 11.24626 secs
#Time difference of 11.42866 secs
#Time difference of 12.26156 secs
#Time difference of 12.11033 secs
#Time difference of 12.87105 secs
#Time difference of 12.2252 secs
#Time difference of 12.67249 secs
#Time difference of 12.10303 secs

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

#combine PCA (1-20) and class
mushroom_train_PCA <- data.frame(class = d.train$classp, pca_result$x)
mushroom_train_PCA <- mushroom_train_PCA[,1:21]
#seemed to be that only 4 variables according to PCA were worthwhile to use




########  PCA K-MEANS ########
# Compute and plot wss for k = 2 to k = 10.
k.max <- 10
wss <- sapply(1:k.max, 
              function(k){kmeans(mushroom_train_PCA[,-1], k, nstart=50,iter.max = 15 )$tot.withinss})
#time it
for (i in (2:10)) {
  start <- Sys.time()
  kmeans(mushroom_train_PCA[,-1], i, nstart=50,iter.max = 15 )
  print(Sys.time() - start)}
#Time difference of 0.191956 secs
#Time difference of 0.209692 secs
#Time difference of 0.2431419 secs
#Time difference of 0.2652981 secs
#Time difference of 0.3119111 secs
#Time difference of 0.3508089 secs
#Time difference of 0.504425 secs
#Time difference of 0.3519781 secs
#Time difference of 0.3644428 secs

#plot elbow graph to see where bend occurs to choose optimal clusters
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
#k = 2

fit <- kmeans(mushroom_train_PCA[,-1], 2, nstart = 50) 

table(fit$cluster, mushroom_train_PCA$class)
#0    1
#1 2436  578
#2    0  937

#append cluster data
cluster_data <- data.frame(mushroom_train_PCA, fit$cluster)

fviz_cluster(fit, cluster_data[, -1])

#accuracy
sum(diag(table(fit$cluster, mushroom_train_PCA$class)))/sum(table(fit$cluster, mushroom_train_PCA$class))
#0.8537079

########  PCA EM ########

em_result <- Mclust(as.matrix(mushroom_train_PCA[,-1]))
summary(em_result)

plot(em_result)
fviz_mclust(em_result, "classification", geom = "point")

for (i in (1:9)) {
  start <- Sys.time()
  Mclust(as.matrix(mushroom_train_PCA[,-1]), G=i)
  print(Sys.time() - start)}
#Time difference of 0.03623796 secs
#Time difference of 3.684125 secs
#Time difference of 4.177082 secs
#Time difference of 4.230366 secs
#Time difference of 4.250784 secs
#Time difference of 4.467327 secs
#Time difference of 4.552049 secs
#Time difference of 4.841419 secs
#Time difference of 4.910665 secs


##############################
########      ICA     ########
##############################

# Write CSV in R to move data to python for ICA analysis
write.csv(d.train, file = "mushroom_train.csv")

#load in ICA data
#K-MEANS
mushroom_train_ICA_KMEANS <- read.csv("mushroom_ICA.csv",header=F, stringsAsFactors=FALSE, na.strings=c("?"))
mushroom_train_ICA_KMEANS <- data.frame(d.train$classp, mushroom_train_ICA_KMEANS)

#EM
mushroom_train_ICA_EM <- read.csv("mushroom_ICA copy.csv",header=F, stringsAsFactors=FALSE, na.strings=c("?"))
mushroom_train_ICA_EM <- data.frame(d.train$classp, mushroom_train_ICA_EM)


names(mushroom_train_ICA_KMEANS) <- c('class','cap.shapec',
                                      'cap.shapef',
                                      'cap.shapek',
                                      'cap.shapes',
                                      'cap.shapex',
                                      'cap.surfaceg',
                                      'cap.surfaces',
                                      'cap.surfacey',
                                      'cap.colorc',
                                      'cap.colore',
                                      'cap.colorg',
                                      'cap.colorn',
                                      'cap.colorp',
                                      'cap.colorw',
                                      'cap.colory',
                                      'bruises.t',
                                      'odorc',
                                      'odorf',
                                      'odorl',
                                      'odorm',
                                      'odorn',
                                      'odorp',
                                      'gill.attachmentf',
                                      'gill.spacingw',
                                      'gill.sizen',
                                      'gill.colorh',
                                      'gill.colork',
                                      'gill.colorn',
                                      'gill.colorp',
                                      'gill.colorr',
                                      'gill.coloru',
                                      'gill.colorw',
                                      'gill.colory',
                                      'stalk.shapet',
                                      'stalk.rootc',
                                      'stalk.roote',
                                      'stalk.rootr',
                                      'stalk.surface.above.ringk',
                                      'stalk.surface.above.rings',
                                      'stalk.surface.above.ringy',
                                      'stalk.surface.below.ringk',
                                      'stalk.surface.below.rings',
                                      'stalk.surface.below.ringy',
                                      'stalk.color.above.ringc',
                                      'stalk.color.above.ringg',
                                      'stalk.color.above.ringn',
                                      'stalk.color.above.ringp',
                                      'stalk.color.above.ringw',
                                      'stalk.color.above.ringy',
                                      'stalk.color.below.ringc',
                                      'stalk.color.below.ringg',
                                      'stalk.color.below.ringn',
                                      'stalk.color.below.ringp',
                                      'stalk.color.below.ringw',
                                      'stalk.color.below.ringy',
                                      'veil.colory',
                                      'ring.numbero',
                                      'ring.numbert',
                                      'ring.typel',
                                      'ring.typen',
                                      'ring.typep',
                                      'spore.print.colork',
                                      'spore.print.colorn',
                                      'spore.print.colorr',
                                      'spore.print.coloru',
                                      'spore.print.colorw',
                                      'populationc',
                                      'populationn',
                                      'populations',
                                      'populationv',
                                      'populationy',
                                      'habitatg',
                                      'habitatl',
                                      'habitatm',
                                      'habitatp',
                                      'habitatu')

#used to run EM
names(mushroom_train_ICA_EM) <- c('class', 'cap.shapec',
                               'cap.shapef',
                               'cap.shapek',
                               'cap.shapes',
                               'cap.shapex',
                               'cap.surfaceg',
                               'cap.surfaces',
                               'cap.surfacey',
                               'cap.colorc',
                               'cap.colore',
                               'cap.colorg',
                               'cap.colorn',
                               'cap.colorp',
                               'cap.colorw',
                               'cap.colory',
                               'bruises.t',
                               'odorc',
                               'odorf',
                               'odorl',
                               'odorm',
                               'odorn',
                               'odorp',
                               'gill.attachmentf',
                               'gill.spacingw',
                               'gill.sizen',
                               'gill.colorh',
                               'gill.colork',
                               'gill.colorn',
                               'gill.colorp',
                               'gill.colorr',
                               'gill.coloru',
                               'gill.colorw',
                               'gill.colory')

########  ICA K-MEANS ########
# Compute and plot wss for k = 2 to k = 10.
k.max <- 10
wss <- sapply(1:k.max, 
              function(k){kmeans(mushroom_train_ICA_KMEANS[,-1], k, nstart=50,iter.max = 15 )$tot.withinss})
#time it
for (i in (2:10)) {
  start <- Sys.time()
  kmeans(mushroom_train_ICA_KMEANS[,-1], i, nstart=50,iter.max = 15 )
  print(Sys.time() - start)}
#Time difference of 0.7850499 secs
#Time difference of 1.354524 secs
#Time difference of 1.832887 secs
#Time difference of 2.094429 secs
#Time difference of 2.469588 secs
#Time difference of 2.707982 secs
#Time difference of 2.895813 secs
#Time difference of 3.175745 secs
#Time difference of 2.988941 secs

#plot elbow graph to see where bend occurs to choose optimal clusters
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

fit <- kmeans(mushroom_train_ICA_KMEANS[,-1], 2, nstart = 50) 

table(fit$cluster, mushroom_train_ICA_KMEANS$class)
#       0    1
#1 1211  790
#2 1225  725

cluster_data <- data.frame(mushroom_train_ICA_KMEANS, fit$cluster)

fviz_cluster(fit, cluster_data[, -1])

#accuracy
sum(diag(table(fit$cluster, mushroom_train_ICA_KMEANS$class)))/sum(table(fit$cluster, mushroom_train_ICA_KMEANS$class))
#0.4900025

########  ICA EM ########
em_result <- Mclust(as.matrix(mushroom_train_ICA_EM[,-1]))
summary(em_result)

plot(em_result)
fviz_mclust(em_result, "classification", geom = "point")

for (i in (1:9)) {
  start <- Sys.time()
  Mclust(as.matrix(mushroom_train_ICA_EM[,-1]), G=i)
  print(Sys.time() - start)}
#Time difference of 0.07630491 secs
#Time difference of 1.196614 mins
#Time difference of 1.288573 mins
#Time difference of 6.958603 secs
#Time difference of 1.218444 mins
#Time difference of 7.434316 secs
#Time difference of 7.344275 secs
#Time difference of 7.632534 secs
#Time difference of 9.703845 secs

##############################
####Randomized Projections####
##############################

#load in Rand Proj data
mushroom_train_RP <- read.csv("mushroom_RP.csv",header=F, stringsAsFactors=FALSE, na.strings=c("?"))

mushroom_train_RP <- data.frame(d.train$classp, mushroom_train_RP)

names(mushroom_train_RP) <- c('class', 'cap-shape', 'cap-surface', 'cap-color', 'bruises?', 'odor', 'gill-attachment', 'gill-spacing', 'gill-size', 'gill-color',
                               'stalk-shape','stalk-root', 'stalk-surface-above-ring', 'stalk-surface-below-ring', 'stalk-color-above-ring', 'stalk-color-below-ring',
                               'veil-color', 'ring-number', 'ring-type', 'spore-print-color','population', 'habitat')

########  RP K-MEANS ########
# Compute and plot wss for k = 2 to k = 10.
k.max <- 10
wss <- sapply(1:k.max, 
              function(k){kmeans(mushroom_train_RP[,-1], k, nstart=50,iter.max = 15 )$tot.withinss})
#time it
for (i in (2:10)) {
  start <- Sys.time()
  kmeans(mushroom_train_RP[,-1], i, nstart=50,iter.max = 15 )
  print(Sys.time() - start)}
#Time difference of 0.4849441 secs
#Time difference of 0.7405391 secs
#Time difference of 0.8995211 secs
#Time difference of 1.049462 secs
#Time difference of 1.183981 secs
#Time difference of 1.254181 secs
#Time difference of 1.397479 secs
#Time difference of 1.561346 secs
#Time difference of 1.364441 secs

#plot elbow graph to see where bend occurs to choose optimal clusters
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

fit <- kmeans(mushroom_train_RP[,-1], 2, nstart = 50) 

table(fit$cluster, mushroom_train_RP$class)
#       0    1
#1    0  937
#2 2436  578

cluster_data <- data.frame(mushroom_train_RP, fit$cluster)

fviz_cluster(fit, cluster_data[, -1])

#accuracy
sum(diag(table(fit$cluster, mushroom_train_RP$class)))/sum(table(fit$cluster, mushroom_train_RP$class))
#0.1462921

########  RP EM ########
em_result <- Mclust(as.matrix(mushroom_train_RP[,-1]))
summary(em_result)

plot(em_result)
fviz_mclust(em_result, "classification", geom = "point")

for (i in (1:9)) {
  start <- Sys.time()
  Mclust(as.matrix(mushroom_train_RP[,-1]), G=i)
  print(Sys.time() - start)}
#Time difference of 0.229871 secs
#Time difference of 11.36573 secs
#Time difference of 11.56455 secs
#Time difference of 11.88943 secs
#Time difference of 12.47704 secs
#Time difference of 13.06208 secs
#Time difference of 13.09611 secs
#Time difference of 15.01089 secs
#Time difference of 13.70312 secs


##############################
####   Information Gain   ####
##############################
#read in the data and remove NAs
data <- read.csv("Mushroom.csv",  header=T, stringsAsFactors=FALSE, na.strings=c("?"))
data <- na.omit(data)
data = subset(data, select = -c(veil.type))

#shuffle data
shuffle_index <- sample(1:nrow(data))
data <- creditcard[shuffle_index, ]

#split the data into training and testing
set.seed(123)
#randomly sample 70% as training set
train <- sample(1:nrow(data), size = round(0.7*(nrow(data))))
d.trainRF <- data[train,]
d.testRF <- data[-train,]

d.trainRF$class <- as.factor(d.trainRF$class)


#d.trainRF$class[d.trainRF$class == 1] <- 0
#d.trainRF$class[d.trainRF$class == 2] <- 1

d.trainRF$cap.shape <- as.factor(d.trainRF$cap.shape)
#d.trainRF$cap.shape <- as.numeric(d.trainRF$cap.shape)

d.trainRF$cap.surface <- as.factor(d.trainRF$cap.surface)
#d.trainRF$cap.surface <- as.numeric(d.trainRF$cap.surface)

d.trainRF$cap.color <- as.factor(d.trainRF$cap.color)
#d.trainRF$cap.color <- as.numeric(d.trainRF$cap.color)

d.trainRF$bruises. <- as.factor(d.trainRF$bruises.)
#d.trainRF$bruises. <- as.numeric(d.trainRF$bruises.)

d.trainRF$odor <- as.factor(d.trainRF$odor)
#d.trainRF$odor <- as.numeric(d.trainRF$odor)

d.trainRF$gill.attachment <- as.factor(d.trainRF$gill.attachment)
#d.trainRF$gill.attachment <- as.numeric(d.trainRF$gill.attachment)

d.trainRF$gill.spacing <- as.factor(d.trainRF$gill.spacing)
#d.trainRF$gill.spacing <- as.numeric(d.trainRF$gill.spacing)

d.trainRF$gill.size <- as.factor(d.trainRF$gill.size)
#d.trainRF$gill.size <- as.numeric(d.trainRF$gill.size)

d.trainRF$gill.color <- as.factor(d.trainRF$gill.color)
#d.trainRF$gill.color <- as.numeric(d.trainRF$gill.color)

d.trainRF$stalk.shape <- as.factor(d.trainRF$stalk.shape)
#d.trainRF$stalk.shape <- as.numeric(d.trainRF$stalk.shape)

d.trainRF$stalk.root <- as.factor(d.trainRF$stalk.root)
#d.trainRF$stalk.root <- as.numeric(d.trainRF$stalk.root)

d.trainRF$stalk.surface.above.ring <- as.factor(d.trainRF$stalk.surface.above.ring)
#d.trainRF$stalk.surface.above.ring <- as.numeric(d.trainRF$stalk.surface.above.ring)

d.trainRF$stalk.surface.below.ring <- as.factor(d.trainRF$stalk.surface.below.ring)
#d.trainRF$stalk.surface.below.ring <- as.numeric(d.trainRF$stalk.surface.below.ring)

d.trainRF$stalk.color.above.ring <- as.factor(d.trainRF$stalk.color.above.ring)
#d.trainRF$stalk.color.above.ring <- as.numeric(d.trainRF$stalk.color.above.ring)

d.trainRF$stalk.color.below.ring <- as.factor(d.trainRF$stalk.color.below.ring)
#d.trainRF$stalk.color.below.ring <- as.numeric(d.trainRF$stalk.color.below.ring)

d.trainRF$veil.color <- as.factor(d.trainRF$veil.color)
#d.trainRF$veil.color <- as.numeric(d.trainRF$veil.color)

d.trainRF$ring.number <- as.factor(d.trainRF$ring.number)
#d.trainRF$ring.number <- as.numeric(d.trainRF$ring.number)

d.trainRF$ring.type <- as.factor(d.trainRF$ring.type)
#d.trainRF$ring.type <- as.numeric(d.trainRF$ring.type)

d.trainRF$spore.print.color <- as.factor(d.trainRF$spore.print.color)
#d.trainRF$spore.print.color <- as.numeric(d.trainRF$spore.print.color)

d.trainRF$population <- as.factor(d.trainRF$population)
#d.trainRF$population <- as.numeric(d.trainRF$population)

d.trainRF$habitat <- as.factor(d.trainRF$habitat)
#d.trainRF$habitat <- as.numeric(d.trainRF$habitat)

str(d.trainRF)

rf <- randomForest(class ~., data=d.trainRF, importance = TRUE)
imp <- importance(rf, type=2)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])
featureImportance

ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_dark(base_size=10) +
  xlab("Importance") +
  ylab("") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=10))

featureImportance$pct = featureImportance$Importance / sum(featureImportance$Importance)

ggplot(featureImportance, aes(x=reorder(Feature, pct), y=pct)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_dark(base_size=10) +
  xlab("Importance") +
  ylab("") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=10))


featureImportance <- featureImportance[order(-featureImportance$Importance),]
featureImportance$pct_sum = cumsum(featureImportance$pct)

featureImportance

ggplot(featureImportance, aes(x=reorder(Feature, -pct), y=pct_sum)) +
  geom_bar(stat="identity", fill="#53cfff") +
  theme_dark(base_size=10) +
  ylab("Cumulative % Var Imp") +
  xlab("") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=10))

#The second measure is the total decrease in node impurities from splitting on the variable, averaged over all trees. 
#For classification, the node impurity is measured by the Gini index.



mushroom_train_RF <- d.trainRF[,c("class","gill.attachment","veil.color","cap.shape","gill.spacing","ring.number","bruises.","cap.surface","gill.color","population","stalk.color.below.ring","cap.color","habitat","stalk.root","stalk.color.above.ring","gill.size","stalk.surface.below.ring")]
#vars chosen over 75% of cumulative variance

#return vars to dummy vars for k-means and EM
library(caret)
dummy <- dummyVars(~ ., data = mushroom_train_RF, fullRank = TRUE)
mushroom_train_RF <- as.data.frame(predict(dummy, mushroom_train_RF))



#######  IG K-MEANS  #######
# Compute and plot wss for k = 2 to k = 10.
k.max <- 10
wss <- sapply(1:k.max, 
              function(k){kmeans(mushroom_train_RF[,-1], k, nstart=50,iter.max = 15 )$tot.withinss})
#time it
for (i in (2:10)) {
  start <- Sys.time()
  kmeans(mushroom_train_RF[,-1], i, nstart=50,iter.max = 15 )
  print(Sys.time() - start)}
#Time difference of 0.355649 secs
#Time difference of 0.4907401 secs
#Time difference of 0.5904641 secs
#Time difference of 0.664623 secs
#Time difference of 0.8422632 secs
#Time difference of 0.8941021 secs
#Time difference of 0.9280829 secs
#Time difference of 0.9052839 secs
#Time difference of 1.06984 secs

#plot elbow graph to see where bend occurs to choose optimal clusters
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
#seems to be at 3

fit <- kmeans(mushroom_train_RF[,-1], 2, nstart = 50) 

table(fit$cluster, mushroom_train_RF$class.p)
#       0    1
#1 1321  574
#2 1115  941

cluster_data <- data.frame(mushroom_train_RF, fit$cluster)

fviz_cluster(fit, cluster_data[, -1])

sum(diag(table(fit$cluster, mushroom_train_RF$class.p)))/sum(table(fit$cluster, mushroom_train_RF$class.p))
#0.5725133

#######    IG EM     #######

em_result <- Mclust(as.matrix(mushroom_train_RF[,-1]))
summary(em_result)

plot(em_result)
fviz_mclust(em_result, "classification", geom = "point")

for (i in (1:9)) {
  start <- Sys.time()
  Mclust(as.matrix(mushroom_train_RF[,-1]), G=i)
  print(Sys.time() - start)}
#Time difference of 0.1680701 secs
#Time difference of 8.380239 secs
#Time difference of 9.23165 secs
#Time difference of 8.590707 secs
#Time difference of 9.47046 secs
#Time difference of 9.13161 secs
#Time difference of 9.210514 secs
#Time difference of 9.283259 secs
#Time difference of 9.324302 secs


