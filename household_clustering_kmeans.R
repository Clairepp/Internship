library(msos)
library(stats)
library(randomForest)
## maximum
train_data_max_cluster<-train_data_max[1:6000,c(-1,-22)]
## clustering

##########################
# find the k 
kms <- vector('list',10)
for(K in 2:10) {
  kms[[K]] <- kmeans(train_data_max_cluster,centers=K,nstart=10)
}
sil.ave <- NULL # To collect silhouette's means for each K
par(mfrow=c(3,3))
for(K in 2:10) {
  sil <- silhouette.km(train_data_max_cluster,kms[[K]]$centers)
  sil.ave <- c(sil.ave,mean(sil))
  ssil <- sort_silhouette(sil,kms[[K]]$cluster)
  plot(ssil,type='h',xlab='Observations',ylab='Silhouettes')
  title(paste('K =',K))
}
sil.ave
plot(2:10,sil.ave,type='l',xlab='K',ylab='Average silhouette width')
# k = 3
########################################

# devide the data into 3 groups
kms <- kmeans(train_data_max_cluster,centers = 4, nstart = 8000)


hh_cluster<-data.frame(kms$cluster)
names(hh_cluster)<- c("hh_cluster")
train_data_max_whole<-cbind(train_data_max[1:6000,],hh_cluster)

## group 1
train_data_max_group1 <- train_data_max_whole[train_data_max_whole$hh_cluster %in% 1,]
train_max_group1 <- train_data_max_group1[,c(-1,-23)]
train_max_group1$hh_size <- as.factor(train_max_group1$hh_size)
max_group1.rf <- randomForest(hh_size ~ ., data=train_max_group1, importance=TRUE,
                           proximity=TRUE)
print(max_group1.rf)

## group 2
train_data_max_group2 <- train_data_max_whole[train_data_max_whole$hh_cluster %in% 2,]
train_max_group2 <- train_data_max_group2[,c(-1,-23)]
train_max_group2$hh_size <- as.factor(train_max_group2$hh_size)
max_group2.rf <- randomForest(hh_size ~ ., data=train_max_group2, importance=TRUE,
                              proximity=TRUE)
print(max_group2.rf)

## group 3
train_data_max_group3 <- train_data_max_whole[train_data_max_whole$hh_cluster %in% 3,]
train_max_group3 <- train_data_max_group3[,c(-1,-23)]
train_max_group3$hh_size <- as.factor(train_max_group3$hh_size)
max_group3.rf <- randomForest(hh_size ~ ., data=train_max_group3, importance=TRUE,
                              proximity=TRUE)
print(max_group3.rf)

## group 4
train_data_max_group4 <- train_data_max_whole[train_data_max_whole$hh_cluster %in% 3,]
train_max_group4 <- train_data_max_group4[,c(-1,-23)]
train_max_group4$hh_size <- as.factor(train_max_group4$hh_size)
max_group4.rf <- randomForest(hh_size ~ ., data=train_max_group4, importance=TRUE,
                              proximity=TRUE)
print(max_group4.rf)

## Average
train_data_avg_cluster<-train_data_avg[1:6000,c(-1,-22)]
## clustering
# devide the data into 3 groups
kms <- kmeans(train_data_avg_cluster,centers = 4, nstart = 8000)


hh_cluster<-data.frame(kms$cluster)
names(hh_cluster)<- c("hh_cluster")
train_data_avg_whole<-cbind(train_data_avg[1:6000,],hh_cluster)

## group 1
train_data_avg_group1 <- train_data_avg_whole[train_data_avg_whole$hh_cluster %in% 1,]
train_avg_group1 <- train_data_avg_group1[,c(-1,-23)]
train_avg_group1$hh_size <- as.factor(train_avg_group1$hh_size)
avg_group1.rf <- randomForest(hh_size ~ ., data=train_avg_group1, importance=TRUE,
                              proximity=TRUE)
print(avg_group1.rf)

## group 2
train_data_avg_group2 <- train_data_avg_whole[train_data_avg_whole$hh_cluster %in% 2,]
train_avg_group2 <- train_data_avg_group2[,c(-1,-23)]
train_avg_group2$hh_size <- as.factor(train_avg_group2$hh_size)
avg_group2.rf <- randomForest(hh_size ~ ., data=train_avg_group2, importance=TRUE,
                              proximity=TRUE)
print(avg_group2.rf)

## group 3
train_data_avg_group3 <- train_data_avg_whole[train_data_avg_whole$hh_cluster %in% 3,]
train_avg_group3 <- train_data_avg_group3[,c(-1,-23)]
train_avg_group3$hh_size <- as.factor(train_avg_group3$hh_size)
avg_group3.rf <- randomForest(hh_size ~ ., data=train_avg_group3, importance=TRUE,
                              proximity=TRUE)
print(avg_group3.rf)
