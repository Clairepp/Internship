# hh_size = 2
library(tidyr)
library(dplyr)
library(data.table)
#require(RPostgreSQL) 
#install.packages("RPostgreSQL")
library(RPostgreSQL)
library(msos)
library(ggplot2)
library(car)
library(rgl)
library(FactoMineR)
library(cluster)
library(fpc)
#write.table(output,"PCA.txt")
#setwd("//csiadsta01/Data_Dump/cpeng/Household")
options(digits = 22)
gp_connect = src_postgres(dbname = "gp_adw",
                          host = "csia4gpm01",
                          port = 5432,
                          user = "cpeng",
                          password = "changeme"
) 
data.table.cp_session_month <- data.table(collect(tbl(gp_connect,sql("select * from cp_session_web1 ")))) 
raw_data <- as.data.frame(data.table.cp_session_month)
dbDisconnect(gp_connect$con)

train_data <- raw_data[,c(-1,-2)]
train_data <- train_data %>% spread(web_id,count)
train_data[is.na(train_data)] <- 0
train <- train_data[,-1]

## pca
y<- train
#y.sca <- scale(y)
#y.sca<-apply(y, 2, function(x)(x/sum(x)))
#y.sca<-t(apply(y, 1, function(x)(x/sum(x))))
y.pr <- princomp(y,cor = F)
summary(y.pr)
screeplot(y.pr, type = "lines")
load <- loadings(y.pr)
plot(load)
comp <- data.frame(y.pr$scores[,1:4])

# Multi 3D plot
plot3d(comp$Comp.1, comp$Comp.2,comp$Comp.3)
pairs(comp)

set.seed(1)
kms <- vector('list',10)
number <- vector('list',10)
for(K in 2:10) {
  kms[[K]] <- kmeans(comp,centers=K,nstart=10)
  number[[K]] <- table(kms[[K]]$cluster)
}
sil.ave <- NULL # To collect silhouette's means for each K
par(mfrow=c(3,3))
for(K in 2:10) {
  sil <- silhouette.km(comp,kms[[K]]$centers)
  sil.ave <- c(sil.ave,mean(sil))
  ssil <- sort_silhouette(sil,kms[[K]]$cluster)
  plot(ssil,type='h',xlab='Observations',ylab='Silhouettes')
  title(paste('K =',K))
}
sil.ave
number
par(mfrow=c(1,1))
plot(2:10,sil.ave,type='l',xlab='K',ylab='Average silhouette width')


###########################
## category



gp_connect = src_postgres(dbname = "gp_adw",
                          host = "csia4gpm01",
                          port = 5432,
                          user = "cpeng",
                          password = "changeme"
) 
data.table.cp_session_month <- data.table(collect(tbl(gp_connect,sql("select * from cp_session_categoty ")))) 
raw_data <- as.data.frame(data.table.cp_session_month)
dbDisconnect(gp_connect$con)

train_data <- raw_data[,c(-1,-2)]
train_data <- train_data %>% spread(category,count)
train_data[is.na(train_data)] <- 0
train <- train_data[,-1]

## pca
y<- train
#y.sca <- scale(y)
y.sca<-apply(y, 2, function(x)(x/sum(x)))
#y.sca<-t(apply(y, 1, function(x)(x/sum(x))))
y.pr <- princomp(y.sca)
summary(y.pr)
screeplot(y.pr, type = "lines")
load <- loadings(y.pr)
plot(load)
comp <- data.frame(y.pr$loadings[,1:4])

# Multi 3D plot
plot3d(comp$Comp.1, comp$Comp.2,comp$Comp.3)
pairs(comp)

set.seed(1)
kms <- vector('list',10)
number <- vector('list',10)
for(K in 2:10) {
  kms[[K]] <- kmeans(comp,centers=K,nstart=10)
  number[[K]] <- table(kms[[K]]$cluster)
}
sil.ave <- NULL # To collect silhouette's means for each K
par(mfrow=c(3,3))
for(K in 2:10) {
  sil <- silhouette.km(comp,kms[[K]]$centers)
  sil.ave <- c(sil.ave,mean(sil))
  ssil <- sort_silhouette(sil,kms[[K]]$cluster)
  plot(ssil,type='h',xlab='Observations',ylab='Silhouettes')
  title(paste('K =',K))
}
sil.ave
number
par(mfrow=c(1,1))
plot(2:10,sil.ave,type='l',xlab='K',ylab='Average silhouette width')

