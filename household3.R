# household=2
## main 
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
# setwd("//csiadsta01/Data_Dump/cpeng/Household")

gp_connect = src_postgres(dbname = "gp_adw",
                          host = "csia4gpm01",
                          port = 5432,
                          user = "cpeng",
                          password = "changeme"
) 
options(digits = 22)

## hh size = 4
data.table <- data.table(collect(tbl(gp_connect,sql("select * from cp_session_web1")))) 
raw_data <- as.data.frame(data.table)
dbDisconnect(gp_connect$con)
table(raw_data$count %in% 1)
raw_data <- filter(raw_data,count <16)

train_data <- raw_data %>% spread(web_id,count)
train_data[is.na(train_data)] <- 0
train <- train_data[,-1]

###################################
boxplot(raw_data$count)
raw_data1<-filter(raw_data,count > 4)
outliers = boxplot(raw_data1$count, plot=FALSE)$out
raw_data1<-raw_data1[raw_data1$count %in% outliers,]

train_data <- raw_data1 %>% spread(web_id,count)
train_data[is.na(train_data)] <- 0
train <- train_data[,-1]


## pca
y<- train
#y.sca <- scale(y)
#y.sca<-apply(y, 2, function(x)(x/sum(x)))
y.sca<-t(apply(y, 1, function(x)(x/sum(x))))
y.pr <- princomp(y.sca)
summary(y.pr)
screeplot(y.pr, type = "lines")
load <- loadings(y.pr)
plot(load)
comp <- data.frame(y.pr$scores[,1:3])

# Multi 3D plot
plot3d(comp$Comp.1, comp$Comp.2,comp$Comp.3)


## k-means
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


k = 7
n = 660
km = kmeans(comp, k, alg="Lloyd")
names(km)
be <- as.numeric(km$betweenss/(k-1))
wi <- as.numeric((km$withinss)/(n-k))
ch <- sum(be/wi)

a <- as.matrix(km$centers)
b <- as.matrix(comp)

get_CH(b,a)


kms <- kmeans(comp,centers=4,nstart=10)
predict <- data.frame(kms$cluster)
table(kms$cluster)


library(clues)
set.seed(1)
kms <- vector('list',10)
for(K in 2:10) {
  kms[[K]] <- kmeans(comp,centers=K,nstart=10)
}
sil.ave <- NULL # To collect silhouette's means for each K
par(mfrow=c(3,3))
for(K in 2:10) {
  comp <- as.matrix(comp)
  kms[[K]] <- as.matrix(kms[[K]]$centers)
  sil <- get_Silhouette(comp,kms[[K]])
  sil.ave <- sil$avg.s
  }
sil.ave
par(mfrow=c(1,1))
plot(2:10,sil.ave,type='l',xlab='K',ylab='Average silhouette width')
