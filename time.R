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
library(FactoMineR)
# setwd("//csiadsta01/Data_Dump/cpeng/Household")
# cp_session_time 2
# cp_session_time1 3
# cp_session_time3 4
# cp_session_time4 5
# cp_session_time5 6
# cp_session_time6 2
## cp_session_time61 2
## cp_session_time7 3
gp_connect = src_postgres(dbname = "gp_adw",
                          host = "csia4gpm01",
                          port = 5432,
                          user = "cpeng",
                          password = "changeme"
) 
options(digits = 22)

data.table <- data.table(collect(tbl(gp_connect,sql("select * from cp_session_time61")))) 
raw_data <- as.data.frame(data.table)
dbDisconnect(gp_connect$con)
train_data <- raw_data[,c(-1,-2)]

#train_data$count <- 1
#train_data <- train_data %>% spread(browsertype,count)
#train_data[is.na(train_data)] <- 0

train_data$date  <- as.numeric(format(train_data$session_start, "%d"))
train_data$hour <- as.numeric(format(raw_data$session_start, "%H"))
train <- train_data[,c(-1,-2)]


set.seed(1)
kms <- vector('list',10)
number <- vector('list',10)
for(K in 2:10) {
  kms[[K]] <- kmeans(train,centers=K,nstart=10)
  number[[K]] <- table(kms[[K]]$cluster)
}
sil.ave <- NULL # To collect silhouette's means for each K
par(mfrow=c(3,3))
for(K in 2:10) {
  sil <- silhouette.km(train,kms[[K]]$centers)
  sil.ave <- c(sil.ave,mean(sil))
  ssil <- sort_silhouette(sil,kms[[K]]$cluster)
  plot(ssil,type='h',xlab='Observations',ylab='Silhouettes')
  title(paste('K =',K))
}
sil.ave
number
par(mfrow=c(1,1))
plot(2:10,sil.ave,type='l',xlab='K',ylab='Average silhouette width')




y<- train
y.sca <- scale(y)
#y.sca<-apply(y, 2, function(x)(x/sum(x)))
#y.sca<-t(apply(y, 1, scale))
y.pr <- princomp(y.sca)
summary(y.pr)
#screeplot(y.pr, type = "lines")
load <- loadings(y.pr)
#plot(load)
comp <- data.frame(y.pr$scores[,1:2])

# Multi plot
plot(comp$Comp.1, comp$Comp.2)
plot3d(comp$Comp.1, comp$Comp.2)
#plot3d(comp$Dim.1,comp$Dim.2,comp$Dim.3)

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


###################################################
# household=2
gp_connect = src_postgres(dbname = "gp_adw",
                          host = "csia4gpm01",
                          port = 5432,
                          user = "cpeng",
                          password = "changeme"
) 
options(digits = 22)

## hh size = 2
data.table <- data.table(collect(tbl(gp_connect,sql("select * from cp_session_time_web")))) 
raw_data <- as.data.frame(data.table)
dbDisconnect(gp_connect$con)
train_data <- raw_data



#train_data <- train_data %>% spread(web_id,count)
#train_data[is.na(train_data)] <- 0
#


train_data$date  <- as.numeric(format(raw_data$session_start, "%d"))
train_data$hour <- as.numeric(format(raw_data$session_start, "%H"))

train <- train_data[,c(-1,-2,-4)]
set.seed(1)
kms <- vector('list',10)
number <- vector('list',10)
for(K in 2:10) {
  kms[[K]] <- kmeans(train,centers=K,nstart=10)
  number[[K]] <- table(kms[[K]]$cluster)
}
sil.ave <- NULL # To collect silhouette's means for each K
par(mfrow=c(3,3))
for(K in 2:10) {
  sil <- silhouette.km(train,kms[[K]]$centers)
  sil.ave <- c(sil.ave,mean(sil))
  ssil <- sort_silhouette(sil,kms[[K]]$cluster)
  plot(ssil,type='h',xlab='Observations',ylab='Silhouettes')
  title(paste('K =',K))
}
sil.ave
number
par(mfrow=c(1,1))
plot(2:10,sil.ave,type='l',xlab='K',ylab='Average silhouette width')
