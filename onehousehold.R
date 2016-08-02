# session_id
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

# one household
gp_connect = src_postgres(dbname = "gp_adw",
                          host = "csia4gpm01",
                          port = 5432,
                          user = "cpeng",
                          password = "changeme"
) 
## hh size = 2
## where ip_address = 202304721 and uid_hash = 906863937
data.table<- data.table(collect(tbl(gp_connect,sql("select * from cp_session_web2_2 ")))) 
raw_data1 <- as.data.frame(data.table)
dbDisconnect(gp_connect$con)


train_data1 <- raw_data1 %>% spread(web_id,count)
train_data1[is.na(train_data1)] <- 0
train <- train_data1[,-1]

###pca
y<- train
y.sca<-apply(y, 2, function(x)(x/sum(x)))
y.sca<-t(apply(y, 1, function(x)(x/sum(x))))
y.pr <- princomp(y.sca)
summary(y.pr)
screeplot(y.pr, type = "lines")
load <- loadings(y.pr)
plot(load)
comp <- data.frame(y.pr$scores[,1:4])


#pc <- prcomp(y,scale = T)
#summary(pc)
#biplot(pc)
#comp <- data.frame(pc$x[,1:4])
#plot(comp, pch=16, col=rgb(0,0,0,0.5))


# Multi 3D plot
plot3d(comp$Comp.1, comp$Comp.2,comp$Comp.3)
#plot3d(comp$PC1, comp$PC3, comp$PC4)


###################################
set.seed(1000)
kms <- vector('list',10)
for(K in 2:10) {
  kms[[K]] <- kmeans(comp,centers=K,nstart=100)
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
par(mfrow=c(1,1))
plot(2:10,sil.ave,type='l',xlab='K',ylab='Average silhouette width')

###########################################################################


gp_connect = src_postgres(dbname = "gp_adw",
                          host = "csia4gpm01",
                          port = 5432,
                          user = "cpeng",
                          password = "changeme"
) 
## hh size = 2
## where ip_address = 202304721 and uid_hash = 906863937
data.table<- data.table(collect(tbl(gp_connect,sql("select * from cp_web_category_2_2 ")))) 
raw_data2 <- as.data.frame(data.table)
dbDisconnect(gp_connect$con)


train_data2 <- raw_data2 %>% spread(category,count)
train_data2[is.na(train_data2)] <- 0
train <- train_data2[,-1]
##pca
y<- train
y.sca<-apply(y, 2, function(x)(x/sum(x)))
y.sca<-t(apply(y, 1, function(x)(x/sum(x))))
y.pr <- princomp(y.sca)
summary(y.pr)
screeplot(y.pr, type = "lines")
load <- loadings(y.pr)
plot(load)
comp <- data.frame(y.pr$scores[,1:4])



# Multi 3D plot
plot3d(comp$Comp.1, comp$Comp.2,comp$Comp.3)
#plot3d(comp$PC1, comp$PC3, comp$PC4)

###################################
set.seed(1000)
kms <- vector('list',10)
for(K in 2:10) {
  kms[[K]] <- kmeans(comp,centers=K,nstart=100)
}
sil.ave <- NULL # To collect silhouette's means for each K
number <- NULL
par(mfrow=c(3,3))
for(K in 2:10) {
  sil <- silhouette.km(comp,kms[[K]]$centers)
  sil.ave <- c(sil.ave,mean(sil))
  ssil <- sort_silhouette(sil,kms[[K]]$cluster)
  plot(ssil,type='h',xlab='Observations',ylab='Silhouettes')
  title(paste('K =',K))
  number <- table(kms[[k]]$cluster)
}
sil.ave
number
par(mfrow=c(1,1))
plot(2:10,sil.ave,type='l',xlab='K',ylab='Average silhouette width')
