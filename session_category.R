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
library(randomForest)
# setwd("//csiadsta01/Data_Dump/cpeng/Household")
options(digits = 22)

#######################################################
gp_connect = src_postgres(dbname = "gp_adw",
                          host = "csia4gpm01",
                          port = 5432,
                          user = "cpeng",
                          password = "changeme"
) 
## hh size = 4
## where ip_address = 136116921 and uid_hash = 326969734
data.table<- data.table(collect(tbl(gp_connect,sql("select * from cp_web_category_1_4 ")))) 
raw_data <- as.data.frame(data.table)
dbDisconnect(gp_connect$con)

##########################################################
train_data <- raw_data %>% spread(category,count)
train_data[is.na(train_data)] <- 0
train <- train_data[,-1]
## pca
y<- train
y.sca<-t(apply(y, 1, function(x)(x/sum(x))))
y.sca<-scale(y.sca)
y.pr <- princomp(y.sca)
summary(y.pr)
screeplot(y.pr, type = "lines")
load <- loadings(y.pr)
plot(load)
comp <- data.frame(y.pr$scores[,1:4])

pc <- prcomp(y,scale = T)
summary(pc)
biplot(pc)
comp <- data.frame(pc$x[,1:4])
plot(comp, pch=16, col=rgb(0,0,0,0.5))



## k-means
set.seed(1000)
kms <- vector('list',10)
for(K in 2:10) {
  kms[[K]] <- kmeans(y.sca,centers=K,nstart=10)
}
sil.ave <- NULL # To collect silhouette's means for each K
par(mfrow=c(3,3))
for(K in 2:10) {
  sil <- silhouette.km(y.sca,kms[[K]]$centers)
  sil.ave <- c(sil.ave,mean(sil))
  ssil <- sort_silhouette(sil,kms[[K]]$cluster)
  plot(ssil,type='h',xlab='Observations',ylab='Silhouettes')
  title(paste('K =',K))
}
sil.ave
par(mfrow=c(1,1))
plot(2:10,sil.ave,type='l',xlab='K',ylab='Average silhouette width')

########################################################################
gp_connect = src_postgres(dbname = "gp_adw",
                          host = "csia4gpm01",
                          port = 5432,
                          user = "cpeng",
                          password = "changeme"
) 
## hh size = 2
## where ip_address = 202304721 and uid_hash = 906863937
data.table<- data.table(collect(tbl(gp_connect,sql("select * from cp_web_category_2_2 ")))) 
raw_data <- as.data.frame(data.table)
dbDisconnect(gp_connect$con)

########################################################################
gp_connect = src_postgres(dbname = "gp_adw",
                          host = "csia4gpm01",
                          port = 5432,
                          user = "cpeng",
                          password = "changeme"
) 
## hh size = 5
## where ip_address = 216232522 and uid_hash = 1662689883
data.table<- data.table(collect(tbl(gp_connect,sql("select * from cp_web_category_3_5 ")))) 
raw_data <- as.data.frame(data.table)
dbDisconnect(gp_connect$con)

########################################################################
gp_connect = src_postgres(dbname = "gp_adw",
                          host = "csia4gpm01",
                          port = 5432,
                          user = "cpeng",
                          password = "changeme"
) 
## hh size = 3
## where ip_address = 393360640 and uid_hash = 146968015
data.table<- data.table(collect(tbl(gp_connect,sql("select * from cp_web_category_4_3 ")))) 
raw_data <- as.data.frame(data.table)
dbDisconnect(gp_connect$con)

########################################################################
gp_connect = src_postgres(dbname = "gp_adw",
                          host = "csia4gpm01",
                          port = 5432,
                          user = "cpeng",
                          password = "changeme"
) 
## hh size = 3
## where ip_address = 393360640 and uid_hash = 146968015
data.table<- data.table(collect(tbl(gp_connect,sql("select * from cp_web_pro_4_3  ")))) 
raw_data <- as.data.frame(data.table)
dbDisconnect(gp_connect$con)

train_data <- raw_data %>% spread(web_id,pro)
train_data[is.na(train_data)] <- 0
train <- train_data[,-1]
## pca

y<- train
#y.sca<-scale(y,scale = F)
y.pr <- princomp(y)
summary(y.pr)
screeplot(y.pr, type = "lines")
load <- loadings(y.pr)
plot(load)
comp <- data.frame(y.pr$scores[,1:8])
