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
# setwd("//csiadsta01/Data_Dump/cpeng/Household")

gp_connect = src_postgres(dbname = "gp_adw",
                          host = "csia4gpm01",
                          port = 5432,
                          user = "cpeng",
                          password = "changeme"
) 
options(digits = 22)

## hh size = 4
data.table.cp_session_month <- data.table(collect(tbl(gp_connect,sql("select * from cp_session_web1 ")))) 
raw_data <- as.data.frame(data.table.cp_session_month)
dbDisconnect(gp_connect$con)

#########################################################################
train_data <- raw_data %>% spread(web_id,count)
train_data[is.na(train_data)] <- 0
train <- train_data[,-1]

###pca
y<- train
y.sca<-scale(y,scale=T)
y.pr <- princomp(y.sca)
summary(y.pr)
screeplot(y.pr, type = "lines")
load <- loadings(y.pr)
plot(load)
#comp <- data.frame(y.pr$scores[,1:4])

pc <- prcomp(y,scale = T)
summary(pc)
biplot(pc)
comp <- data.frame(pc$x[,1:4])
plot(comp, pch=16, col=rgb(0,0,0,0.5))

#library(rgl)
# Multi 3D plot
#plot3d(comp$PC1, comp$PC2,comp$PC3)
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

#############################################
# hh_size = 4
gp_connect = src_postgres(dbname = "gp_adw",
                          host = "csia4gpm01",
                          port = 5432,
                          user = "cpeng",
                          password = "changeme"
) 
data.table.cp_session_month <- data.table(collect(tbl(gp_connect,sql("select * from cp_session_web2_4 ")))) 
raw_data <- as.data.frame(data.table.cp_session_month)
dbDisconnect(gp_connect$con)

#raw_data <- read.delim("~/Pro/household/dataset/cp_session_web2_4.txt", header=FALSE)
#names(raw_data)<- c("session_id","web_id","count")
#############################################
# hh_size = 5
gp_connect = src_postgres(dbname = "gp_adw",
                          host = "csia4gpm01",
                          port = 5432,
                          user = "cpeng",
                          password = "changeme"
) 
data.table.cp_session_month <- data.table(collect(tbl(gp_connect,sql("select * from cp_session_web5_5 ")))) 
raw_data <- as.data.frame(data.table.cp_session_month)
dbDisconnect(gp_connect$con)
#############################################
# hh_size = 2
gp_connect = src_postgres(dbname = "gp_adw",
                          host = "csia4gpm01",
                          port = 5432,
                          user = "cpeng",
                          password = "changeme"
) 
data.table.cp_session_month <- data.table(collect(tbl(gp_connect,sql("select * from cp_session_web6_2 ")))) 
raw_data <- as.data.frame(data.table.cp_session_month)
dbDisconnect(gp_connect$con)

######################################################
# hh_size = 3
gp_connect = src_postgres(dbname = "gp_adw",
                          host = "csia4gpm01",
                          port = 5432,
                          user = "cpeng",
                          password = "changeme"
) 
data.table.cp_session_month <- data.table(collect(tbl(gp_connect,sql("select * from cp_session_web7_3 ")))) 
raw_data <- as.data.frame(data.table.cp_session_month)
dbDisconnect(gp_connect$con)


######################################################
# hh_size = 6
gp_connect = src_postgres(dbname = "gp_adw",
                          host = "csia4gpm01",
                          port = 5432,
                          user = "cpeng",
                          password = "changeme"
) 
data.table.cp_session_month <- data.table(collect(tbl(gp_connect,sql("select * from cp_session_web8_7 ")))) 
raw_data <- as.data.frame(data.table.cp_session_month)
dbDisconnect(gp_connect$con)

######################################################
# hh_size = 1
gp_connect = src_postgres(dbname = "gp_adw",
                          host = "csia4gpm01",
                          port = 5432,
                          user = "cpeng",
                          password = "changeme"
) 
data.table.cp_session_month <- data.table(collect(tbl(gp_connect,sql("select * from cp_session_web9_1 ")))) 
raw_data <- as.data.frame(data.table.cp_session_month)
dbDisconnect(gp_connect$con)

gp_connect = src_postgres(dbname = "gp_adw",
                          host = "csia4gpm01",
                          port = 5432,
                          user = "cpeng",
                          password = "changeme"
) 
data.table.cp_session_month <- data.table(collect(tbl(gp_connect,sql("select * from cp_session_web10_1 ")))) 
raw_data <- as.data.frame(data.table.cp_session_month)
dbDisconnect(gp_connect$con)








#################################################################
kms_2 <- kmeans(comp,centers=2,nstart=10)
prediction<- as.data.frame(kms_2$cluster)

names(prediction) <- c("prediction")
data.predic_2 <- cbind(prediction,train_data)


kms_5 <- kmeans(comp,centers=5,nstart=10)
prediction<- as.data.frame(kms_5$cluster)

names(prediction) <- c("prediction")
data.predic_5 <- cbind(prediction,train_data)
