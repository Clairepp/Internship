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
library(cluster)
library(fpc)
# setwd("//csiadsta01/Data_Dump/cpeng/Household")
# cp_session_time_two 2 (100)
# cp_session_time_three 3 (100)
# cp_session_time_four 4 (100)
#write.table(x,"raw_data.txt")
gp_connect = src_postgres(dbname = "gp_adw",
                          host = "csia4gpm01",
                          port = 5432,
                          user = "cpeng",
                          password = "changeme"
) 
options(digits = 22)

data.table <- data.table(collect(tbl(gp_connect,sql("select * from cp_session_time_four")))) 
raw_data <- as.data.frame(data.table)
dbDisconnect(gp_connect$con)

setorder(raw_data,ip_address)
train_data <- raw_data

#train_data$count <- 1
#train_data <- train_data %>% spread(browsertype,count)
#train_data[is.na(train_data)] <- 0

train_data$date  <- as.numeric(format(train_data$session_start, "%d"))
train_data$hour <- as.numeric(format(raw_data$session_start, "%H"))

train <- train_data[,-4]

train = data.table(train)
train[, id := paste(ip_address, uid_hash, sep = "")]
train <- data.frame(train)
train <-  train[,c(4,5,6,7)]
length(unique(train$id))


x <- list()
pamk.best <-list()
p <- list()
a<-list()
b<-list()
y <- list()
y.pr <-list()
comp <- list()
c <- list()
kms<-list()
number<-list()
k = NULL
# x <- train[which(train$id == '3939924481399633033'),]
for(i in 1:100)
{
  k = unique(train$id)
  x[[i]] =  train[which(train$id == k[i]),-4]
  y[[i]]<- x[[i]]
  y[[i]]<- scale(y[[i]])
  y.pr[[i]] <- princomp(y[[i]])
  comp[[i]] <- data.frame(y.pr[[i]]$scores[,1:2])
  
  
  pamk.best[[i]] <- pamk(comp[[i]])
  #cat("number of clusters estimated by optimum average silhouette width:", pamk.best[[i]]$nc, "\n")
  p[[i]] <- pam(comp[[i]], pamk.best[[i]]$nc)
  a[[i]] <- table(p[[i]]$clustering)
  b[[i]] <- pamk.best[[i]]$nc
  #c[[i]] <- clusGap(x[[i]], kmeans, 10, B = 100, verbose = interactive())
  
}


b <- matrix(b,ncol = 1)
b <- as.data.frame(b)
table(b$V1 %in% 2)




for(K in 2:10) {
  kms[[K]] <- kmeans(comp[[i]],centers=K,nstart=10)
  number[[K]] <- table(kms[[K]]$cluster)
}
sil.ave <- NULL # To collect silhouette's means for each K
#par(mfrow=c(3,3))
for(K in 2:10) {
  sil <- silhouette.km(comp[[i]],kms[[K]]$centers)
  sil.ave <- c(sil.ave,mean(sil))
  ssil <- sort_silhouette(sil,kms[[K]]$cluster)
  plot(ssil,type='h',xlab='Observations',ylab='Silhouettes')
  title(paste('K =',K))
}
c[[i]] <- sil.ave
c<- as.matrix(c,ncol = 9)
c <- as.data.frame(c)