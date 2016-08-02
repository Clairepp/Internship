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
# cp_session_web_four 4
# cp_session_web_three 3
# cp_session_web 2 (100)
# cp_session_categoty 2
# write.table(x,"raw_data.txt")
gp_connect = src_postgres(dbname = "gp_adw",
                          host = "csia4gpm01",
                          port = 5432,
                          user = "cpeng",
                          password = "changeme"
) 
options(digits = 22)

data.table <- data.table(collect(tbl(gp_connect,sql("select * from cp_session_category")))) 
raw_data <- as.data.frame(data.table)
dbDisconnect(gp_connect$con)

setorder(raw_data,ip_address)

train_data = data.table(raw_data)
train_data[, id := paste(ip_address, uid_hash, sep = "")]
train_data <- data.frame(train_data)

train <- train_data[,c(-1,-2)]

x <- list()
p <- list()
a<-list()
b<-list()
y <- list()
y.pr <-list()
comp <- list()
c <- list()


k = NULL

for (i in 1:100)
{
  k = unique(train$id)
  x[[i]] =  train[which(train$id == k[i]),c(-4)]
  x[[i]]<-x[[i]] %>% spread(category,count)
  x[[i]][is.na(x[[i]])] <- 0
  x[[i]] <- x[[i]][,-1]
  y[[i]]<-scale(x[[i]])
  y.pr[[i]] <- princomp(y[[i]])
  comp[[i]] <- data.frame(y.pr[[i]]$scores[,1:3])
  
  pamk.best[[i]] <- pamk(comp[[i]])
  #cat("number of clusters estimated by optimum average silhouette width:", pamk.best[[i]]$nc, "\n")
  p[[i]] <- pam(comp[[i]], pamk.best[[i]]$nc)
  a[[i]] <- table(p[[i]]$clustering)
  b[[i]] <- pamk.best[[i]]$nc
  
}

b <- matrix(b,ncol = 1)
b <- as.data.frame(b)
table(b$V1 %in% 2)
