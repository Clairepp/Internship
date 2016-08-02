library(data.table)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(randomForest)
library(car)

# setwd("C:/Users/cpeng/Documents/Pro/household/dataset")

## max
# Import the dataset
raw_data_max <- read_tsv("jpc_hh_demos_max.txt",col_name = F)
names(raw_data_max) <- c("ip_address","uid_hash","demo1","demo2","demo3","demo4","demo5","demo6","demo7","demo8","demo9","demo10","demo11","demo12","demo13","demo14","demo15","demo16","demo17","demo18","demo19","demo20","hh_size")
raw_data_max$ip_address<-as.character(raw_data_max$ip_address)
raw_data_max$uid_hash<-as.character(raw_data_max$uid_hash)
setorder(raw_data_max,ip_address)
hh_id<-data.frame(seq(1:796123))
names(hh_id) <- c("hh_id")
index_table_max <- cbind(hh_id,raw_data_max[,1:2])
train_data_max <-cbind(hh_id,raw_data_max[,-c(1,2)])

# plot
temp <- NULL
somePDFPath = "max.pdf"
pdf(file=somePDFPath,width=12.8, height=8) 
par(mfrow=c(5,2))
for (i in 1:50)
{
temp_max <- train_data_max[i,]
temp1_max<-gather(temp_max,demo,measurement,demo1:demo20)
plot(temp1_max$measurement,type = "l", xlab = "demo",ylab = "Probablity",ylim=c(0,0.5) )
# ggsave(plot1, file="plot_50.png")
}
dev.off()

data_observe_max <- train_data_max[1:50,]

#subplot

temp <- NULL
somePDFPath = "max.pdf"

pdf(file=somePDFPath,width=12.8, height=8) 
for (j in 1:9)
{
train_data_max_adj1 <- train_data_max[train_data_max$hh_size %in% c(j),]
par(mfrow=c(5,4))
for (i in 1:20)
{
  temp_max <- train_data_max_adj1[i,]
  temp1_max<-gather(temp_max,demo,measurement,demo1:demo20)
  plot(temp1_max$measurement,type = "l", xlab = "demo",ylab = "Probablity",ylim=c(0,0.5) )
}}
dev.off()

train_data_max_adj2 <- train_data_max[train_data_max$hh_size %in% c(1),]
temp <- NULL
somePDFPath = "max1.pdf"
pdf(file=somePDFPath,width=12.8, height=8) 
par(mfrow=c(5,2))
for (i in 1:50)
{
  temp_max <- train_data_max_adj2[i,]
  temp1_max<-gather(temp_max,demo,measurement,demo1:demo20)
  plot(temp1_max$measurement,type = "l", xlab = "demo",ylab = "Probablity",ylim=c(0,0.5) )
}
dev.off()



## average

raw_data_avg <- read_tsv("jpc_hh_demos_avg.txt",col_name = F)
names(raw_data_avg) <- c("ip_address","uid_hash","demo1","demo2","demo3","demo4","demo5","demo6","demo7","demo8","demo9","demo10","demo11","demo12","demo13","demo14","demo15","demo16","demo17","demo18","demo19","demo20","hh_size")
raw_data_avg$ip_address<-as.character(raw_data_avg$ip_address)
raw_data_avg$uid_hash<-as.character(raw_data_avg$uid_hash)
setorder(raw_data_avg,ip_address)
hh_id<-data.frame(seq(1:796123))
names(hh_id) <- c("hh_id")
index_table_avg <- cbind(hh_id,raw_data_avg[,1:2])
train_data_avg <-cbind(hh_id,raw_data_avg[,-c(1,2)])

# plot
temp <- NULL
somePDFPath = "avg.pdf"
pdf(file=somePDFPath,width=12.8, height=8) 
par(mfrow=c(5,2))
for (i in 1:50)
{
  temp_avg <- train_data_avg[i,]
  temp1_avg<-gather(temp_avg,demo,measurement,demo1:demo20)
  plot(temp1_avg$measurement,type = "l", xlab = "demo",ylab = "Probablity",ylim=c(0,0.5) )
  # ggsave(plot1, file="plot_50.png")
}
dev.off()


# feature and model
# train_data_long <- gather(train_data,demo,measurement,demo1:demo20)


train_max <- train_data_max[1:12000,c(-1)]
train_max$hh_size <- recode(train_max$hh_size,"1='1';2='2';3='3';else='4+'")
train_max$hh_size <- as.factor(train_max$hh_size)

train_avg <- train_data_avg[1:12000,c(-1)]
train_avg$hh_size <- as.factor(train_avg$hh_size)



# random forest
max.rf <- randomForest(hh_size ~ ., data=train_max, importance=TRUE,
                       proximity=TRUE)
print(max.rf)

avg.rf <- randomForest(hh_size ~ ., data=train_avg, importance=TRUE,
                        proximity=TRUE)
print(avg.rf)


# adjustment (subset)
train_data_max_adj <- train_data_max[train_data_max$hh_size %in% c(7,8,9),]
train_max_adj <- train_data_max_adj[1:2000,c(-1)]
train_max_adj$hh_size <- as.factor(train_max_adj$hh_size)
max_adj.rf <- randomForest(hh_size ~ ., data=train_max_adj, importance=TRUE,
                       proximity=TRUE)
print(max_adj.rf)

## standardization
train_data_max_adj <-scale(train_data_max[,-c(1,22)],scale = F )
train_data_max_adj <- cbind(train_data_max_adj,train_data_max[,22])
train_max_adj <- as.data.frame(train_data_max_adj[1:7000,])
train_max_adj$V21 <- as.factor(train_max_adj$V21)
max_adj.rf <- randomForest(V21 ~ ., data=train_max_adj, importance=TRUE,
                           proximity=TRUE)
print(max_adj.rf)



### transform

hh_size <- train_max[,21]
temp2 <- cbind(train_max[,-21],hh_size)



# random forest
max.rf <- randomForest(hh_size ~ ., data=temp2, importance=TRUE,
                       proximity=TRUE)
print(max.rf)
