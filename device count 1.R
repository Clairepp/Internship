library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(car)
library(GGally)
library(lvplot)

# setwd("C:/Users/cpeng/Documents/Pro/household/dataset")

# Import the dataset
raw_data_max_device <- read_tsv("jpc_hh_demos_max_device_counts.txt",col_name = F)
names(raw_data_max_device) <- c("hh_id","demo1","demo2","demo3","demo4","demo5","demo6","demo7","demo8","demo9","demo10","demo11","demo12","demo13","demo14","demo15","demo16","demo17","demo18","demo19","demo20","device1", "device2", "device3", "device4","hh_size")
hh_id<-data.frame(seq(1:600501))
names(hh_id) <- c("hh_id")
index_table_max_device <- cbind(hh_id,raw_data_max_device[,1])
train_data_max_device <-cbind(hh_id,raw_data_max_device[,-c(1)])

train_data_device <- train_data_max_device[,c(1,22,23,24,25,26)]



mean(train_data_device$device2) + 2*sd(train_data_device$device2)

x<- train_data_device[,c(1,3,6)]
#x<-x[(!x$device1 %in% 0),]
y<-x[(x$device2 %in% c(0:10)),]
hh_size<-y$hh_size
#y1<-cbind(scale(y$device1),hh_size)
y <- x[(!x$device2 %in% boxplot.stats(x$device2)$out),]
#y$hh_size <- recode(y$hh_size,"1='1';2='1';3='2';4 ='2';5='2';else='3'")
#y1<-as.data.frame(y1)
y$hh_size<-as.factor(y$hh_size)

#y$hh_size <- recode(y$hh_size,"1='1';2='2';3='3';4 ='4';5='5';else='5+'")


qplot(device2,
      data=y,
      geom='density',
      fill=hh_size,
      alpha=I(0.6),
      xlim=c(0,10))




ggplot(y,aes(hh_size,device2)) +
  geom_boxplot(aes(color = hh_size)) +
  coord_flip() +
  ggtitle("device2 per Household Size Boxplot")+
  ylab("number of device2")+
  xlab("Househould Size") +
  theme(legend.position="None") 

train_max <- train_data_max_device[1:12000,c(1,23,26)]
train_max <- train_max[(train_max$device2 %in% c(1:10)),]
train_max$hh_size1 <- recode(train_max$hh_size,"1='1';2='1';3='2';4 ='2';else='3'")

#####
hh_size <- train_max[,2]
temp2 <- cbind(scale(train_max[,1]),hh_size)
temp2<-as.data.frame(temp2)
train_max$hh_size <- as.factor(train_max$hh_size)

max.rf <- randomForest(hh_size ~ ., data=train_max, importance=TRUE,
                       proximity=TRUE)
print(max.rf)
####
kms <- kmeans(train_max[(train_max$hh_size1 == 1),],centers = 2, nstart = 8000)
hh_cluster<-data.frame(kms$cluster)
names(hh_cluster)<- c("hh_cluster")
whole<-cbind(train_max[(train_max$hh_size1 == 1),],hh_cluster)

table(whole$hh_size == whole$hh_cluster)
######
kms <- kmeans(train_max[(train_max$hh_size1 == 2),],centers = 3, nstart = 8000)
hh_cluster<-data.frame(kms$cluster)
names(hh_cluster)<- c("hh_cluster")
whole<-cbind(train_max[(train_max$hh_size1 == 2),],hh_cluster)
whole$hh_cluster1 <- recode(whole$hh_cluster,"1='4';2='3';else='5'")
table(whole$hh_cluster1==whole$hh_size)

######
kms <- kmeans(train_max[(train_max$hh_size1 != 1),],centers = 3, nstart = 8000)
hh_cluster<-data.frame(kms$cluster)
names(hh_cluster)<- c("hh_cluster")
whole<-cbind(train_max[(train_max$hh_size1 != 1),],hh_cluster)
table(whole$hh_size)
table(whole$hh_cluster)

whole$hh_cluster1 <- recode(whole$hh_cluster,"3='3';2='4';else='5'")
whole$hh_size2<-recode(whole$hh_size,"3='3';4='4';else = '5'")
table(whole$hh_cluster1==whole$hh_size2)



x1 <- train_data_device$device1
boxplot(x1)
boxplot(x1,outline = F)

y1<- x1[(!x1 %in% boxplot.stats(x1)$out)]
boxplot(y1)
