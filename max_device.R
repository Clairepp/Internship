library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(randomForest)
library(data.table)
library(car)
# setwd("C:/Users/cpeng/Documents/Pro/household/dataset")

# Import the dataset
raw_data_max_device <- read_tsv("jpc_hh_demos_max_device_counts.txt",col_name = F)
names(raw_data_max_device) <- c("hh_id","demo1","demo2","demo3","demo4","demo5","demo6","demo7","demo8","demo9","demo10","demo11","demo12","demo13","demo14","demo15","demo16","demo17","demo18","demo19","demo20","device1", "device2", "device3", "device4","hh_size")
hh_id<-data.frame(seq(1:600501))
names(hh_id) <- c("hh_id")
index_table_max_device <- cbind(hh_id,raw_data_max_device[,1])
train_data_max_device <-cbind(hh_id,raw_data_max_device[,-c(1)])


# plot
# par(mfrow=c(1,1))
plot(train_data_max_device$demo1)
plot(train_data_max_device$device1)

  

# Category vs count
data_plot = train_data_max_device %>%
  group_by(hh_size) %>%
  summarise(count = n()) %>%
  transform(hh_size = reorder(hh_size,-count))

plot0 <- ggplot(data_plot) + 
  geom_bar(aes(x=hh_size, y=count, 
               color = hh_size, fill = hh_size),
           stat="identity")+
  theme(legend.position="None")+
  ggtitle("Number of Households in Individual Household")+
  ylab("Number of Household")+
  xlab("Category of Household")
ggsave(plot0, file="People_Household_by_Category.png", width=12.8, height=8)


temp <- NULL
somePDFPath = "max_device.pdf"
pdf(file=somePDFPath,width=12.8, height=8) 
par(mfrow=c(5,4))
for (i in 1:50)
{
  temp_max <- train_data_max_device[i,]
  temp1_max<-gather(temp_max,device,measurement,device1:device4)
  plot(temp1_max$measurement, xlab = "device",ylab = "number",ylim = c(1,13),type = "b",xaxt="n")
  axis(side = 1,at=c(1,2,3,4))
  # ggsave(plot1, file="plot_50.png")
}
dev.off()

temp <- NULL
somePDFPath = "maxdevice.pdf"

pdf(file=somePDFPath,width=12.8, height=8) 
for (j in 1:9)
{
  train_data_max_adj1 <- train_data_max_device[train_data_max_device$hh_size %in% c(j),]
  par(mfrow=c(5,4))
  for (i in 1:20)
  {
    temp_max <- train_data_max_adj1[i,]
    temp1_max<-gather(temp_max,device,measurement,device1:device4)
    plot(temp1_max$measurement, xlab = "device",ylab = "number",ylim = c(1,15),type = "b",xaxt="n"  )
    axis(side = 1,at=c(1,2,3,4))
  }}
dev.off()

#subplot
train_data_max_device_adj1 <- train_data_max_device[train_data_max_device$hh_size %in% c(1,2,3),]
temp <- NULL
somePDFPath = "max_device123.pdf"
pdf(file=somePDFPath,width=12.8, height=8) 
par(mfrow=c(5,2))
for (i in 1:50)
{
  temp_max <- train_data_max_device_adj1[i,]
  temp1_max<-gather(temp_max,device,measurement,device1:device4)
  plot(temp1_max$measurement,type = "l", xlab = "device",ylab = "number",ylim = c(1,15) )
}
dev.off()

train_data_max_device_adj2 <- train_data_max_device[train_data_max_device$hh_size %in% c(1),]
temp <- NULL
somePDFPath = "max_device1.pdf"
pdf(file=somePDFPath,width=12.8, height=8) 
par(mfrow=c(5,2))
for (i in 1:50)
{
  temp_max <- train_data_max_device_adj2[i,]
  temp1_max<-gather(temp_max,device,measurement,device1:device4)
  plot(temp1_max$measurement,type = "l", xlab = "device",ylab = "number",ylim = c(1,15) )
}
dev.off()

## random forest
 train_max <- train_data_max_device[1:12000,c(23,26)]
 train_max$hh_size <- recode(train_max$hh_size,"1='1';2='1';3='2';4 ='2';5='2';else='3'")

hh_size <- train_max[,2]
temp2 <- cbind(scale(train_max[,1]),hh_size)
temp2<-as.data.frame(temp2)
train_max$hh_size <- as.factor(train_max$hh_size)

max.rf <- randomForest(hh_size ~ ., data=train_max, importance=TRUE,
                      proximity=TRUE)
print(max.rf)

## cluster
train_data_max_device_cluster<-train_data_max_device[1:6000,c(-1,-26)]
kms <- kmeans(train_data_max_device_cluster,centers = 3, nstart = 8000)


hh_cluster<-data.frame(kms$cluster)
names(hh_cluster)<- c("hh_cluster")
train_data_max_device_whole<-cbind(train_data_max_device[1:6000,],hh_cluster)

## group 1
train_data_max_device_group1 <- train_data_max_device_whole[train_data_max_device_whole$hh_cluster %in% 1,]
train_max_device_group1 <- train_data_max_device_group1[,c(-1,-27)]
train_max_device_group1$hh_size <- as.factor(train_max_device_group1$hh_size)
max_device_group1.rf <- randomForest(hh_size ~ ., data=train_max_device_group1, importance=TRUE,
                              proximity=TRUE)
print(max_device_group1.rf)

## group 2
train_data_max_device_group2 <- train_data_max_device_whole[train_data_max_device_whole$hh_cluster %in% 2,]
train_max_device_group2 <- train_data_max_device_group2[,c(-1,-23)]
train_max_device_group2$hh_size <- as.factor(train_max_device_group2$hh_size)
max_device_group2.rf <- randomForest(hh_size ~ ., data=train_max_device_group2, importance=TRUE,
                              proximity=TRUE)
print(max_device_group2.rf)

## group 3
train_data_max_device_group3 <- train_data_max_device_whole[train_data_max_device_whole$hh_cluster %in% 3,]
train_max_device_group3 <- train_data_max_device_group3[,c(-1,-23)]
train_max_device_group3$hh_size <- as.factor(train_max_device_group3$hh_size)
max_device_group3.rf <- randomForest(hh_size ~ ., data=train_max_device_group3, importance=TRUE,
                              proximity=TRUE)
print(max_device_group3.rf)

## group 4
train_data_max_device_group4 <- train_data_max_device_whole[train_data_max_device_whole$hh_cluster %in% 3,]
train_max_device_group4 <- train_data_max_device_group4[,c(-1,-23)]
train_max_device_group4$hh_size <- as.factor(train_max_device_group4$hh_size)
max_device_group4.rf <- randomForest(hh_size ~ ., data=train_max_device_group4, importance=TRUE,
                              proximity=TRUE)
print(max_device_group4.rf)
