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


#########
## histogram 
# Category vs. Household Size
data_plot_hh_size = train_data_max_device %>%
  group_by(hh_size) %>%
  summarise(count = n()) %>%
  transform(hh_size = reorder(hh_size,-count))


plot0 <- ggplot(data_plot_hh_size) + 
  geom_bar(aes(x=hh_size, y=count, 
               color = hh_size, fill = hh_size),
           stat="identity")+
  theme(legend.position="None")+
  ggtitle("Number of Households in Individual Household")+
  ylab("Number of Household")+
  xlab("Category of Household")
ggsave(plot0, file="C:/Users/cpeng/Documents/Pro/household/pic/People_Household_by_Category.png", width=12.8, height=8)

#### Demography vs. Household Size

data_plot_demo1 = train_data_max_device %>%
  group_by(demo1) %>%
  summarise(count = n()) %>%
  transform(demo1 = reorder(demo1,-count))

data_plot_demo1$demo1<-as.character(data_plot_demo1$demo1)
data_plot_demo1$demo1<-as.numeric(data_plot_demo1$demo1)
p<-ggplot(data_plot_demo1,aes(factor(count),demo1))
p+geom_boxplot()

plot1 <- ggplot(data_plot_demo1,aes) + 
  geom_boxplot(aes(x=demo1, y=count, 
               color = demo1,fill = demo1),
           stat="boxplot")+
  scale_x_continuous(breaks=seq(0,1,0.02)) +
  scale_y_continuous(breaks=seq(0,30000,1500)) +
  coord_flip() +
  theme(legend.position="None")+
  ggtitle("Number of Households in Probability of demo1")+
  ylab("Number of Household")+
  xlab("Probability of Demo1")



plot1 <- ggplot(data_plot_demo1) + 
  geom_bar(aes(x=demo1, y=count, 
               color = demo1,fill = demo1),
           stat="identity")+
  scale_x_continuous(breaks=seq(0,1,0.02)) +
  scale_y_continuous(breaks=seq(0,30000,1500)) +
  coord_flip() +
  theme(legend.position="None")+
  ggtitle("Number of Households in Probability of demo1")+
  ylab("Number of Household")+
  xlab("Probability of Demo1")
ggsave(plot1, file="C:/Users/cpeng/Documents/Pro/household/pic/Household_by_Demo1_Probability.png", width=12.8, height=8)

# group 1
data_plot_demo1_1 <- slice(data_plot_demo1,1:1812)
data_plot_demo1_1$demo1<-as.character(data_plot_demo1_1$demo1)
data_plot_demo1_1$demo1<-as.numeric(data_plot_demo1_1$demo1)

plot11 <- ggplot(data_plot_demo1_1) + 
  geom_bar(aes(x=demo1, y=count, 
               color = demo1, fill = demo1),
           stat="identity")+
  scale_y_continuous(breaks=seq(0,3000,150)) +
  scale_x_continuous(breaks=seq(0,1,0.001)) +
  coord_flip() +
  theme(legend.position="None")+
  ggtitle("Number of Households in Probability of demo1")+
  ylab("Number of Household")+
  xlab("Probability of Demo1")
ggsave(plot11, file="C:/Users/cpeng/Documents/Pro/household/pic/Household_by_Demo1_Probability_1.png", width=12.8, height=8)

# group 2
data_plot_demo1_2 <- slice(data_plot_demo1,1813:3624)
data_plot_demo1_2$demo1<-as.character(data_plot_demo1_2$demo1)
data_plot_demo1_2$demo1<-as.numeric(data_plot_demo1_2$demo1)

plot12 <- ggplot(data_plot_demo1_2) + 
  geom_bar(aes(x=demo1, y=count, 
               color = demo1, fill = demo1),
           stat="identity")+
  coord_flip() +
  scale_x_continuous(breaks=seq(0,1,0.001)) +
  scale_y_continuous(breaks=seq(0,8000,400)) +
  theme(legend.position="None")+
  ggtitle("Number of Households in Probability of demo1")+
  ylab("Number of Household")+
  xlab("Probability of Demo1")
ggsave(plot12, file="C:/Users/cpeng/Documents/Pro/household/pic/Household_by_Demo1_Probability_2.png", width=12.8, height=8)

# group 3
data_plot_demo1_3 <- slice(data_plot_demo1,3625:5436)
data_plot_demo1_3$demo1<-as.character(data_plot_demo1_3$demo1)
data_plot_demo1_3$demo1<-as.numeric(data_plot_demo1_3$demo1)

plot13 <- ggplot(data_plot_demo1_3) + 
  geom_bar(aes(x=demo1, y=count, 
               color = demo1, fill = demo1),
           stat="identity")+
  scale_x_continuous(breaks=seq(0,1,0.001)) +
  scale_y_continuous(breaks=seq(0,30000,1500)) +
  coord_flip() +
  theme(legend.position="None")+
  ggtitle("Number of Households in Probability of demo1")+
  ylab("Number of Household")+
  xlab("Probability of Demo1")
ggsave(plot13, file="C:/Users/cpeng/Documents/Pro/household/pic/Household_by_Demo1_Probability_3.png", width=12.8, height=8)

# group 4
data_plot_demo1_4 <- slice(data_plot_demo1,5437:6754)
data_plot_demo1_4$demo1<-as.character(data_plot_demo1_4$demo1)
data_plot_demo1_4$demo1<-as.numeric(data_plot_demo1_4$demo1)

plot14 <- ggplot(data_plot_demo1_4) + 
  geom_bar(aes(x=demo1, y=count, 
               color = demo1, fill = demo1),
           stat="identity")+
  scale_x_continuous(breaks=seq(0,1,0.002)) +
  scale_y_continuous(breaks=seq(0,8000,400)) +
  coord_flip() +
  theme(legend.position="None")+
  ggtitle("Number of Households in Probability of demo1")+
  ylab("Number of Household")+
  xlab("Probability of Demo1")
ggsave(plot14, file="C:/Users/cpeng/Documents/Pro/household/pic/Household_by_Demo1_Probability_4.png", width=12.8, height=8)

# group 4
data_plot_demo1_5 <- slice(data_plot_demo1,6755:7251)
data_plot_demo1_5$demo1<-as.character(data_plot_demo1_5$demo1)
data_plot_demo1_5$demo1<-as.numeric(data_plot_demo1_5$demo1)

plot15 <- ggplot(data_plot_demo1_5) + 
  geom_bar(aes(x=demo1, y=count, 
               color = demo1, fill = demo1),
           stat="identity")+
  scale_x_continuous(breaks=seq(0,1,0.02)) +
  scale_y_continuous(breaks=seq(0,500,25)) +
  coord_flip() +
  theme(legend.position="None")+
  ggtitle("Number of Households in Probability of demo1")+
  ylab("Number of Household")+
  xlab("Probability of Demo1")
ggsave(plot15, file="C:/Users/cpeng/Documents/Pro/household/pic/Household_by_Demo1_Probability_5.png", width=12.8, height=8)

# demo_adjustment
data_plot_demo14 = train_data_max_device %>%
  group_by(demo14) %>%
  summarise(count = n()) %>%
  transform(demo14 = reorder(demo14,-count))

data_plot_demo14$demo14<-as.character(data_plot_demo14$demo14)
data_plot_demo14$demo14<-as.numeric(data_plot_demo14$demo14)

plot1 <- ggplot(data_plot_demo14) + 
  geom_bar(aes(x=demo14, y=count, 
               color = demo14,fill = demo14),
           stat="identity")+
  scale_x_continuous(breaks=seq(0,1,0.02)) +

  coord_flip() +
  theme(legend.position="None")+
  ggtitle("Number of Households in Probability of demo14")+
  ylab("Number of Household")+
  xlab("Probability of demo14")
ggsave(plot1, file="C:/Users/cpeng/Documents/Pro/household/pic/Household_by_demo14_Probability.png", width=12.8, height=8)


#### Devices vs. Household Size

# device1
data_plot_device1 = train_data_max_device %>%
  group_by(device1) %>%
  summarise(count = n()) %>%
  transform(device1 = reorder(device1,-count))

data_plot_device1$device1<-as.character(data_plot_device1$device1)
data_plot_device1$device1<-as.numeric(data_plot_device1$device1)

plot2 <- ggplot(data_plot_device1) + 
  geom_bar(aes(x=device1, y=count, 
               color = device1,fill = device1),
           stat="identity")+
  coord_flip() +
  scale_y_continuous(breaks=seq(0,360000,18000)) +
  scale_x_continuous(breaks=seq(0,39,1)) +
  theme(legend.position="None")+
  ggtitle("Number of Households in number of device1")+
  ylab("Number of Household")+
  xlab("number of device1")
ggsave(plot2, file="C:/Users/cpeng/Documents/Pro/household/pic/Household_by_Device1_Probability.png", width=12.8, height=8)

# device2
data_plot_device2 = train_data_max_device %>%
  group_by(device2) %>%
  summarise(count = n()) %>%
  transform(device2 = reorder(device2,-count))

data_plot_device2$device2<-as.character(data_plot_device2$device2)
data_plot_device2$device2<-as.numeric(data_plot_device2$device2)

plot3 <- ggplot(data_plot_device2) + 
  geom_bar(aes(x=device2, y=count, 
               color = device2,fill = device2),
           stat="identity")+
  coord_flip() +
  scale_y_continuous(breaks=seq(0,240000,12000)) +
  scale_x_continuous(breaks=seq(0,250,5)) +
  theme(legend.position="None")+
  ggtitle("Number of Households in number of device2")+
  ylab("Number of Household")+
  xlab("number of device2")
ggsave(plot3, file="C:/Users/cpeng/Documents/Pro/household/pic/Household_by_device2_Probability.png", width=12.8, height=8)

# device3
data_plot_device3 = train_data_max_device %>%
  group_by(device3) %>%
  summarise(count = n()) %>%
  transform(device3 = reorder(device3,-count))

data_plot_device3$device3<-as.character(data_plot_device3$device3)
data_plot_device3$device3<-as.numeric(data_plot_device3$device3)

plot4 <- ggplot(data_plot_device3) + 
  geom_bar(aes(x=device3, y=count, 
               color = device3,fill = device3),
           stat="identity")+
  coord_flip() +
  scale_y_continuous(breaks=seq(0,380000,19000)) +
  scale_x_continuous(breaks=seq(0,33,1)) +
  theme(legend.position="None")+
  ggtitle("Number of Households in number of device3")+
  ylab("Number of Household")+
  xlab("number of device3")
ggsave(plot4, file="C:/Users/cpeng/Documents/Pro/household/pic/Household_by_device3_Probability.png", width=12.8, height=8)

# device4
data_plot_device4 = train_data_max_device %>%
  group_by(device4) %>%
  summarise(count = n()) %>%
  transform(device4 = reorder(device4,-count))

data_plot_device4$device4<-as.character(data_plot_device4$device4)
data_plot_device4$device4<-as.numeric(data_plot_device4$device4)

plot5 <- ggplot(data_plot_device4) + 
  geom_bar(aes(x=device4, y=count, 
               color = device4,fill = device4),
           stat="identity")+
  coord_flip() +
  scale_y_continuous(breaks=seq(0,540000,27000)) +
  scale_x_continuous(breaks=seq(0,21,1)) +
  theme(legend.position="None")+
  ggtitle("Number of Households in number of device4")+
  ylab("Number of Household")+
  xlab("number of device4")
ggsave(plot5, file="C:/Users/cpeng/Documents/Pro/household/pic/Household_by_device4_Probability.png", width=12.8, height=8)

###### 
## scatter plot
ggpairs(train_data_max_device[,22:26])

######
## Boxplot
LVboxplot(data_plot_demo1$demo1,xlab="Probability of Demo1")
LVboxplot(data_plot_demo20$demo20,xlab="Probability of demo20")
par(mfrow=c(2,2))
LVboxplot(data_plot_device1$device1,xlab="number of device1")
LVboxplot(data_plot_device3$device3,xlab="number of device3")
LVboxplot(data_plot_device3$device3,xlab="number of device3")
LVboxplot(data_plot_device4$device4,xlab="number of device4")

# by household
par(mfrow=c(1,1))
train_data_max_device$hh_size <- as.factor(train_data_max_device$hh_size)

# demo
p1<-ggplot(train_data_max_device,aes(hh_size,log(demo20))) +
    geom_boxplot(aes(color = hh_size)) +
    coord_flip() +
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  ggtitle("demo20 per Household Size Boxplot")+
  ylab("Probability Of demo20")+
  xlab("Househould Size") +
  theme(legend.position="None")
ggsave(p1, file="C:/Users/cpeng/Documents/Pro/household/pic/demo20_Boxplot_per_Household.png", width=12.8, height=8)
# device
p2<-ggplot(train_data_max_device,aes(hh_size,device3)) +
  geom_boxplot(aes(color = hh_size)) +
  coord_flip() +
  
  ggtitle("device3 per Household Size Boxplot")+
  ylab("number of device3")+
  xlab("Househould Size") +
  theme(legend.position="None") 
ggsave(p2, file="C:/Users/cpeng/Documents/Pro/household/pic/device3_Boxplot_per_Household.png", width=12.8, height=8)

# transform 
p1<-ggplot(train_data_max_device,aes(hh_size,sqrt(1-demo1))) +
  geom_boxplot(aes(color = hh_size)) +
  coord_flip() +
  ggtitle("demo1 per Household Size Boxplot")+
  ylab("Probability Of demo1")+
  xlab("Househould Size") +
  theme(legend.position="None")
ggsave(p1, file="C:/Users/cpeng/Documents/Pro/household/pic/demo1_Boxplot_per_Household_transformation.png", width=12.8, height=8)

temp <- cbind(train_data_max_device$demo1^2,train_data_max_device$demo4^2,train_data_max_device$hh_size)
temp <- as.data.frame(temp)
temp$V3 <- as.factor(temp$V3)

ggpairs(temp)

########
##density
train_data_max_device <-cbind(hh_id,raw_data_max_device[,-c(1)])
#temp <- filter(train_data_max_device,hh_size<5)
#temp$hh_size <- as.factor(temp$hh_size)
train_data_max_device$hh_size <- as.factor(train_data_max_device$hh_size)
train_data_max_device$hh_size <- recode(train_data_max_device$hh_size,"1='1';2='2';3='3';4 ='4';else='5+'")
qplot(device2,
               data=train_data_max_device,
               geom='density',
               fill=hh_size,
               alpha=I(0.6),
      xlim=c(0,10))





ggplot(temp,aes(demo1,color = hh_size)) +
  geom_density()
ggsave(p1, file="C:/Users/cpeng/Documents/Pro/household/pic/density_demo1_hhsize_sqrt(2).png", width=8, height=12.8)




dplt3 <- qplot(displ,
               data=mpg,
               geom='density',
               fill=class,
               alpha=I(0.6))
train <- data.table(train_data_max_device)
train[, .(n = .N, mean = mean(as.numeric(device3)), median = median(as.numeric(device3))), by='hh_size'][order(hh_size)]
