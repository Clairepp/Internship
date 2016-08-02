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
data.table.cp_session_month <- data.table(collect(tbl(gp_connect,sql("select * from cp_data_session_sample ")))) 
train_data <- data.table.cp_session_month[,c()]

##################################################################################
library(dplyr)
library(data.table)
#require(RPostgreSQL) 
#install.packages("RPostgreSQL")
library(RPostgreSQL)
gp_connect = src_postgres(dbname = "gp_adw",
                          host = "csia4gpm01",
                          port = 5432,
                          user = "cpeng",
                          password = "changeme"
) 
options(digits = 22)
data.table.cp_session_month <- data.table(collect(tbl(gp_connect,sql("select * from cp_individual_session2 ")))) 
raw_data <- as.data.frame(data.table.cp_session_month)

train_data<- raw_data[,c(3:4)]
names(train_data)<- c("session_id","recorded")
train_data$session <- scale(train_data$session_id)
plot(train_data$session,type = "b")

###################################################
## main dataset
#session_3 <- read.csv("~/Pro/household/session_3")
#raw_data <- as.data.frame(session_3)
options(digits = 22)
data.table.cp_session_month <- data.table(collect(tbl(gp_connect,sql("select * from cp_individual_session3_web ")))) 
raw_data <- as.data.frame(data.table.cp_session_month)

train_data<- raw_data[,c(-1,-2,-3)]

#train_data$session <- scale(train_data$session_id)
#plot(train_data$session,type = "b")

#write.csv(raw_data,"session_3")
train_data$date  <- as.factor(format(train_data$recorded, "%d"))
train_data$hour <- as.factor(format(train_data$recorded, "%H"))

train<-train_data[,c(3,5,6)]
train$web_id<-as.factor(train$web_id)
#train$date<-as.factor(train$date)
train$hour<-as.factor(train$hour)

table(train$hour)
train$hour<-recode(train$hour,"'00'='1';'01'='1';'02'=1;'03'='2';'04'='2';'05'='2';'06'='2';'07'='2';'08'='3';'09'='3';'10'='3';'11'='3';'12'='3';'13'='4';'14'='4';'15'='4';'16'='4';'17'='4';'18'='5';'19'='5';'20'='5';'21'='5';'22'='5';'23'='1'")

### dummy
#date.f <- factor(train$date)
#dummies.date <- model.matrix(~date.f)
#dummies.date<-data.frame(dummies.date)

hour.f <- factor(train$hour)
dummies.hour <- model.matrix(~hour.f)
dummies.hour<-data.frame(dummies.hour)

web_id.f <- factor(train$web_id)
dummies.web_id <- model.matrix(~web_id.f)
dummies.web_id<-data.frame(dummies.web_id)


##data_train <- cbind(train[,2],dummies.web_id)
data_train <- cbind(dummies.web_id,dummies.hour)
data_train <- cbind(data_train,as.numeric(train$date))
##train <- scale(train,center = F)

kms <- vector('list',10)
for(K in 2:10) {
  kms[[K]] <- kmeans(data_train,centers=K,nstart=10)
}
sil.ave <- NULL # To collect silhouette's means for each K
par(mfrow=c(3,3))
for(K in 2:10) {
  sil <- silhouette.km(data_train,kms[[K]]$centers)
  sil.ave <- c(sil.ave,mean(sil))
  ssil <- sort_silhouette(sil,kms[[K]]$cluster)
  plot(ssil,type='h',xlab='Observations',ylab='Silhouettes')
  title(paste('K =',K))
}
sil.ave
par(mfrow=c(1,1))
plot(2:10,sil.ave,type='l',xlab='K',ylab='Average silhouette width')



################################################
options(digits = 22)
data.table.cp_session_month <- data.table(collect(tbl(gp_connect,sql("select * from cp_individual_session3_web ")))) 
raw_data <- as.data.frame(data.table.cp_session_month)

web_name <- data.frame(table(raw_data$web_name))
web_name <- arrange(web_name,Freq)

options(digits = 22)
data.table.cp_session_month <- data.table(collect(tbl(gp_connect,sql("select * from cp_individual_session32_web ")))) 
raw_data1 <- as.data.frame(data.table.cp_session_month)
web_name1 <- data.frame(table(raw_data1$web_name))
web_name1<-arrange(web_name1,Freq)


options(digits = 22)
data.table.cp_session_month <- data.table(collect(tbl(gp_connect,sql("select * from cp_individual_session2_web ")))) 
raw_data2 <- as.data.frame(data.table.cp_session_month)
web_name2 <- data.frame(table(raw_data2$web_name))
web_name2<-arrange(web_name2,Freq)


########################################
#k = 8

kms_8 <- kmeans(data_train,centers=4,nstart=10)
prediction <- as.data.frame(kms_8$cluster)
names(prediction) <- c("prediction")
prediction.data <- cbind(train,prediction)
predict.1 <- filter(prediction.data,prediction ==1)
predict.2 <- filter(prediction.data,prediction ==2)
predict.3 <- filter(prediction.data,prediction ==3)

prediction.data$prediction <- as.factor(prediction.data$prediction)


data_plot = prediction.data %>%
  group_by(date,hour,web_id,prediction) %>%
  summarise(count = n()) 

ggplot(data = data_plot,aes(x=hour,y=count,fill = prediction)) + 
  geom_boxplot() +
  facet_wrap(~prediction,ncol = 5)+
  theme(legend.position="None",
        axis.text.x = element_text(angle = 90, hjust = 1))

qplot(hour,
      data=data_plot,
      #geom='density',
      fill=prediction,
      alpha=I(0.6))
qplot(date,
      data=data_plot,
      #geom='density',
      fill=prediction,
      alpha=I(0.6))
qplot(web_id,
      data=data_plot,
      geom='density',
      fill=prediction,
      alpha=I(0.6))
##############################################
## pca

y<- cbind(prediction,data_train)
y<-scale(y,scale=F)
eg<-eigen(var(y))
### prcomp()

ev<-eg$vectors
w<-y%*%ev 
r<-range(w)
plot(w[,1:2], cex = 1,xlab="PC1",xlim=r,ylim=r, ylab="PC2")





y<- cbind(prediction,data_train)
y.pr <- princomp(y)
summary(y.pr, loadings=TRUE)
predict(y.pr)
screeplot(y.pr, type = "lines")

load <- loadings(y.pr)
plot(y.pr)
biplot(y.pr, choices = 1:2, labs )
plot(load[,1:2]); text(load[,1], load[,2], adj = c(-0.4,0.3))




################################################
## feature selection
