library(tidyr)
library(dplyr)
library(data.table)
library(RPostgreSQL)
library(msos)
library(ggplot2)
library(car)
library(rgl)
library(FactoMineR)
library(cluster)
library(fpc)
#write.table(mylist,"mylist.txt")
#setwd("//csiadsta01/Data_Dump/cpeng/Household")

###  first step  ###
options(digits = 22)
gp_connect = src_postgres(dbname = "gp_adw",
                          host = "csia4gpm01",
                          port = 5432,
                          user = "cpeng",
                          password = "changeme"
) 
data.table.cp_session_month <- data.table(collect(tbl(gp_connect,sql("select * from cp_session_web_3 ")))) 
raw_data <- as.data.frame(data.table.cp_session_month)
dbDisconnect(gp_connect$con)

train_data <- raw_data %>% spread(web_id,count)
train_data[is.na(train_data)] <- 0
train <- train_data[,-1]

## pca
y<- train
y.sca<-scale(y)
y.pr <- princomp(y.sca,cor = T)
pca_summary <- summary(y.pr)

vars=pca_summary$sdev^2
vars <- vars/sum(vars)
pca.cumprop=as.data.frame(cbind('Std_dev' = pca_summary$sdev, 'Prop_Variance' = vars, 'Cum_Prop' = cumsum(vars)))

## Take the number of components that account for at least 90% of the total variance
num.cols=length(rownames(pca.cumprop[pca.cumprop$Cum_Prop<=.8,])) + 1

screeplot(y.pr, type = "lines")
load <- loadings(y.pr)
plot(load)
comp <- data.frame(y.pr$scores[,1:num.cols])


set.seed(1)
gskmn<-clusGap(comp, fanny, 10, B = 300, verbose = interactive())
gskmn<-clusGap(y.sca, fanny, 10, B = 300, verbose = interactive())
plot(gskmn, main = "clusGap(., FUN = fuzzy statistics, n.start=10, B= 2000)")
gskmn

f<-fanny(comp,3)
table(f$clustering)
plot(f)

f<- fanny(y.sca,4)
table(f$clustering)

plot(f)

#plot
plot3d(comp$Comp.1,comp$Comp.2,comp$Comp.3)
pairs(comp)

## Clustering
## k-means
set.seed(100)
kms <- vector('list',10)
number <- vector('list',10)
for(K in 2:10) {
  kms[[K]] <- kmeans(comp,centers=K,nstart=10)
  number[[K]] <- table(kms[[K]]$cluster)
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
number
par(mfrow=c(1,1))
plot(2:10,sil.ave,type='l',xlab='K',ylab='Average silhouette width')









## hiearchical 
clusters <- hclust(dist(comp))
plot(clusters)

kms <- kmeans(comp,centers=2,nstart=10)
kms$cluster
which(kms$cluster ==1)
a<-data.frame(table(raw_data$session_id))

### second part ###

########################################################################
#######################################################################
options(digits = 22)
gp_connect = src_postgres(dbname = "gp_adw",
                          host = "csia4gpm01",
                          port = 5432,
                          user = "cpeng",
                          password = "changeme"
) 
data.table.cp_session_month <- data.table(collect(tbl(gp_connect,sql("select * from cp_session_web ")))) 
raw_data <- as.data.frame(data.table.cp_session_month)
mylist <- read.csv("//csiadsta01/Data_Dump/cpeng/Household/mylist.txt", sep="")
#mylist <- data.table(collect(tbl(gp_connect,sql("select * from cp_household_100 ")))) 
dbDisconnect(gp_connect$con)

ip_address <- unique(raw_data$ip_address)
##########################################################################

####
mydata <- filter(raw_data,ip_address == ip_address[7])
detail <- filter(mylist, ip_address == ip_address[7])
####

train_data <- mydata %>% spread(web_id,count)
train_data[is.na(train_data)] <- 0
train <- train_data[,c(-1,-2,-3)]
y<- train
y.sca<-scale(y)
y.pr <- princomp(y.sca,cor = T)
pca_summary <- summary(y.pr)

vars=pca_summary$sdev^2
vars <- vars/sum(vars)
pca.cumprop=as.data.frame(cbind('Std_dev' = pca_summary$sdev, 'Prop_Variance' = vars, 'Cum_Prop' = cumsum(vars)))

## Take the number of components that account for at least 90% of the total variance
num.cols=length(rownames(pca.cumprop[pca.cumprop$Cum_Prop<=.8,])) + 1
load <- loadings(y.pr)
comp <- data.frame(y.pr$scores[,1:num.cols])


set.seed(100)
kms <- vector('list',10)
number <- vector('list',10)
for(K in 2:10) {
  kms[[K]] <- kmeans(comp,centers=K,nstart=10)
  number[[K]] <- table(kms[[K]]$cluster)
}
sil.ave <- NULL # To collect silhouette's means for each K
#par(mfrow=c(3,3))
for(K in 2:10) {
  sil <- silhouette.km(comp,kms[[K]]$centers)
  sil.ave <- c(sil.ave,mean(sil))
  ssil <- sort_silhouette(sil,kms[[K]]$cluster)
  #plot(ssil,type='h',xlab='Observations',ylab='Silhouettes')
  #title(paste('K =',K))
}

#par(mfrow=c(1,1))


#result

print (num.cols)
print (pca.cumprop$Cum_Prop[num.cols])
print (sil.ave)
print (number)
plot(2:10,sil.ave,type='l',xlab='K',ylab='Average silhouette width')

#plot
plot3d(comp$Comp.1,comp$Comp.2,comp$Comp.3)
pairs(comp)


clusGap(comp, kmeans, 10, B = 1000, verbose = interactive())


pamk.best <- pamk(comp)
cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
p <- pam(comp, pamk.best$nc)
table(p$clustering)
