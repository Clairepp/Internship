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
#write.table(output,"PCA.txt")
#setwd("//csiadsta01/Data_Dump/cpeng/Household")
options(digits = 22)
gp_connect = src_postgres(dbname = "gp_adw",
                          host = "csia4gpm01",
                          port = 5432,
                          user = "cpeng",
                          password = "changeme"
) 
data.table.cp_session_month <- data.table(collect(tbl(gp_connect,sql("select * from cp_session_category_20")))) 
raw_data <- as.data.frame(data.table.cp_session_month)
dbDisconnect(gp_connect$con)

train_data <- raw_data %>% spread(category,count)
train_data[is.na(train_data)] <- 0
train <- train_data[,c(-1)]

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

#plot
plot3d(comp$Comp.1,comp$Comp.2,comp$Comp.3)
pairs(comp)

## Clustering
## k-means
set.seed(1)
kms <- vector('list',10)
number <- vector('list',10)
for(K in 2:10) {
  kms[[K]] <- kmeans(y.sca,centers=K,nstart=10)
  number[[K]] <- table(kms[[K]]$cluster)
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
number
par(mfrow=c(1,1))
plot(2:10,sil.ave,type='l',xlab='K',ylab='Average silhouette width')

set.seed(5)
gskmn.comp<-clusGap(comp, kmeans, 10, B = 100, verbose = interactive())
gskmn.comp

comp.A <- gskmn.comp$Tab[,3]
comp.B <- gskmn.comp$Tab[,3]-gskmn.comp$Tab[,4]
j = 0
#try(while(j<10) { j=j+1;if(comp.A[j]>=comp.B[j+1]) break; comp.k = j+1}
for(j in 1:9)
 {
   if (comp.A[j]<comp.B[j+1])
   {
     print (j)
     print  ("doens't work")
     comp.k = 10
     }
   else 
     {
     comp.k = j
     break
   }
 }
    

set.seed(100)
gskmn<-clusGap(y.sca, kmeans, 10, B = 100, verbose = interactive())
gskmn

gskmn<-clusGap(y, fanny, 10, B = 100, verbose = interactive())

plot(gskmn, main = "clusGap(., FUN = fuzzy statistics, n.start=10, B= 2000)")

result <- NULL

gskmn$Tab[,3]-gskmn$Tab[,4]

which(gskmn$Tab[,3]>=gskmn$Tab[,3]-gskmn$Tab[,4])
#kmeans, pam, clara, hclust,fanny
f<-fanny(comp,5)
plot(f)

f<- fanny(y.sca,8)
table(f$clustering)
plot(f)
##  
clusters <- hclust(dist(comp))
plot(clusters)

kms <- kmeans(comp,centers=5,nstart=10)
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
data.table.cp_session_month <- data.table(collect(tbl(gp_connect,sql("select * from cp_session_category ")))) 
raw_data <- as.data.frame(data.table.cp_session_month)
mylist <- read.csv("//csiadsta01/Data_Dump/cpeng/Household/mylist.txt", sep="")
#mylist <- data.table(collect(tbl(gp_connect,sql("select * from cp_household_100 ")))) 
dbDisconnect(gp_connect$con)

ip_address <- unique(raw_data$ip_address)
ip_address <- sort(ip_address)
##########################################################################

####
setorder(raw_data,ip_address)

ip <- ip_address[8]
mydata <- filter(raw_data, ip_address == ip)
detail <- filter(mylist, ip_address == ip)
####

train_data <- mydata %>% spread(category,count)
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
print (num.cols)
print (pca.cumprop$Cum_Prop[num.cols])

#############################################################################
set.seed(1)
gskmn<-clusGap(comp, kmeans, 10, B = 100, verbose = interactive())
gskmn

set.seed(1)
gskmn<-clusGap(y.sca, kmeans, 10, B = 100, verbose = interactive())
gskmn



set.seed(3)
gskmn<-clusGap(comp, fanny, 10, B = 300, verbose = interactive())
gskmn<-clusGap(y.sca, fanny, 10, B = 300, verbose = interactive())
plot(gskmn, main = "clusGap(., FUN = fanny, n.start=10, B= 2000)")
gskmn
###############################################################################


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


set.seed(1)
gskmn<-clusGap(comp, fanny, 10, B = 500, verbose = interactive())
#gskmn<-clusGap(y.sca, kmeans, 10, B = 2000, verbose = interactive())
plot(gskmn, main = "clusGap(., FUN = kmeans, n.start=20, B= 300)")
gskmn
## package cluster!!

pamk.best <- pamk(comp)
cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
p <- pam(comp, pamk.best$nc)
table(p$clustering)
