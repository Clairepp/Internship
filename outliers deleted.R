## outliers delete
boxplot(raw_data1$count)
raw_data2<-filter(raw_data1,count > 19)
#outliers = boxplot(raw_data1$count, plot=FALSE)$out
#raw_data1<-raw_data1[raw_data1$count %in% outliers,]

train_data1 <- raw_data2 %>% spread(web_id,count)
train_data1[is.na(train_data1)] <- 0
train <- train_data1[,-1]

y<- train
y.sca<-apply(y, 2, function(x)(x/sum(x)))
y.sca<-t(apply(y, 1, function(x)(x/sum(x))))
y.pr <- princomp(y.sca)
summary(y.pr)
screeplot(y.pr, type = "lines")
load <- loadings(y.pr)
plot(load)
comp <- data.frame(y.pr$scores[,1:2])


#pc <- prcomp(y,scale = T)
#summary(pc)
#biplot(pc)
#comp <- data.frame(pc$x[,1:4])
#plot(comp, pch=16, col=rgb(0,0,0,0.5))


# Multi 3D plot
plot3d(comp$Comp.1, comp$Comp.2,comp$Comp.3)
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
