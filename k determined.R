n = 100
g = 6 
set.seed(g)
d <- data.frame(x = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))), 
                y = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))))
plot(d)

library(cluster)
library(fpc)

d<-comp
#d <- train

pamk.best <- pamk(d)
cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
p <- pam(d, pamk.best$nc)
table(p$clustering)
#plot(p)


library(fpc)
asw <- numeric(20)
for (k in 2:20)
  asw[[k]] <- pam(d, k) $ silinfo $ avg.width
k.best <- which.max(asw)
cat("silhouette-optimal number of clusters:", k.best, "\n")
# still 4

## gap statistics

clusGap(d, kmeans, 10, B = 100, verbose = interactive())



## AIC 
m<- NULL
n<-NULL
k<-NULL
D<-NULL
kmeansAIC = function(fit){
  
  m = ncol(fit$centers)
  n = length(fit$cluster)
  k = nrow(fit$centers)
  D = fit$tot.withinss
  return(D + 2*m*k)
}
w<-list()
for (i in 1:100)
{
  fit <- kmeans(x = d,centers = i)
  w[[i]]<-kmeansAIC(fit)
}
#fit <- kmeans(x = d,centers = 5)
#kmeansAIC(fit)


## BIC
m<- NULL
n<-NULL
k<-NULL
D<-NULL
kmeansAIC = function(fit){
  
  m = ncol(fit$centers)
  n = length(fit$cluster)
  k = nrow(fit$centers)
  D = fit$tot.withinss
  return(D + log(n)*m*k)
}
w<-list()
for (i in 1:100)
{
  fit <- kmeans(x = d,centers = i)
  w[[i]]<-kmeansAIC(fit)
}
