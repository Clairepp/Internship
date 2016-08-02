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



ip <- NULL
mydata <- NULL
num.pc <- NULL
detail.result <- NULL
num.proportion <- NULL
result.comp <- NULL
result.y <- NULL
num.observation<-NULL
num.feature<-NULL
comp.k <- NULL
comp.cluster <- NULL
y.k <- NULL
y.cluster <- NULL

## test
for (i in 1:100)
{
  ip <- ip_address[i]
  detail <- filter(mylist, ip_address == ip)
  mydata <- subset(raw_data,ip_address == ip)
  train_data <- mydata %>% spread(category,count)
  train_data[is.na(train_data)] <- 0
  train <- train_data[,c(-1,-2,-3)]
  y<- train
  y.sca<-scale(y)
  try(y.pr <- princomp(y.sca,cor = T))
  pca_summary <- summary(y.pr)
  print(i)
}


ip_address <- ip_address[-90]

for (i in 1:2)
{
  ip <- ip_address[i]
  detail <- filter(mylist, ip_address == ip)
  mydata <- subset(raw_data,ip_address == ip)
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
  
  ## Take the number of components that account for at least 80% of the total variance
  num.cols=length(rownames(pca.cumprop[pca.cumprop$Cum_Prop<=.8,])) + 1
  load <- loadings(y.pr)
  comp <- data.frame(y.pr$scores[,1:num.cols])
  train.row<- nrow(train)
  train.col<- ncol(train)
  
  gskmn.comp<-clusGap(comp, fanny, 10, B = 100, verbose = interactive())
  gskmn.y<-clusGap(y.sca, fanny, 10, B = 100, verbose = interactive())
  
  comp.A <- gskmn.comp$Tab[,3]
  comp.B <- gskmn.comp$Tab[,3]-gskmn.comp$Tab[,4]
  j = 1
  while(j<10){j=j+1;if(comp.A[j]>=comp.B[j+1]) break;comp.k= (j+1)}
  
  y.A <- gskmn.y$Tab[,3]
  y.B <- gskmn.y$Tab[,3]-gskmn.y$Tab[,4]
  j = 1
  while(j<10){j=j+1;if(y.A[j]>=y.B[j+1]) break;y.k= (j+1)}
  
  detail.result <- rbind(detail.result,detail)
  num.observation <- rbind(num.observation,train.row)
  num.feature <- rbind(num.feature,train.col)
  num.pc = rbind(num.pc,num.cols)
  num.proportion = rbind( num.proportion,pca.cumprop$Cum_Prop[num.cols])
  comp.cluster = rbind(comp.cluster,comp.k)
  y.cluster = rbind(y.cluster,y.k)

}
mydetail <- data.frame(detail.result[c(-4)],num.observation,num.feature,num.pc,num.proportion,comp.cluster,y.cluster)


