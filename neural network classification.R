library(neuralnet)

itrain <- iris[sample(1:150, 50),]

itrain$setosa <- c(itrain$Species == 'setosa')

itrain$versicolor <- c(itrain$Species == 'versicolor')

itrain$virginica <- c(itrain$Species == 'virginica')

itrain$Species <- NULL

inet <- neuralnet(setosa + versicolor + virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, itrain, hidden=3, lifesign="full")

plot(inet, rep="best")

plot(inet, rep="best", intercept=FALSE)





predict <- compute(inet, iris[1:4])

predict$net.result

result<-0

for (i in 1:150) { result[i] <- which.max(predict$net.result[i,]) }

for (i in 1:150) { if (result[i]==1) {result[i] = "setosa"} }

for (i in 1:150) { if (result[i]==2) {result[i] = "versicolor"} }

for (i in 1:150) { if (result[i]==3) {result[i] = "virginica"} }

comparison <- iris

comparison$Predicted <- result

comparison





raw_data_max_device <- read_tsv("jpc_hh_demos_max_device_counts.txt",col_name = F)
names(raw_data_max_device) <- c("hh_id","demo1","demo2","demo3","demo4","demo5","demo6","demo7","demo8","demo9","demo10","demo11","demo12","demo13","demo14","demo15","demo16","demo17","demo18","demo19","demo20","device1", "device2", "device3", "device4","hh_size")
hh_id<-data.frame(seq(1:600501))
names(hh_id) <- c("hh_id")
index_table_max_device <- cbind(hh_id,raw_data_max_device[,1])
train_data_max_device <-cbind(hh_id,raw_data_max_device[,-c(1)])

itrain <- train_data_max_device[1:2000,-1]

itrain$one <- c(itrain$hh_size == 1)
itrain$two <- c(itrain$hh_size == 2)
itrain$three <- c(itrain$hh_size == 3)
itrain$four <- c(itrain$hh_size == 4)
itrain$five <- c(itrain$hh_size == 5)
itrain$six <- c(itrain$hh_size == 6)
itrain$seven <- c(itrain$hh_size == 7)
itrain$eight <- c(itrain$hh_size == 8)
itrain$nine <- c(itrain$hh_size == 9)
itrain$hh_size <- NULL


data.scaled = scale(itrain)

inet <- neuralnet(one+two+three+four+five+six+seven+eight+nine ~ demo1+demo2+demo3+demo4+demo5+demo6+demo7+demo8+demo9+demo10+demo11+demo12+demo13+demo14+demo15+demo16+demo17+demo18+demo19+demo20+device1+device2+device3+device4, itrain, hidden=2, lifesign="full")
print(inet)
plot(inet, rep="best")
