library(tidyr)
library(dplyr)
library(data.table)
#require(RPostgreSQL) 
#install.packages("RPostgreSQL")
library(RPostgreSQL)
library(msos)
library(ggplot2)
library(car)
library(rgl)
library(FactoMineR)
## cp_session_time61 2
## cp_session_time7 3
gp_connect = src_postgres(dbname = "gp_adw",
                          host = "csia4gpm01",
                          port = 5432,
                          user = "cpeng",
                          password = "changeme"
) 
options(digits = 22)

data.table <- data.table(collect(tbl(gp_connect,sql("select * from cp_session_time61")))) 
raw_data <- as.data.frame(data.table)
dbDisconnect(gp_connect$con)
train_data <- raw_data[,c(-1,-2)]

train_data$count <- 1
train_data <- train_data %>% spread(browsertype,count)
train_data[is.na(train_data)] <- 0

train_data$date  <- as.numeric(format(train_data$session_start, "%d"))
train_data$hour <- as.numeric(format(raw_data$session_start, "%H"))
train <- train_data[,c(-1,-2)]

y <- train[,c(2,3)]

res.ca <- CA(y, graph = FALSE)
print(res.ca)
summary(res.ca, nb.dec = 2, ncp = 2)
comp <- data.frame(res.ca$row$coord[,1:3])


plot3d(comp$Dim.1,comp$Dim.2,comp$Dim.3)