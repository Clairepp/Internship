library(data.table)
library(readr)
library(ggplot2)
library(dplyr)
library(msos)
# library(integer64)
# setwd("C:/Users/cpeng/Documents/Pro/household/dataset")
options(digits = 22)
raw_data <- read_tsv("cp_session_individual_household.txt",col_names = F)
train_data<- raw_data[,c(3:4)]
names(train_data)<- c("session_id","recorded")
train_data$session <- scale(train_data$session_id)
plot(train_data$session,type = "b")
train_data$date  <- as.factor(format(train_data$recorded, "%Y%m%d"))
train_data$hour <- as.factor(format(train_data$recorded, "%H"))

data_plot = train_data %>%
  group_by(date,hour) %>%
  summarise(count = n()) 

#data_plot$date <- as.factor(data_plot$date)

ggplot(data = data_plot,aes(x=date,y=count,fill = date)) + 
  geom_boxplot()



qplot(session,
      data=train_data,
      geom='density',
      fill=date,
      alpha=I(0.6))
