library(dplyr)
library(data.table)
#require(RPostgreSQL) 
#install.packages("RPostgreSQL")
library(RPostgreSQL)
library(ggplot2)
gp_connect = src_postgres(dbname = "gp_adw",
                          host = "csia4gpm01",
                          port = 5432,
                          user = "cpeng",
                          password = "changeme"
) 
options(digits = 22)
data.table.cp_session_month <- data.table(collect(tbl(gp_connect,sql("select * from cp_data_session_sample1 ")))) 
train_data <- as.data.frame(data.table.cp_session_month)
train_data <- train_data[,1:3]

## total count
data_plot = train_data %>%
  group_by(hh_size) %>%
  summarise(count = n()) %>%
  transform(hh_size = reorder(hh_size,-count))

ggplot(data_plot) + 
  geom_bar(aes(x=hh_size, y=count, 
               color = hh_size, fill = hh_size),
           stat="identity")
  #coord_flip()+



train_data$date  <- as.factor(format(train_data$recorded, "%Y%m%d"))
train_data$hour <- as.factor(format(train_data$recorded, "%H"))

# normalization
train_data$session <- scale(train_data$session_id,scale = F)

#plot
data_plot = train_data %>%
  group_by(date,hour,hh_size) %>%
  summarise(count = n()) 
data_plot$hh_size <- as.factor(data_plot$hh_size)

#data_plot$date <- as.factor(data_plot$date)

ggplot(data = data_plot,aes(x=hour,y=count,fill = hh_size)) + 
  geom_boxplot() +
facet_wrap(~hh_size,ncol = 5)+
  theme(legend.position="None",
        axis.text.x = element_text(angle = 90, hjust = 1))

train_data$hh_size <- as.factor(train_data$hh_size)
qplot(date,
      data=train_data,
      geom='density',
      fill=hh_size,
      alpha=I(0.6))

qplot(hour,
      data=train_data,
      geom='density',
      fill=hh_size,
      alpha=I(0.6))

qplot(session,
      data=train_data,
      geom='density',
      fill=hh_size,
      alpha=I(0.6),
      xlim = c(-20,0))


