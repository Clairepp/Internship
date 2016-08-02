library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(car)
library(GGally)
library(lvplot)

# setwd("C:/Users/cpeng/Documents/Pro/household/dataset")

# Import the dataset
raw_data_max_device <- read_tsv("jpc_hh_demos_max_device_counts.txt",col_name = F)
names(raw_data_max_device) <- c("hh_id","demo1","demo2","demo3","demo4","demo5","demo6","demo7","demo8","demo9","demo10","demo11","demo12","demo13","demo14","demo15","demo16","demo17","demo18","demo19","demo20","device1", "device2", "device3", "device4","hh_size")
hh_id<-data.frame(seq(1:600501))
names(hh_id) <- c("hh_id")
index_table_max_device <- cbind(hh_id,raw_data_max_device[,1])
train_data_max_device <-cbind(hh_id,raw_data_max_device[,-c(1)])

train_data_device <- train_data_max_device[,c(1,22,23,24,25,26)]
