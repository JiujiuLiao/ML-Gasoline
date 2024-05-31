##########################################################################################
#################reading dataset and merging price dataset and station dataset##############
##########################################################################################
setwd("C:/Users/jiuji/OneDrive/Desktop/study/Advanced method in data analysis/Applications in Data Analytics/Price dataset Feb.09th - Mar.07th")
prices = list.files(pattern="\\.csv$")
for (i in 1:length(prices)) assign(prices[i], read.csv(prices[i]))
prices_df = do.call(rbind, lapply(prices, function(x) read.csv(x, stringsAsFactors = FALSE)))

setwd("C:/Users/jiuji/OneDrive/Desktop/study/Advanced method in data analysis/Applications in Data Analytics/Station dataset Feb.09th - Mar.07th")
stations = list.files(pattern="\\.csv$")
for (i in 1:length(stations)) assign(stations[i], read.csv(stations[i]))
stations_df =data.frame(do.call(rbind, lapply(stations, function(x) read.csv(x, stringsAsFactors = FALSE))))

sum(is.na(prices_df))
sum(is.na(stations_df))
summary(prices_df)
summary(stations_df)
prices_df <- filter(prices_df , diesel > 0, e5 > 0, e10 > 0)

#write.csv(stations_df, file = "C:/Users/jiuji/OneDrive/Desktop/study/Advanced method in data analysis/Applications in Data Analytics//stations.csv" )

#names(prices_df)[2] ="uuid"
#df <- data.frame( merge(prices_df,stations_df, by= "uuid", all.x  = FALSE, all.y = FALSE, sort =TRUE))

#libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(DataExplorer)
library(tidyverse)
library(mlr3verse)
library(mlr3viz)
