setwd("C:/Users/jiuji/OneDrive/Desktop/study/Advanced method in data analysis/Applications in Data Analytics/dataset Apr.22-28")
setwd("C:/Users/jiuji/OneDrive/Desktop/study/Advanced method in data analysis/Applications in Data Analytics/dataset Apr.22-28/stations")
Sys.setenv(LANG="en")
#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("crayon")
#install.packages("mlr3")
#install.packages("mlr3verse")
#install.packages("GGally")
#libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(DataExplorer)
library(tidyverse)
library(mlr3verse)
library(mlr3viz)
#reading data
prices_25th <- read.csv("C:/Users/jiuji/OneDrive/Desktop/study/Advanced method in data analysis/Applications in Data Analytics/dataset Apr.22-28/2024-04-25-prices.csv")
stations_25th <- read.csv("C:/Users/jiuji/OneDrive/Desktop/study/Advanced method in data analysis/Applications in Data Analytics/dataset Apr.22-28/stations/2024-04-25-stations.csv")

#summary
#DataExplorer::create_report(df1)
summary(prices_25th)
head(prices_25th)
# count total missing values,after checking, there is no missing value in our data set
sum(is.na(prices_25th))
sum(is.na(stations_25th))

#visualise price of diesel，e5 and e10
ggplot(prices_25th, aes(x = date, y = diesel)) + 
  geom_point() 
#visualize price of e5
ggplot(prices_25th, aes(x = date, y = e5)) + 
  geom_point() 
#visualize price of e10 
ggplot(prices_25th, aes(x = date, y = e10)) + 
  geom_point() 

#boxplot is bettere, but so far i do not know how to do it, because in our data set there are three prices for different fuels
#ggplot(prices_25th, aes(x = cut, y = price)) +
 # + geom_boxplot()

#there are many prices are 0 which do not make sense, so next we decide to remove those rows have 0 price.
price_df <- filter(prices_25th, diesel > 0, e5 > 0, e10 > 0)

#rename station_uuid to uuid so that two datasets can be merged
names(price_df)[2] ="uuid"
df <- data.frame( merge(price_df,stations_25th, by= "uuid", all.x  = FALSE, all.y = FALSE, sort =TRUE))
#view how many brands dataset has
number_of_brands <- length(unique(df$brand))

#check how many brands #so after checking  we have 652 brands totally
ddstab <-data.frame(table(df$brand)) 
knitr::kable(ddstab)

#visualization of diesel,e5,e10 price distribution
d <- ggplot(df, aes(
  x = diesel))
#p + geom_freqpoly(bins = 30, boundary = 0.025)
d + geom_histogram(mapping = aes(y = stat(density))) +
  geom_density(color = "red", size = 1)

e5 <- ggplot(df, aes(
  x = e5))
e5 + geom_histogram(mapping = aes(y = stat(density))) +
  geom_density(color = "red", size = 1)

e10 <- ggplot(df, aes(
  x = e10))
e10 + geom_histogram(mapping = aes(y = stat(density))) +
  geom_density(color = "red", size = 1)

#################### construct ML model using mlr3####################
#################### define a task #########################################
diesel_subset = subset(df, select = c("diesel"))
str(diesel_subset)
tsk_diesel = as_task_regr(diesel_subset, target = "diesel", id = "date")
autoplot(tsk_diesel, type = "pairs")
