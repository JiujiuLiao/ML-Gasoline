#install.packages("xts")
#install.packages("mlr3")
#install.packages("AutoPlots")
#install.packages("zoo")
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(xts)
library(mlr3verse)
library(mlr3verse)
library(mlr3viz)
library(lubridate)



#set work directory
setwd("C:/Users/jiuji/OneDrive/Desktop/study/Advanced method in data analysis/Applications in Data Analytics/Price dataset Feb.09th - Mar.07th")
setwd("C:/Users/jiuji/OneDrive/Desktop/study/Advanced method in data analysis/Applications in Data Analytics/Station dataset Feb.09th - Mar.07th")

#######################################################################################
###############################pre-processing data#####################################
#######################################################################################

#combine prices data of day 0209 and 0210
prices_0209th <- read.csv("C:/Users/jiuji/OneDrive/Desktop/study/Advanced method in data analysis/Applications in Data Analytics/Price dataset Feb.09th - Mar.07th/2024-02-09-prices.csv")
prices_0210th <- read.csv("C:/Users/jiuji/OneDrive/Desktop/study/Advanced method in data analysis/Applications in Data Analytics/Price dataset Feb.09th - Mar.07th/2024-02-10-prices.csv")
prices_combined <- rbind(prices_0209th,prices_0210th)

#combine stations data of day 0209 and 0210
stations_0209th <- read.csv("C:/Users/jiuji/OneDrive/Desktop/study/Advanced method in data analysis/Applications in Data Analytics/Station dataset Feb.09th - Mar.07th/2024-02-09-stations.csv")
stations_0210th <- read.csv("C:/Users/jiuji/OneDrive/Desktop/study/Advanced method in data analysis/Applications in Data Analytics/Station dataset Feb.09th - Mar.07th/2024-02-10-stations.csv")
stations_combined <- rbind(stations_0209th,stations_0210th)


#delete unreasonable data
p_sub_df <- subset(prices_combined, diesel > 0& e5 > 0& e10 > 0)
s_sub_df <- data.frame(subset(stations_combined, 
               latitude > 0 & longitude > 0 &
               uuid != "5f15982b-5505-4ea2-def3-f0c94afee70d" &
               uuid != "e70fb59b-3e7d-4ece-9e98-256206f0aec3"))

names(p_sub_df)[2] ="uuid"  #change station_uuid into uuid to combine stations data set and prices data set
prices_stations_combined <- data.frame( merge(p_sub_df,s_sub_df, by= "uuid", all.x  = FALSE, all.y = FALSE, sort =TRUE)) #merge two data sets into one data set

# select only date and diesel columns
date_diesel_data <- select(prices_stations_combined, date, diesel)
# convert date to a POSIXct object
dates <- as.POSIXct(date_diesel_data$date,format= "%Y-%m-%d %H:%M:%S", tz="UTC" )
# convert to xts object 
ts.date_diesel_data <- xts(date_diesel_data$diesel,
                           order.by = dates)

# inspect the object
str(ts.date_diesel_data)

#######################################################################################
###########################visualize the price of diesel###############################
#######################################################################################

##method 1 choose one random price in each hour, and see the trend
#random seed
set.seed(99)

date_sequence <- seq(from = as.POSIXct("2024-02-09 00:00:00", tz = "UTC"), 
                     to = as.POSIXct("2024-02-10 23:59:59", tz = "UTC"), 
                     by = "hour")

# choose 1 data point in per hour
selected_data <- sapply(date_sequence, function(date) {
  
  # find all data set in each hour
  data_in_hour <- ts.date_diesel_data[date <= index(ts.date_diesel_data) & index(ts.date_diesel_data) < date + 3600]
  
  # if there is not data in this hour，return back to NA
  if(length(data_in_hour) == 0) return(NA)
  
  # randomly choose a data point
  sample(data_in_hour, 1)
})

# create a new xts object
selected_ts <- xts(selected_data, order.by = date_sequence)
plot(selected_ts, main = "diesel price by randomly choosing", xlab = "date", ylab = "diesel")


##method 2 calculate the mean of price in each hour, and see the trend
# Convert xts object to data frame
date_diesel_data <- data.frame(date = index(ts.date_diesel_data), diesel = coredata(ts.date_diesel_data))

# Convert the date to hours
date_diesel_data$date <- floor_date(date_diesel_data$date, "hour")

# Calculate the average price for each hour
hourly_avg <- date_diesel_data %>%
  group_by(date) %>%
  summarise(avg_price = mean(diesel, na.rm = TRUE))

# Use ggplot to plot the price trend
ggplot(hourly_avg, aes(x = date, y = avg_price)) +
  geom_line(color = "black") +  # Set the line color to blue
  geom_point(color = "red") +  # Add red points to mark the average price for each hour
  geom_vline(xintercept = as.numeric(as.POSIXct("2024-02-09 23:59:59", tz = "UTC")), linetype = "dashed", color = "yellow") +  # Add a yellow dashed line in the between of the day
  scale_x_datetime(breaks = date_breaks("2 hours"), labels = date_format("%H:%M")) +  # Display the time every two hours
  labs(x = "Date", y = "Average Price", title = "Diesel Price Trend from 2024-02-09 00:00:00 to 2024-02-10 23:59:59") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate the x-axis labels to avoid overlap
