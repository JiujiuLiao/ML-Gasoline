#install.packages("partykit")
#install.packages("ggparty")

#libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(xts)
library(scales)
library(lubridate)
library(tibble)
#set work directory
setwd("C:/Users/jiuji/OneDrive/Desktop/study/Advanced method in data analysis/Applications in Data Analytics")
#######################################################################################
###############################pre-processing data#####################################
#######################################################################################
#combine prices data of day 0209 and 0210
prices_Feb.09th <- read.csv("Price dataset Feb.09th - Mar.07th/2024-02-09-prices.csv")
prices_Feb.10th <- read.csv("Price dataset Feb.09th - Mar.07th/2024-02-10-prices.csv")
prices_0910 <- rbind(prices_Feb.09th,prices_Feb.10th)


#combine stations data of day 02.09 and 02.10
stations_0209th <- read.csv("Station dataset Feb.09th - Mar.07th/2024-02-09-stations.csv")
stations_0210th <- read.csv("Station dataset Feb.09th - Mar.07th/2024-02-10-stations.csv")
stations_0910 <- rbind(stations_0209th,stations_0210th)
#checke stations differences on Feb. 09th and Feb10th. stations information in this two days have the same uuid, other information also almost the same
discrep <- mapply(setdiff, stations_0209th,stations_0210th)

#data pre-processing
#data vasualization

#convert time into posixct form
prices_0910[,"date"] <- as.POSIXct(prices_0910[,"date"])
head(prices_0910[,"date"])
#check missing values
sapply(prices_0910, function(x) sum(is.na(x)))

##########AVERAGE PRICE FUNCTION############
as.numeric(min(prices_0910[,"date"]))
# 00:00 25.04 => 1713996000

#testprice <- prices_0910[prices_0910$station_uuid=="ec18ae16-4359-49d5-acbb-3b5e31d214aa",]
strftime(1707433201)

average_price <- function(id, fueltype){
  #initializing variables to be used later
  averages <- NA
  last_price <- NA
  testing <- NA
  #iterating over 25 steps 
  for (i in 1:48){
    averages[i] <- NA
    # 1707433200 is 00:00:00 09.02.24
    # We add 3600 sec (1 hour) times the iterated step to get a start and end time for the period we look at
    starter <- 1707433200 + (i-1)*3600
    end <- starter + 3600
    # cutting the dataframe to only show price changes within the selected hour for the selected station
    within_price_changes <- prices_0910[prices_0910$station_uuid == id &
                                          prices_0910$date <= end &
                                          prices_0910$date >= starter,]
    
    # very first price to be set, before this price we have no information for the prices of the given gas station
    if (nrow(within_price_changes) != 0 & is.na(last_price)){
      last_price <- within_price_changes[within_price_changes[,"date"] == max(within_price_changes[,"date"]),fueltype]
    }
    
    # after a first price has been set there are different cases for what can happen
    else if (is.na(last_price) == FALSE){
      # case 1: the entire hour goes by without another change in price. The average is the last price that was set.
      if (nrow(within_price_changes) == 0){
        averages[i] <- last_price
      }
      # case 2: one price has been set, old and new price are averaged according to their fraction of the hour
      else if (nrow(within_price_changes) == 1){
        averages[i] <- as.numeric(((as.numeric(within_price_changes[1,"date"]) - starter)) * last_price + 
                                    (end - as.numeric(within_price_changes[1,"date"])) * within_price_changes[1,fueltype]) /
          3600
      }
      # case 3: more than one price has been set. They are all averaged according to their fraction of the hour
      else{
        
        # fraction of the price that was set before the start of the hour
        first_part <-  ((as.numeric(within_price_changes[1,"date"]) - starter) * last_price ) /
          3600
        
        # fractions of the prices set within the hour
        other_parts <- NA
        for (j in 1:(nrow(within_price_changes))){
          
          # last price to be set
          if (j == nrow(within_price_changes)){
            other_parts[j] <- ((end - as.numeric(within_price_changes[j,"date"])) * within_price_changes[j,fueltype]) /
              3600
          }
          # all the other prices. in general they follow the formula ((t3 - t2)*p2)/3600, where p2 is the second price set, t2 is the time its set and t3 is the time the third price is set
          else {
            other_parts[j] <- ((as.numeric(within_price_changes[(j+1),"date"]) - as.numeric(within_price_changes[j,"date"])) * within_price_changes[j,fueltype] ) /
              3600
          }
        }
        #summing up the fractions
        averages[i] <- sum(c(first_part, other_parts))
      }
    }
  }
  return(averages)
  #return(testing) #use in case it breaks and insert stuff like testing[i] <- i 
}

#example how to use the function
average_price("ec18ae16-4359-49d5-acbb-3b5e31d214aa","diesel")


##############ADDING AVERAGE PRICES TO STATION(so far only diesel)#######################
rm(hourly_averages)
hourly_averages <- data.frame(unique(stations_0910$uuid))
colnames(hourly_averages)[1] <- "id"

#creating a table with every gas station as rows and the columns being the 48 hour windows where we take averages
for (id in unique(stations_0910$uuid)) {        
  station_id_row <- unique(stations_0910$uuid) == id
  hourly_averages[station_id_row, 2:49] <- average_price(id,"diesel") 
}

A <- NA
for (i in 1:48){
  starter <- 1707433200 + (i-1)*3600
  end <- starter + 3600
  A[i] <- paste(strftime(starter, format = "%b.%d. %H:%M"),"-",strftime(end, format = "%H:%M"), sep = " ")
}
colnames(hourly_averages)[2:49] <- A
rm(A)
options(digits=3)


hourly_averages_long <- pivot_longer(hourly_averages, !id, names_to = "time", values_to = "price")
#write.csv(hourly_averages_long, "hourly_averages_long.csv")
hourly_averages_long <- read.csv("Station dataset Feb.09th - Mar.07th/hourly_averages_long.csv") #reading this file directly
colnames(hourly_averages_long)[2] <- "uuid"

#plug in more features that extracted by QGIS
spatialinfo_stations <- read.csv("stations_all_germany_ from kirill.csv")
df <- merge(hourly_averages_long,spatialinfo_stations,by="uuid")

#convert time into numeric numbers, so that change time into time_period
df <- add_column(df, time_period = as.numeric(substring(df[,3],9,10)), .after="time")#drop NAs to simplify dataset(#df %>% drop_na(df) can only remove numeric or character NAs)
#add a new time feature time of a day after column time_period
df <- df %>%
  mutate(time_of_day = case_when(
    time_period >= 0 & time_period < 6 ~ "Night",
    time_period >= 6 & time_period < 12 ~ "Morning",
    time_period >= 12 & time_period < 18 ~ "Afternoon",
    time_period >= 18 & time_period <= 23 ~ "Evening",
    TRUE ~ NA_character_  # for any times outside 0-23
  ))%>%
  relocate(time_of_day, .after = "time_period")

sapply(df, function(x) sum(is.na(x)))
df <- df[-which(df$brand == ""), ]  # drop blank cells in brand column
df <- df[complete.cases(df), ]      # drop NAs in brand column

# make up a new subset for ML task(exclude )
df.subset = df[,c(-1,-2,-3,-4,-8,-9,-10)]
str(df.subset)


#set character variables into factor so that regression tree can work
df.subset[,1] <- as.factor(df.subset[,1])
df.subset[,3] <- as.factor(df.subset[,3])
df.subset[,5] <- as.factor(df.subset[,5])
df.subset[,6] <- as.factor(df.subset[,6])
df.subset[,7] <- as.factor(df.subset[,7])

test.df <- df.subset[1:3000,]

##################ML process ################
library(mlr3)
library(mlr3verse)
library(mlr3viz)
library(rpart)
library(kknn)
library(xgboost)
library(kernlab)
#install.packages('kernlab')
# load a task
tsk_diesel.price = as_task_regr(test.df, target = "price",id="id")
print(tsk_diesel.price)

#Partition data
#training set (67% of the total data by default) and test set (the remaining 33% of the total data not in the training set).
splits = partition(tsk_diesel.price)
print(splits$train)

#define a regression decision tree learner and a 24 fold cross-validation resampling object
lrn_rpart <- lrn("regr.rpart")
resampling <- rsmp("cv")
set.seed(2024)
ins <- resampling$instantiate(tsk_diesel.price)
print(ins)
rr_cv <- resample(tsk_diesel.price,lrn_rpart, rsmp("cv", folds = 4))
autoplot(rr_cv, measure = msr("regr.rmse"), type = "boxplot")
autoplot(rr_cv, measure = msr("regr.rmse"), type = "histogram")

lrn_featureless <- lrn("regr.featureless")
lrn_ranger <- lrn("regr.ranger")
rr_resample <- resample(tsk_diesel.price, lrn_ranger, resampling)#it takes very long time
learners = list(
  lrn("regr.rpart"),
  #lrn("regr.randomForest", predict_type = "response", ntree = 8L),
  lrn("regr.kknn",k = 5L),
  lrn("regr.featureless"),
  lrn("regr.ksvm")
  )
# Define a decision tree, a random forest, a k-NN, and a featureless learner.
#learners <- lrns(c("regr.rpart","regr.randomForest", "regr.kknn", "regr.ksvm", "regr.featureless"))
# "regr.randomForest" costs long time

#set a benchmark
design = benchmark_grid(
  tasks = tsk_diesel.price,
  learners = learners,
  resamplings = resampling
  )
bmr = benchmark(design)
bmr
bmr$aggregate()

#Visual Comparison of the Benchmark Results (Boxplot)
autoplot(bmr) +
  theme_bw() +
  ylab("Regression Error") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))

#Visual Comparison of the Benchmark Results (ROC Curve)
autoplot(bmr$filter(task_id = "tsk_diesel.price"), type = "roc") + theme_bw()



###what should be done next step
#fixing random forest and xgboost
#tunning hyperparameters 
#choose the final model
#interpretable machine learning: show our result in a general way
#show interesting example of stations comparison(IML:plot PDP): a couple of list


#usage of our model as a company or a person; how this could be more useful
 





1





































































































#convert blank cells in two data sets into NA, then use function to remove missing values
prices_0910[prices_0910==""] <- NA
stations_0910[stations_0910==""]<- NA

#check how many missing values are in stations_0910 and prices_0910 dataset
sum(is.na(prices_0910))   # 0 missing values
sum(is.na(stations_0910))   # 3432 missing values

#check which columns have missing values in stations_0910 dataset
sapply(stations_0910, function(x) sum(is.na(x)))

####later on, impute missing values using of brands if we need it
#delete brand missing values
rm_missing_stations_0910 <- subset(stations_0910, !is.na(stations_0910[,2])
                                   & !is.na(stations_0910[,3]) 
                                   & !is.na(stations_0910[,5])
                                   & !is.na(stations_0910[,7]))
sapply(rm_missing_stations_0910, function(x) sum(is.na(x)))  #double check missing values of brand


#delete unreasonable data which gasoline price is or less or equal to 0 
p_0910 <- subset(prices_0910, diesel > 0& e5 > 0& e10 > 0)

names(p_0910)[2] ="uuid"  #change station_uuid into uuid to combine stations data set and prices data set
prices_stations_0910 <- data.frame(merge(p_0910,rm_missing_stations_0910,by="uuid")) #merge two data sets into one data set

#delete duplicated rows
prices_stations_0910 <- prices_stations_0910[!duplicated(prices_stations_0910), ]

#proportion of stations
station_counts <- prices_stations_0910 %>%
  group_by(brand) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))




#choose 10 brands which have the most stations
top_brands <- station_counts %>%
  arrange(desc(freq)) %>%
  mutate(brand = if_else(row_number() <= 10, brand, 'Other')) %>%
  group_by(brand) %>%
  summarise(freq = sum(freq))

total_freq = sum()
# calculate proportion of each brand
top_brands <- top_brands %>%
  mutate(prop = freq / total_freq)

#draw ggplot
ggplot(top_brands, aes(x = reorder(brand, freq), y = freq)) +
  geom_bar(stat = "identity", fill = '#FFA500') +
  geom_text(aes(label = scales::percent(prop))) +
  coord_flip() +
  labs(x = "Brand", y = "Frequency", title = "Proportion of Top 10 Brands") +
  theme_minimal()


#visualise prices by changing the p_0910 data set into a long table using gather function
p_long_df <- gather(p_0910, fuel_type, value, diesel, e5, e10)
# draw a density plot of three different gasoline
ggplot(p_long_df, aes(x = value, color = fuel_type)) +
  geom_histogram(aes(y = ..density..), alpha = 0.5, position = 'identity', bins = 30) +
  geom_density(alpha = 0.7) +
  labs(x = "Price", y = "Density", fill = "Fuel Type")

# to classification task, we need to convert target into factor or ordered factor, so that mlr3 package can work
prices_stations_0910$diesel <- as.factor(prices_stations_0910$diesel)

#set a task
task <- TaskClassif$new(id = "fuel", backend = prices_stations_0910, target = "diesel")
task$select("brand") # keep only brand feature

# load a classification ksvm learner
learner = mlr3::lrn("classif.ksvm")



#split the data
n <- nrow(prices_stations_0910)
row_ids <- seq_len(n)  #order our data, then choose the first 80% data as train set
train_set <- row_ids[1:(n * 0.8)]
test_set <- row_ids[(n * 0.8):n]

#train model
#df_0910$time <- as.numeric(format(df_0910$time, "%Y%m%d%H%M%S"))
learner$train(task= task,row_ids=train_set)


# Make predictiotask = # Make predictiotsk()# Make predictions
predictions <- learner$predict(task, row_ids = test_set)

# Evaluate the model
performance(predictions, measures = msr("regr.mae"))











