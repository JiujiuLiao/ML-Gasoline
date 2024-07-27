#libraries
library(dplyr)
library(ggplot2)

#set work directory
setwd("C:/Users/jiuji/OneDrive/Desktop/study/Advanced method in data analysis/Applications in Data Analytics")

df <- read.csv("aggregated_data.csv",row.names = 1)
# make up a new subset for ML task(exclude )
df.subset.e5 = df[,c(-1,-2,-4)]
str(df.subset.e5)

#set character variables into factor so that regression tree can work
df.subset.e5[,2] <- as.factor(df.subset.e5[,2])
df.subset.e5[,5] <- as.factor(df.subset.e5[,5])
df.subset.e5[,6] <- as.factor(df.subset.e5[,6])
df.subset.e5[,7] <- as.factor(df.subset.e5[,7])
df.subset.e5[,9] <- as.factor(df.subset.e5[,9])
df.subset.e5[,10] <- as.factor(df.subset.e5[,10])
test.df <- df.subset.e5[1:2000,]

##################ML process ################
library(mlr3)
library(mlr3verse)
library(mlr3viz)
library(rpart)
library(kknn)
#library(xgboost)
library(kernlab)
library(paradox)

# load a task
tsk_e5.price = as_task_regr(test.df, target = "e5_price",id="id")
print(tsk_e5.price)

#Partition data
#training set (67% of the total data by default) and test set (the remaining 33% of the total data not in the training set).
set.seed(2024)
splits = partition(tsk_e5.price)
print(splits$train)

# Define learners and resampling strategy
learners <- list(
  lrn("regr.rpart"),
  lrn("regr.kknn", k = 10L),
  lrn("regr.featureless"),
  lrn("regr.ranger"),
  lrn("regr.ksvm")
)


resampling <- rsmp("cv", folds = 4)
resampling$instantiate(tsk_e5.price)

# Perform cross-validation for decision tree
rr_cv <- resample(tsk_e5.price, lrn("regr.rpart"), resampling)

# Benchmark learners
design <- benchmark_grid(
  tasks = tsk_e5.price,
  learners = learners,
  resamplings = resampling
)

bmr <- benchmark(design)
print(bmr)
print(bmr$aggregate())

#Visualize the Comparison of Benchmark (Boxplot)
autoplot(bmr) +
  theme_bw() +
  ylab("Regression Error") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))

#Hyperparameter optimization

#search space
lts_rpart = lts("regr.rpart.default")
lts_kknn = lts("regr.kknn.default")
lts_ranger = lts("regr.ranger.default")
lts_svm = lts("regr.svm.default")


# Define tuning instances
tuner <- tnr("random_search")
tuner

at_rpart <- AutoTuner$new(
  learner = lrn("regr.rpart"),
  resampling = rsmp("cv", folds = 5),
  measure = msr("regr.rmse"),
  search_space = lts_rpart,
  terminator = trm("evals", n_evals = 5),
  tuner = tuner
)

at_kknn <- AutoTuner$new(
  learner = lrn("regr.kknn"),
  resampling = rsmp("cv", folds = 5),
  measure = msr("regr.rmse"),
  search_space = lts_kknn,
  terminator = trm("evals", n_evals = 5),
  tuner = tuner
)

at_ranger <- AutoTuner$new(
  learner = lrn("regr.ranger"),
  resampling = rsmp("cv", folds = 5),
  measure = msr("regr.rmse"),
  search_space = lts_ranger,
  terminator = trm("evals", n_evals = 5),
  tuner = tuner
)

at_ksvm <- AutoTuner$new(
  learner = lrn("regr.ksvm"),
  resampling = rsmp("cv", folds = 5),
  measure = msr("regr.rmse"),
  search_space = lts_svm,
  terminator = trm("evals", n_evals = 5),
  tuner = tuner
)

# Define learners with hyperparameter tuning
learners_tuned <- list(at_rpart, at_kknn, at_ranger, at_ksvm, lrn("regr.featureless"))
learners_tuned <- list(at_rpart, at_kknn, at_ranger,lrn("regr.featureless"))

# Define outer resampling
outer_resampling <- rsmp("cv", folds = 4)

# Perform nested resampling
design <- benchmark_grid(
  tasks = tsk_e5.price,
  learners = learners_tuned,
  resamplings = outer_resampling
)

bmr <- benchmark(design)
print(bmr)
print(bmr$aggregate(msr("regr.rmse")))

# Visual comparison of the benchmark results
autoplot(bmr) +
  theme_bw() +
  ylab("Regression Error") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12)) 



