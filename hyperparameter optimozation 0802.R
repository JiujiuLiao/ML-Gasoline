# in case regr.ksvm not found execute following comments
# 
#options(repos = c(
#  mlrorg = "https://mlr-org.r-universe.dev",
#  CRAN = "https://cloud.r-project.org/"
#))
# install.packages("mlr3extralearners")

library(dplyr)
library(ggplot2)

#set work directory
setwd("C:/Users/jiuji/OneDrive/Desktop/study/Advanced method in data analysis/Applications in Data Analytics")

df <- read.csv("aggregated_data.csv",row.names = 1)
# make up a new subset for ML task(exclude )
df.subset.e5 = df[,c(-1,-2,-4)]
str(df.subset.e5)
rm(df)

#set character variables into factor so that regression tree can work
df.subset.e5[, c("brand", "region", "settlement","count_part","day","weekend","hour")] <- lapply(
  df.subset.e5[, c("brand", "region", "settlement","count_part","day","weekend","hour")], as.factor)

test.df <- df.subset.e5[1:2000,]

##################ML process ################
library(mlr3)
library(mlr3extralearners)
library(mlr3verse)
library(mlr3viz)
library(mlr3tuning)
library(mlr3hyperband)
library(rpart)
library(kknn)
#library(xgboost)
library(kernlab)
library(paradox)

#Partition data
train_data <- subset(test.df, test.df[, 9] != "Thu")
test_data <- subset(test.df, test.df[, 9] == "Thu")

# load a task
tsk_e5.price = as_task_regr(test.df, target = "e5_price",id="e5.price")
print(tsk_e5.price)

# Define learners and resampling strategy
learners <- list(
  lrn("regr.rpart"),
  lrn("regr.kknn", k = 10L),
  lrn("regr.featureless"),
  lrn("regr.ranger"),
  lrn("regr.ksvm")
)

resampling <- rsmp("cv", folds = 5)
resampling$instantiate(tsk_e5.price)


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
  ylab("Regression mse") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))

#tuning ksvm learner using Hyperband 
l_ksvm <-lrn("regr.ksvm")
ksvm_rob <- ppl("robustify", task = tsk_e5.price, learner = l_ksvm)  %>>% l_ksvm
ksvm_rob %>% plot

po("subsample")  #subsampling 

graph_learner = GraphLearner$new(                
  po("subsample") %>>%
    ksvm_rob) 
#The graph learner subsamples and then fits a support vector machine on the data subset. 
#The parameter set of the graph learner is a combination of the parameter sets of the PipeOp and learner.
graph_learner$id <- "hb.ksvm"


# List all available parameters
available_params <- as.data.table(graph_learner$param_set)[, .(id, lower, upper, levels)]
print(available_params)

# Set parameter values(search space) for tuning
graph_learner$param_set$set_values(
  subsample.frac        = to_tune(p_dbl(3^-3, 1, tags = "budget")),
  regr.ksvm.kernel      = to_tune(c("rbfdot", "laplacedot", "besseldot", "anovadot")),
  regr.ksvm.C           = to_tune(1e-4, 1e3, logscale = TRUE),
  regr.ksvm.sigma       = to_tune(1e-4, 1e3, logscale = TRUE),
  regr.ksvm.tol         = to_tune(1e-4, 2, logscale = TRUE),
  regr.ksvm.degree      = to_tune(2, 5),
  regr.ksvm.type        = "eps-svr"
)

#Support vector machines often crash or never finish the training with certain hyperparameter configurations. 
#We set a timeout of 30 seconds and a fallback learner to handle these cases.
graph_learner$encapsulate = c(train = "evaluate", predict = "evaluate")
graph_learner$timeout = c(train = 30, predict = 30)  #30 seconds
graph_learner$fallback = lrn("regr.featureless")

#Let’s create the tuning instance. We use the "none" terminator because Hyperband controls the termination itself.
instance_hb = TuningInstanceSingleCrit$new(
  task = tsk_e5.price,
  learner = graph_learner,
  resampling = rsmp("cv", folds = 3),
  #measures = msr("regr.rmse"),
  terminator = trm("none")
)
instance_hb

#We load the Hyperband tuner and set eta = 3.

tuner = tnr("hyperband", eta = 3)
tuner$optimize(instance_hb)

# Create a new learner with the tuned parameters
tuned_ksvm <- graph_learner$clone(deep = TRUE)
tuned_ksvm$param_set$values <- instance_hb$result_learner_param_vals

#random forest,knn, decision tree optimization
#search space
lts_rpart = lts("regr.rpart.default")
lts_kknn = lts("regr.kknn.default")
lts_ranger = lts("regr.ranger.default")
#lts_ksvm = lts("regr.svm.default")


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

# Define learners with hyperparameter tuning
learners_tuned <- list(at_rpart, at_kknn, at_ranger,tuned_ksvm, lrn("regr.featureless"))
learners_total <- list(at_rpart, 
                       at_kknn, 
                       at_ranger,
                       tuned_ksvm, 
                       lrn("regr.featureless"),
                       lrn("regr.rpart"),
                       lrn("regr.kknn", k = 10L),
                       lrn("regr.ranger"),
                       lrn("regr.ksvm"))
#learners_tuned <- list(at_rpart$learner, at_kknn$learner, at_ranger$learner, instance_hb, lrn("regr.featureless"))
#print(learners_tuned)
# Define outer resampling
outer_resampling <- rsmp("cv", folds = 5)

# Perform nested resampling
design <- benchmark_grid(
  tasks = tsk_e5.price,
  learners = learners_total,
  resamplings = outer_resampling
)

bmr <- benchmark(design)
print(bmr)
print(bmr$aggregate(msr("regr.rmse")))

# Visual comparison of the benchmark results
autoplot(bmr) +
  theme_bw() +
  ylab("Regression rmse") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12)) 

1
library(purrr)  # For using the map function
# Split maps# Split the data into training and testing sets
train_data <- test.df %>% filter(day %in% c("Fri", "Sat", "Sun", "Mon", "Tue"))
test_data <- test.df %>% filter(day %in% c("Wed", "Thu"))

# Predict on the test data using the trained ranger model
predictions <- at_ranger$predict_newdata(test_data)
print(predictions)

#run the code above, if it works we can try to visualise the results

