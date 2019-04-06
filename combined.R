library(caret)
library(splines)
library(mgcv)
library(tidyverse)
library(ggplot2)
library(glmnet)
#------
library(doParallel)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)
#------

df = read_csv("data/merged_data_2.csv") %>% janitor::clean_names()
names(df)
df_cleaned = df[c(-1,-9:-13)]
df_cleaned = df_cleaned %>% select(-"date")
df_cleaned$dept = as.factor(df_cleaned$dept)
df_cleaned$type = as.factor(df_cleaned$type)
names(df_cleaned)
head(df_cleaned)




# seperate test and train 
smp_size <- floor(0.75 * nrow(df_cleaned))
set.seed(123)
train_ind <- sample(seq_len(nrow(df_cleaned)), size = smp_size)

train.data <- df_cleaned[train_ind, ]
test.data  <- df_cleaned[-train_ind, ]
#train.data = sample_frac(train.data,0.1)

x = model.matrix(weekly_sales~.,train.data)[,-1]
y = train.data$weekly_sales



#lasso using glmnet 
set.seed(2)
cv.lasso <- cv.glmnet(x, y,
                     alpha = 1,
                     lambda = exp(seq(-40, 10, length=100)),
                     type.measure = "mse",
                     parallel=TRUE)
plot(cv.lasso)
cv.lasso$lambda.1se
cv.lasso$lambda.min
summary(cv.lasso)
predict(cv.lasso, s="lambda.min", type="coefficients")







#caret
ctrl1 = trainControl(method = "repeatedcv", number = 10, repeats = 5)
#ridge
set.seed(123)
ridge <- 
  train(
  x,y, method = "glmnet",
  trControl = ctrl1,
  tuneGrid = expand.grid(alpha = 0, lambda = exp(seq(5, 7, length=100)) 
                         ))
plot(ridge, xTrans = function(x)log(x))
ridge$bestTune %>% log()

# Model coefficients
coef(ridge$finalModel, ridge$bestTune$lambda)
# Make predictions
predictions <- ridge %>% predict(test.data)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  Rsquare = R2(predictions, test.data$medv)
)













lambda <- 10^seq(-10, 10, length = 100)

#lasso caret 
set.seed(123)
lasso <- 
  train(
  x, 
  y, 
  method = "glmnet",
  trControl = ctrl1,
  tuneGrid = expand.grid(alpha = 1, lambda = exp(seq(-10,10, length = 100))),
  allowParallel=TRUE
)
plot(lasso)

# Model coefficients
coef(lasso$finalModel, lasso$bestTune$lambda)
# Make predictions
predictions <- lasso %>% predict(test.data)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  Rsquare = R2(predictions, test.data$medv)
)




#====== 
#install.packages("AmesHousing")
#install.packages("rsample")
library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)          # an extremely fast java-based platform
set.seed(123)
ames_split <- initial_split(df_cleaned, prop = .7)
ames_train <- training(ames_split)
ames_test  <- testing(ames_split)

m1 <- randomForest(
  formula = weekly_sales ~ .,
  data    = ames_train
)

m1


# data Transformation
# there are 80 unique departments and we need to group them into smaller category
# base on their mean sales

df_plot = sample_frac(df,0.01)
plot(df_plot$dept,df_plot$weekly_sales)
unique(df_plot$dept) %>% length()
unique(df_plot$store) %>% length()
df_plot %>% group_by(dept) %>% summarise(avgsale = mean(weekly_sales)) %>% plot()

df = read_csv("data/merged_data_2.csv") %>% janitor::clean_names()
df_useful = df[,c("weekly_sales","store","is_holiday","dept","temperature","fuel_price","cpi","unemployment","type","size","week_of_year","year","month_of_year")]

df_1percent = sample_frac(df_useful,0.1)
df_1percent =
  df_1percent %>%
  group_by(dept) %>%
  mutate(dept_ave_sales = mean(weekly_sales),
         dept_rank = ifelse(dept_ave_sales<6000,1,
                            ifelse(dept_ave_sales<16000,2,3)))


# foo$dept_ave_sales %>% hist()
df_1percent$dept_rank %>% hist()

df_1percent$store = as.factor(df_1percent$store)
df_1percent$type = as.factor(df_1percent$type)
df_1percent$dept_rank = as.factor(df_1percent$dept_rank)
df_1percent = subset(df_1percent,select = -c(dept,dept_ave_sales))



ames_split <- initial_split(df_1percent, prop = .7)
ames_train <- training(ames_split)
ames_test  <- testing(ames_split)

m1 <- randomForest(
  formula = weekly_sales ~ .,
  data    = ames_train
)

m1
plot(m1)
# number of trees with lowest MSE
which.min(m1$mse)
## [1] 202

# RMSE of this optimal random forest
sqrt(m1$mse[which.min(m1$mse)])
## [1] 15958.34


# create training and validation data 
set.seed(123)
valid_split <- initial_split(ames_train, .8)

# training data
ames_train_v2 <- analysis(valid_split)

# validation data
ames_valid <- assessment(valid_split)
x_test <- ames_valid[setdiff(names(ames_valid), "weekly_sales")]
y_test <- ames_valid$weekly_sales

rf_oob_comp <- randomForest(
  formula = weekly_sales ~ .,
  data    = ames_train_v2,
  xtest   = x_test,
  ytest   = y_test
)

# extract OOB & validation errors
oob <- sqrt(rf_oob_comp$mse)
validation <- sqrt(rf_oob_comp$test$mse)

# compare error rates
tibble::tibble(
  `Out of Bag Error` = oob,
  `Test error` = validation,
  ntrees = 1:rf_oob_comp$ntree
) %>%
  gather(Metric, RMSE, -ntrees) %>%
  ggplot(aes(ntrees, RMSE, color = Metric)) +
  geom_line() +
  scale_y_continuous(labels = scales::dollar) +
  xlab("Number of trees")


#Initial tuning with randomForest
features <- setdiff(names(ames_train), "weekly_sales")

set.seed(123)

m2 <- tuneRF(
  x          = ames_train[features],
  y          = ames_train$weekly_sales,
  ntreeTry   = 500,
  mtryStart  = 5,
  stepFactor = 1.5,
  improve    = 0.01,
  trace      = FALSE      # to not show real-time progress 
)


importance(m1)

varImpPlot(m1)

