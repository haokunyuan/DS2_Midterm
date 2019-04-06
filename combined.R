library(caret)
library(splines)
library(mgcv)
library(tidyverse)
library(ggplot2)
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

train.data = sample_frac(train.data,0.1)

#caret
ctrl1 = trainControl(method = "repeatedcv", number = 10, repeats = 5)
ctr2 = trainControl("cv", number = 10)
lambda <- 10^seq(-10, 10, length = 100)

#lasso
set.seed(123)
lasso <- 
  train(
  weekly_sales ~., 
  data = train.data, 
  method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 1, lambda = exp(seq(-1,10, length = 100))),
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



#ridge
set.seed(123)
ridge <- train(
  weekly_sales ~., data = train.data, method = "glmnet",
  trControl = ctrl1,
  tuneGrid = expand.grid(alpha = 0, lambda = lambda)
)
# Model coefficients
coef(ridge$finalModel, ridge$bestTune$lambda)
# Make predictions
predictions <- ridge %>% predict(test.data)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  Rsquare = R2(predictions, test.data$medv)
)
