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

df_plot = sample_frac(df,0.01)
plot(df$dept,df$weekly_sales)

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




