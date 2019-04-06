library(caret)
library(splines)
library(mgcv)
library(tidyverse)
library(ggplot2)
library(glmnet)
#====== 
#install.packages("AmesHousing")
#install.packages("rsample")
library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)          # an extremely fast java-based platform

# data Transformation
# there are 80 unique departments and we need to group them into smaller category
# base on their mean sales


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







set.seed(123)
ames_split <- initial_split(df_cleaned, prop = .7)
ames_train <- training(ames_split)
ames_test  <- testing(ames_split)

m1 <- randomForest(
  formula = weekly_sales ~ .,
  data    = ames_train
)

m1