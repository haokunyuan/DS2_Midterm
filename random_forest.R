library(caret)
library(splines)
library(mgcv)
library(tidyverse)
library(ggplot2)
library(glmnet)
library(pls)
library(randomForest)
#====== 
# data Transformation
# there are 80 unique departments and we need to group them into smaller category
# base on their mean sales


df = read_csv("data/merged_data_2.csv") %>% janitor::clean_names()
df_useful = df[,c("weekly_sales","store","is_holiday","dept","temperature","fuel_price","cpi","unemployment","type","size","week_of_year","year","month_of_year")]

df_1percent = sample_frac(df_useful,0.5)
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
df_1percent = subset(df_1percent,select = -c(dept,dept_ave_sales,store))
# done transform




# seperate test and train 
smp_size <- floor(0.75 * nrow(df_1percent))
set.seed(123)
train_ind <- sample(seq_len(nrow(df_1percent)), size = smp_size)

train.data <- df_1percent[train_ind, ]
test.data  <- df_1percent[-train_ind, ]

x <- model.matrix(weekly_sales~.,train.data)[,-1]
y <- train.data$weekly_sales

newx =  model.matrix(weekly_sales~.,test.data)[,-1]
newy =  test.data$weekly_sales



#randome forest 
m1 <- randomForest(
  formula = weekly_sales ~ .,
  data    = train.data,
  keep.forest=TRUE,
  importance=FALSE,
  proximity=F,
  ntree=100,
  do.trace=5,
  nodesize=500
)

plot(m1)

mean((predict(m1,newdata = test.data)-test.data$weekly_sales)^2) %>% sqrt()
importance(m1)
varImpPlot(m1)
