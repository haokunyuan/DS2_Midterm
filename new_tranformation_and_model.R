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
df_useful = df[,c("weekly_sales","store","is_holiday","dept","temperature","fuel_price","cpi","unemployment","type","size","week_of_year","year","month_of_year")]

df_1percent = sample_frac(df_useful,0.01)
df_1percent = 
  df_1percent %>% 
  group_by(dept) %>% 
  mutate(dept_avg_sales = mean(weekly_sales),
         dept_rank = ifelse(dept_avg_sales<6000,1,
                            ifelse(dept_avg_sales<16000,2,3)))
# foo$dept_ave_sales %>% hist()
df_1percent$dept_rank %>% hist()
# group stores
df_1percent = 
  df_1percent %>% 
  group_by(store) %>% 
  mutate(store_avg_sales = mean(weekly_sales),
         store_rank = ifelse(store_avg_sales<10500,1,
                            ifelse(store_avg_sales<18000,2,3)))
df_1percent$store_rank %>% hist()




df_1percent = subset(df_1percent,select = -c(dept,dept_avg_sales,store,store_avg_sales))
df_1percent$store_rank = as.factor(df_1percent$store_rank)
df_1percent$type = as.factor(df_1percent$type)
df_1percent$dept_rank = as.factor(df_1percent$dept_rank)



