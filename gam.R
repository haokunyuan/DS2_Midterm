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

dev.off()
df = read_csv("data/merged_data_2.csv") %>% janitor::clean_names()
df_useful = df[,c("weekly_sales","store","is_holiday","dept","temperature","fuel_price","cpi","unemployment","type","size","week_of_year","year","month_of_year")]

df$weekly_sales %>% min()
df_useful$weekly_sales = log(df_useful$weekly_sales- min(df_useful$weekly_sales)+ 0.01) 
df_useful$weekly_sales %>% hist()
summary(df_useful$weekly_sales)

foo = quantile(df_useful$weekly_sales, seq(0,1,0.01)) 

df_1percent = sample_frac(df_useful,0.1)

# df_1percent$dept_rank %>% table()

df_1percent$store = as.factor(df_1percent$store)
df_1percent$type = as.factor(df_1percent$type)
df_1percent$dept = as.factor(df_1percent$dept)
# df_1percent$dept_rank = as.factor(df_1percent$dept_rank)
# df_1percent = subset(df_1percent,select = -c(dept,dept_ave_sales))
# done transform


unique(df$dept) %>% length()

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



# GAM 
gam.m1 <- gam(weekly_sales ~store+is_holiday+temperature+
              fuel_price+cpi+unemployment+type+size+
              week_of_year+year+month_of_year+dept, 
              data = train.data)

gam.m2 <- gam(weekly_sales ~
                store+
                is_holiday+
                s(temperature)+
                fuel_price+
                cpi+
                unemployment+
                type+
                size+
                week_of_year+
                year+
                month_of_year+
                dept, 
              data = train.data)
gam.m3 <- gam(weekly_sales ~
                store+
                is_holiday+
                s(temperature)+
                s(fuel_price)+
                cpi+
                unemployment+
                type+
                size+
                week_of_year+
                year+
                month_of_year+
                dept, 
              data = train.data)
gam.m4 <- gam(weekly_sales ~
                store+
                is_holiday+
                s(temperature)+
                s(fuel_price)+
                s(cpi)+
                s(unemployment)+
                type+
                size+
                week_of_year+
                year+
                month_of_year+
                dept, 
              data = train.data)
gam.m5 <- gam(weekly_sales ~
                store+
                is_holiday+
                s(temperature)+
                s(fuel_price)+
                s(cpi)+
                s(unemployment)+
                type+
                s(size)+
                week_of_year+
                year+
                month_of_year+
                dept, 
              data = train.data)
gam.m6 <- gam(weekly_sales ~ 
                store+
                is_holiday+
                s(temperature)+
                s(fuel_price)+
                s(cpi)+
                s(unemployment)+
                type+
                s(size)+
                s(week_of_year)+
                year+
                s(month_of_year)+
                dept, 
              data = train.data)
gam.m8 <- gam(weekly_sales ~ 
                 store+
                 is_holiday+
                 s(temperature)+
                 s(fuel_price)+
                 s(cpi)+
                 s(unemployment)+
                 type+
                 s(size)+
                 te(week_of_year,month_of_year)+
                 year+
                 dept, 
               data = train.data)

gam.m7 <- gam(weekly_sales ~ 
                store+
                is_holiday+
                s(cpi)+
                s(unemployment)+
                type+
                s(size)+
                te(week_of_year,month_of_year,temperature,fuel_price)+
                year+
                dept, 
              data = train.data)



anova(gam.m1, gam.m2, gam.m3, gam.m4, gam.m5,gam.m6,gam.m8, gam.m7, test = "F") 
AIC(gam.m1, gam.m2, gam.m3, gam.m4, gam.m5,gam.m6, gam.m7,gam.m8) 
anova(gam.m6, gam.m7,gam.m8, test = "F")


#plot
#gam.check(gam.m_6)
par(mfrow=c(2,2))
gam.check(gam.m1)

par(mfrow=c(2,4))
plot(gam.m2,se=TRUE,col = "red")
gam.pred = predict(gam.m2, newdata = test.data,type = "response")
# test MSE
mean((gam.pred-newy)^2) %>% sqrt()

gam.pred = predict(gam.m5, newdata = test.data,type = "response")
# test MSE
mean((gam.pred-newy)^2) %>% sqrt()

gam.pred = predict(gam.m6, newdata = test.data,type = "response")
# test MSE
mean((gam.pred-newy)^2) %>% sqrt()

gam.pred = predict(gam.m7, newdata = test.data,type = "response")
# test MSE
mean((gam.pred-newy)^2) %>% sqrt()

gam.pred = predict(gam.m8, newdata = test.data,type = "response")
# test MSE
mean((gam.pred-newy)^2) %>% sqrt()
