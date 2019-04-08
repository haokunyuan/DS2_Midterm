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
df_1percent$store = as.factor(df_1percent$store)
df_1percent$type = as.factor(df_1percent$type)
df_1percent$dept = as.factor(df_1percent$dept)

smp_size <- floor(0.75 * nrow(df_1percent))
set.seed(123)
train_ind <- sample(seq_len(nrow(df_1percent)), size = smp_size)

train.data <- df_1percent[train_ind, ]
test.data  <- df_1percent[-train_ind, ]

x <- model.matrix(weekly_sales~.,train.data)[,-1]
y <- train.data$weekly_sales

newx =  model.matrix(weekly_sales~.,test.data)[,-1]
newy =  test.data$weekly_sales


set.seed(2)
pcr.mod <- pcr(weekly_sales~., 
               data = df_1percent[train_ind,],
               #scale = TRUE, 
               validation = "CV")

summary(pcr.mod)
predplot(pcr.mod)
coefplot(pcr.mod)


# Plot the R2
validationplot(pcr.mod, val.type = "R2")
validationplot(pcr.mod, val.type="MSEP", legendpos = "topright")

selectNcomp(pcr.mod, method = c("randomization"),
            ncomp = pcr.mod$ncomp)

predy2.pcr <- predict(pcr.mod, newdata = df_1percent[-train_ind,], 
                      ncomp = 129)

# test MSE
mean((predy2.pcr-newy)^2) %>% sqrt()



## Partial least squares (PLS)

#We fit the PLS model using the function `plsr()`.

set.seed(2)
pls.mod <- plsr(weekly_sales~., 
                data = df_1percent[train_ind,],
                #scale = TRUE, 
                validation = "CV")

summary(pls.mod)
validationplot(pls.mod, val.type="MSEP", legendpos = "topright")
selectNcomp(pls.mod, method = c("randomization"),
            ncomp = pcr.mod$ncomp)
predy2.pls <- predict(pls.mod, newdata = df_1percent[-train_ind,], 
                      ncomp = 33)
# test MSE
mean((predy2.pls-newy)^2) %>% sqrt()


