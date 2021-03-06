#Midterm Project
library(caret)
library(splines)
library(mgcv)
library(tidyverse)
library(ggplot2)
library(glmnet)
library(pls)
library(randomForest)
library(MASS)

#====== 
# data Transformation
# there are 80 unique departments and we need to group them into smaller category
# base on their mean sales

dev.off()
df = read_csv("data/merged_data_2.csv") %>% janitor::clean_names()
df_useful = df[,c("weekly_sales","store","is_holiday","dept","temperature","fuel_price","cpi","unemployment","type","size","week_of_year","year","month_of_year")]

#df$weekly_sales %>% min()
df_useful$weekly_sales = log(df_useful$weekly_sales- min(df_useful$weekly_sales)+ 0.01) 
# df_useful$weekly_sales %>% hist()
# summary(df_useful$weekly_sales)



df_1percent = df_useful
 # sample_frac(df_useful,0.1)

# df_1percent$dept_rank %>% table()

df_1percent$store = as.factor(df_1percent$store)
df_1percent$type = as.factor(df_1percent$type)
df_1percent$dept = as.factor(df_1percent$dept)
# df_1percent$dept_rank = as.factor(df_1percent$dept_rank)
# df_1percent = subset(df_1percent,select = -c(dept,dept_ave_sales))
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


# train_data
ctrl1 <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
#----- 

#linear model
set.seed(2)
lm.fit <- train(weekly_sales~.,
                data= train.data,
                method = "lm",
                trControl = ctrl1)
#ridge model
set.seed(2)
ridge.fit <- train(weekly_sales~.,
                   data= train.data,
                   method = "glmnet",
                   tuneGrid = expand.grid(alpha = 0, 
                                          lambda = exp(seq(-50, 10, length=100))),
                   # preProc = c("center", "scale"),
                   trControl = ctrl1)

plot(ridge.fit, xTrans = function(x) log(x))

ridge.fit$bestTune

coef(ridge.fit$finalModel,ridge.fit$bestTune$lambda)


# lasso model

set.seed(2)
lasso.fit <- train(weekly_sales~.,
                   data= train.data,
                   method = "glmnet",
                   tuneGrid = expand.grid(alpha = 1, 
                                          lambda = exp(seq(-50, 5, length=100))),
                   # preProc = c("center", "scale"),
                   trControl = ctrl1)
plot(lasso.fit, xTrans = function(x) log(x))

lasso.fit$bestTune

coef(lasso.fit$finalModel,lasso.fit$bestTune$lambda)

# train data mse- compare lasso, ridge, lm 
resamp <- resamples(list(lasso = lasso.fit, ridge = ridge.fit, lm = lm.fit))
#summary(resamp)
#parallelplot(resamp, metric = "RMSE")
bwplot(resamp, metric = "RMSE")



## Principal components regression (PCR)

#PCR model- `pcr()`.
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



## Partial least squares (PLS)- `plsr()`.

set.seed(2)
pls.mod <- plsr(weekly_sales~., 
                data = df_1percent[train_ind,],
                #scale = TRUE, 
                validation = "CV")

summary(pls.mod)
validationplot(pls.mod, val.type="MSEP", legendpos = "topright")
selectNcomp(pls.mod, method = c("randomization"), #randomizationonesigma
            ncomp = pls.mod$ncomp)
predy2.pls <- predict(pls.mod, newdata = df_1percent[-train_ind,], 
                      ncomp = 47)
# test MSE
mean((predy2.pls-newy)^2) %>% sqrt()




# gam fit
gam.fit <- train(weekly_sales~.,
                 data = train.data,
                 method = "gam",
                 tuneGrid = data.frame(method = "GCV.Cp", select = FALSE),
                 trControl = ctrl1)

# GAM (anova)
gam.1 <- gam(weekly_sales ~store+is_holiday+temperature+
                fuel_price+cpi+unemployment+type+size+
                week_of_year+year+month_of_year+dept,
              data = train.data)

gam.2 <- gam(weekly_sales ~
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
gam.3 <- gam(weekly_sales ~
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
gam.4 <- gam(weekly_sales ~
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
gam.5 <- gam(weekly_sales ~
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
gam.6 <- gam(weekly_sales ~ 
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
gam.7 <- gam(weekly_sales ~
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

gam.8 <- gam(weekly_sales ~
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



anova(gam.1, gam.2, gam.3, gam.4, gam.5,gam.6,gam.8, gam.7, test = "F")
AIC(gam.1, gam.2, gam.3, gam.4, gam.5,gam.6, gam.7,gam.8)

###### Model seletion is based on the train MSE/RESAMPLING. GAM works well.

##### RMSE (train)
#lm 
pred_tr_l = predict(lm.fit, newdata = train.data)
mean((pred_tr_l - y)^2) %>% sqrt()
# lasso
pred_tr_lasso = predict(lasso.fit, newdata  = train.data, s=best.lambda) 
mean((pred_tr_lasso - y)^2) %>% sqrt()

#ridge
pred_tr_ridge = predict(ridge.fit, newdata  = train.data, s=best.lambda) 
mean((pred_tr_ridge - y)^2) %>% sqrt()
#pcr
pred_tr_pcr = predict(pcr.mod, newdata = train.data, ncomp = 129)
mean((pred_tr_pcr - y)^2) %>% sqrt()
#pls
pred_tr_pls = predict(pls.mod, ewdata = train.data, ncomp = 47)
mean((pred_tr_pls - y)^2) %>% sqrt()
#gam
pred_tr_gam2 = predict(gam.m6, newdata = train.data, type="response")
mean((pred_tr_gam2 - y)^2)%>% sqrt()


##### evaluate test performance
#####  RMSE (train) 
#lm 
pred_tr_l = predict(lm.fit, newdata = test.data)
mean((pred_tr_l - newy)^2) %>% sqrt()
# lasso
pred_tr_lasso = predict(lasso.fit, newdata  = test.data, s=best.lambda) 
mean((pred_tr_lasso - newy)^2) %>% sqrt()

#ridge
pred_tr_ridge = predict(ridge.fit, newdata  = test.data, s=best.lambda) 
mean((pred_tr_ridge - newy)^2) %>% sqrt()
#pcr
pred_tr_pcr = predict(pcr.mod, newdata = test.data, ncomp = 129)
mean((pred_tr_pcr - newy)^2) %>% sqrt()
#pls
pred_tr_pls = predict(pls.mod, newdata = test.data, ncomp = 47)
mean((pred_tr_pls - newy)^2) %>% sqrt()
#gam
pred_tr_gam2 = predict(gam.m6, newdata = test.data, type="response")
mean((pred_tr_gam2 - newy)^2)%>% sqrt()