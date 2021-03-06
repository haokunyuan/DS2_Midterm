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
quantile(df$weekly_sales, seq(0,1,0.1)) 
foo = quantile(df$weekly_sales, seq(0,1,0.1))

# department ranked according to the average weekly sale
df_1percent = sample_frac(df_useful,0.1)
df_1percent =
  df_1percent %>%
  group_by(dept) %>%
  mutate(dept_ave_sales = mean(weekly_sales),
         dept_rank = ifelse(dept_ave_sales<foo[1],1,
                            ifelse(dept_ave_sales<foo[2],2,
                                   ifelse(dept_ave_sales<foo[3],3, 
                                          ifelse(dept_ave_sales<foo[4],4,
                                                 ifelse(dept_ave_sales<foo[5],5,
                                                        ifelse(dept_ave_sales<foo[6],6,
                                                               ifelse(dept_ave_sales<foo[7],7,
                                                                      ifelse(dept_ave_sales<foo[8],8,
                                                                             ifelse(dept_ave_sales<foo[9],9,10))))))))))




# foo$dept_ave_sales %>% hist()
df_1percent$dept_rank %>% hist()

df_1percent$store = as.factor(df_1percent$store)
df_1percent$type = as.factor(df_1percent$type)
df_1percent$dept_rank = as.factor(df_1percent$dept_rank)
df_1percent = subset(df_1percent,select = -c(dept,dept_ave_sales))
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



# linear model 
lm.fit <- lm(weekly_sales~., data = train.data)
summary(lm.fit)

mean((predict(lm.fit,newdata = test.data ) - newy)^2) %>% sqrt() # test

#lasso using glmnet 
set.seed(2)
cv.lasso <- cv.glmnet(x, y,
                      alpha = 1,
                      lambda = exp(seq(-40, 10, length=100)),
                      type.measure = "mse",
                      parallel = TRUE)
plot(cv.lasso)
cv.lasso$lambda.1se
cv.lasso$lambda.min
predict(cv.lasso, s="lambda.min", type="coefficients")

mean((predict(cv.lasso, newx = newx ,s="lambda.min", type="response") - newy)^2) %>% sqrt()


# ridge

cv.ridge <- cv.glmnet(x, y,
                      alpha = 0,
                      lambda = exp(seq(-40, 10, length=100)),
                      type.measure = "mse",
                      parallel = TRUE)
plot(cv.ridge)
cv.ridge$lambda.1se
cv.ridge$lambda.min
predict(cv.ridge, s="lambda.min", type="coefficients")

mean((predict(cv.ridge, newx = newx ,s="lambda.min", type="response") - newy)^2) %>% sqrt()

mean((predict(cv.ridge, newx = newx ,s="lambda.min", type="response") - newy)^2)%>% sqrt()
## PCR
## Principal components regression (PCR)

#We fit the PCR model using the function `pcr()`.


set.seed(2)
pcr.mod <- pcr(weekly_sales~., 
               data = df_1percent[train_ind,],
               scale = TRUE, 
               validation = "CV")

summary(pcr.mod)
predplot(pcr.mod)
coefplot(pcr.mod)


# Plot the R2
validationplot(pcr.mod, val.type = "R2")
validationplot(pcr.mod, val.type="MSEP", legendpos = "topright")

predy2.pcr <- predict(pcr.mod, newdata = df_1percent[-train_ind,], 
                      ncomp = 51)
# test MSE
mean((predy2.pcr-newy)^2) %>% sqrt()



## Partial least squares (PLS)

#We fit the PLS model using the function `plsr()`.

set.seed(2)
pls.mod <- plsr(weekly_sales~., 
                data = df_1percent[train_ind,],
                scale = TRUE, 
                validation = "CV")

summary(pls.mod)
validationplot(pls.mod, val.type="MSEP", legendpos = "topright")

predy2.pls <- predict(pls.mod, newdata = df_1percent[-train_ind,], 
                      ncomp = 8)
# test MSE
mean((predy2.pls-newy)^2) %>% sqrt()



# GAM 
gam.m1 <- gam(weekly_sales ~
                store+
                is_holiday+
                temperature+
                fuel_price+
                cpi+
                unemployment+
                type+
                size+
                week_of_year+
                year+
                month_of_year+
                dept_rank, 
              data = train.data)
gam.m2 <- gam(weekly_sales ~ 
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
                dept_rank, 
              data = train.data)


anova(gam.m1, gam.m2, test = "F")
par(mfrow=c(2,4))
plot(gam.m2,se=TRUE,col = "red")


gam.pred = predict(gam.m2, newdata = test.data,type = "response")

# test MSE
mean((gam.pred-newy)^2) %>% sqrt()


#lm 
mean((predict(lm.fit,newdata = test.data ) - newy)^2) %>% sqrt()
# lasso 
mean((predict(cv.lasso, newx = newx ,s="lambda.min", type="response") - newy)^2) %>% sqrt()
#ridge
mean((predict(cv.ridge, newx = newx ,s="lambda.min", type="response") - newy)^2)%>% sqrt()
#pcr
mean((predy2.pcr-newy)^2) %>% sqrt()
#pls
mean((predy2.pls-newy)^2) %>% sqrt()
#gam
mean((gam.pred-newy)^2) %>% sqrt()

##### cross_validation for model selection 
#lm 
pred_tr_l = predict(lm.fit, newdata = train.data)
mean((pred_tr_l - y)^2) %>% sqrt()
# lasso
pred_tr_lasso = predict(cv.lasso, newx  = x, s="lambda.min", type="response") 
mean((pred_tr_lasso - y)^2) %>% sqrt()

#ridge
pred_tr_ridge = predict(cv.ridge, newx  = x, s="lambda.min", type="response") 
mean((pred_tr_ridge - y)^2) %>% sqrt()
#pcr
pred_tr_pcr = predict(pcr.mod, newdata = train.data, ncomp = 51)
mean((pred_tr_pcr - y)^2) %>% sqrt()
#pls
pred_tr_pls = predict(pls.mod, ewdata = train.data, ncomp = 8)
mean((pred_tr_pls - y)^2) %>% sqrt()
#gam
pred_tr_gam2 = predict(gam.m2, newdata = train.data, type="response")
mean((pred_tr_gam2 - y)^2)%>% sqrt()


