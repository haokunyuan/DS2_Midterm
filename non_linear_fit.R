library(caret)
library(splines)
library(mgcv)
library(tidyverse)
library(ggplot2)

df = read_csv("data/merged_data_2.csv") %>% janitor::clean_names()

# names(df)
# 
# df_cleaned = df[c("weekly_sales","is_holiday","dept","store",
#                   "temperature","year","month_of_year","week_of_year")]
# names(df_cleaned)

names(df)
df_cleaned = df[c(-1,-9:-13)]
df_cleaned$dept = as.factor(df_cleaned$dept)
df_cleaned$type = as.factor(df_cleaned$type)
names(df_cleaned)
head(df_cleaned)
# df_cleaned$month_of_year = as.factor(df_cleaned$month_of_year)
# df_cleaned$year = as.factor(df_cleaned$year)
 # df_cleaned$Dept = as.factor(df_cleaned$Dept)
#df_cleaned$week_of_year = as.factor(df_cleaned$week_of_year)

x <- model.matrix(weekly_sales~.,df_cleaned)[,-1]
y <- df_cleaned$weekly_sales

theme1 <- trellis.par.get()
theme1$plot.symbol$col <- rgb(.2, .4, .2, .5)
theme1$plot.symbol$pch <- 16
theme1$plot.line$col <- rgb(.8, .1, .1, 1)
theme1$plot.line$lwd <- 2
theme1$strip.background$col <- rgb(.0, .2, .6, .2)
trellis.par.set(theme1)

dim(df_cleaned)
df_cleaned_small = sample_frac(df_cleaned,0.1)


featurePlot(x = df_cleaned_small[-1],
            y = df_cleaned_small$weekly_sales,
            plot = "scatter",
            span = .5,
            labels = c("Predictors","Compressive Strength"),
            type = c("p", "smooth"), # use this include a smooth line layout = c(4, 2))
            layout = c(4, 2))

df_cleaned_small %>% 
  group_by(year,month_of_year) %>% 
  summarise(monthly_sales = mean(weekly_sales)) %>% 
  mutate(sequence=row_number()) %>% 
  ggplot() +
  geom_point(aes(x = sequence, y = monthly_sales)) +
  geom_line(aes(x = sequence, y = monthly_sales,group = year,color = as.factor(year)))



fit1 <- lm(weekly_sales~., data = df_cleaned)
# not numeric 
#fit2 <- lm(Weekly_Sales~poly(month_of_year,2), data = df_cleaned) 
summary(fit1)

##smooth spline 


fit.ss <- smooth.spline(df_cleaned$month_of_year, df_cleaned$weekly_sales)
fit.ss$df
plot(fit.ss)

summary(fit.ss)
names(df_cleaned)



# seperate test and train 
smp_size <- floor(0.75 * nrow(df_cleaned))
set.seed(123)
train_ind <- sample(seq_len(nrow(df_cleaned)), size = smp_size)

train <- df_cleaned[train_ind, ]
test <- df_cleaned[-train_ind, ]


# gam.m2 <- gam(weekly_sales~
#                 s(store)+
#                 is_holiday+
#                 s(dept)+
#                 s(month_of_year)+
#                 s(temperature)+
#                 s(year)+
#                 s(week_of_year), 
#               data = df_cleaned)
names(df_cleaned)

gam.m2 <- gam(weekly_sales~
                store+
                is_holiday+
                dept+
                s(month_of_year)+
                s(week_of_year)+
                s(fuel_price)+
                s(cpi)+
                s(temperature)+
                unemployment+
                type+
                s(size)+
                year+
                s(week_of_year), 
              data = train,
              select = TRUE)

par(mfrow = c(2,3))
plot(gam.m2, se = TRUE, col = "red")
#dev.off()
anova(gam.m2)
summary(gam.m2)


#TODO make few GAM and do 
# anova(gam.m1,gam.m2, test="F")

foo = predict(gam.m2,test) 
sqrt(mean((foo - test$weekly_sales)^2))  


# lasso 

library(tidyverse)
library(corrplot)
library(caret)
library(glmnet)

x = model.matrix(weekly_sales~., train)[,-1]
y = train$weekly_sales

set.seed(2)
ctrl1 = trainControl(method = "repeatedcv", number = 10, repeats = 5)
lasso.fit = train(x, y,
                  method = "glmnet",
                  tuneGrid = expand.grid(alpha = 1,
                                         lambda = exp(seq(-1,10, length = 100))),
                  trControl = ctrl1)

plot(lasso.fit)
lasso.fit$bestTune

set.seed(2)
lasso <- train(
  weekly_sales ~., data = train, method = "glmnet",
  trControl = ctrl1,
  tuneGrid = expand.grid(alpha = 1, 
                         lambda = 10^seq(-10, 10, length = 100))
)
plot(lasso, xTrans = function(x)log(x))
lasso$bestTune
coef(lasso$finalModel, lasso$bestTune$lambda)
predictions <- lasso %>% predict(test)

data.frame(
  RMSE = RMSE(predictions, test$weekly_sales),
  Rsquare = R2(predictions, test$weekly_sales)
)



plot(lasso.fit, xTrans = function(x)log(x))
coef(lasso.fit$finalModel, ridge.fit$bestTune$lambda)
