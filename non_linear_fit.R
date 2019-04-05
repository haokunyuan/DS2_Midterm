library(caret) # only for plot
library(splines)
library(mgcv)
library(tidyverse)
library(ggplot2)

df = read_csv("data/merged_data_2.csv") 

df_cleaned = df[c("Store","IsHoliday","Dept","Weekly_Sales",
                  "Temperature","year","month_of_year","week_of_year")]
names(df_cleaned)

df_cleaned$month_of_year = as.factor(df_cleaned$month_of_year)
df_cleaned$year = as.factor(df_cleaned$year)
#df_cleaned$Dept = as.factor(df_cleaned$Dept)
#df_cleaned$week_of_year = as.factor(df_cleaned$week_of_year)

x <- model.matrix(Weekly_Sales~.,df_cleaned)[,-1]
y <- df_cleaned$Weekly_Sales

# theme1 <- trellis.par.get() 
# theme1$plot.symbol$col <- rgb(.2, .4, .2, .5) 
# theme1$plot.symbol$pch <- 16
# theme1$plot.line$col <- rgb(.8, .1, .1, 1) 
# theme1$plot.line$lwd <- 2 
# theme1$strip.background$col <- rgb(.0, .2, .6, .2) 
# trellis.par.set(theme1)

fit1 <- lm(Weekly_Sales~., data = df_cleaned)

summary(fit1)
