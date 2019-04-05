# Import data 
library(tidyverse)
library(lubridate)
raw_df_train = read_csv("data/train.csv")
raw_df_stores = read_csv("data/stores.csv")
raw_df_ftrs = read_csv("data/features.csv")

# Merge data
df_merged = merge(merge(raw_df_train,raw_df_ftrs),raw_df_stores)

## change markdown to either have or don't have markdown 
df_merged$MarkDown1 = ifelse(is.na(df_merged$MarkDown1),0,1)
df_merged$MarkDown2 = ifelse(is.na(df_merged$MarkDown2),0,1)
df_merged$MarkDown3 = ifelse(is.na(df_merged$MarkDown3),0,1)
df_merged$MarkDown4 = ifelse(is.na(df_merged$MarkDown4),0,1)
df_merged$MarkDown5 = ifelse(is.na(df_merged$MarkDown5),0,1)
head(df_merged)
table(df_merged$MarkDown1)

str(df_merged$Date)



df_merged$week_of_year = week(df_merged$Date)
df_merged$month_of_year = month(df_merged$Date)
df_merged$year = year(df_merged$Date)

# tail(df_merged,10)
# hist(df_merged$week_of_year)

#write.csv(df_merged,"data/merged_data_1.csv")

head(df_merged)
skimr::skim(df_merged)


# how many years 
unique(df_merged$year)  # 3 unique years 
#QC if month is corerct 
hist(df_merged$month)  


# missing data for 2012 # nov and dec 
ggplot(df_merged) +
  geom_histogram(aes(x = month_of_year, fill = as.factor(year)))



df_merged %>% 
  group_by(year,month_of_year) %>% 
  summarise(n = n()) %>% 
  ggplot() +
  geom_smooth(aes(x = month_of_year, y =n ,color = as.factor(year)),se=FALSE)



