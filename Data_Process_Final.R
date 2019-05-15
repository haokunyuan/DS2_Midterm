# Import data 
library(tidyverse)
library(lubridate)
raw_df_train = read_csv("data/train.csv")
raw_df_stores = read_csv("data/stores.csv")
raw_df_ftrs = read_csv("data/features.csv")

# Merge data
df_merged = merge(merge(raw_df_train,raw_df_ftrs),raw_df_stores)

dim(df_merged) # 421570     16

# select store 1 
store.1 = df_merged %>% filter(Store == 1)

# change data to right format
store.1$week_of_year = week(store.1$Date)
store.1$month_of_year = month(store.1$Date)
store.1$year = year(store.1$Date)


# skim data 
skimr::skim(store.1)
# shows that Size, Store can be drop since only 1 data 
# also markdown need to be droped due to large number 50% of missing value 
store.1_short = subset(store.1, select = -c(MarkDown1,MarkDown2,MarkDown3,MarkDown4,MarkDown5,Type,Size,Store))
skimr::skim(store.1_short)


#write.csv(store.1_short,"data/final_data_one_store.csv")
