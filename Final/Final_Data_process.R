# Import data 
library(tidyverse)
library(lubridate)
raw_df_train = read_csv("data/train.csv")
raw_df_stores = read_csv("data/stores.csv")
raw_df_ftrs = read_csv("data/features.csv")

# Merge data
df_merged = merge(merge(raw_df_train,raw_df_ftrs),raw_df_stores)
dim(df_merged) # 421570     16
head(df_merged)

# select dept1 
dept.1 = df_merged %>% filter(Dept == 1)

# change data to right format
dept.1$week_of_year = week(dept.1$Date)
dept.1$month_of_year = month(dept.1$Date)
dept.1$year = year(dept.1$Date)


# skim data 
skimr::skim(dept.1)
# shows that Size, Store can be drop since only 1 data 
# also markdown need to be droped due to large number 50% of missing value 
dept.1_short = subset(dept.1, select = -c(MarkDown1,MarkDown2,MarkDown3,MarkDown4,MarkDown5,Dept,Date))
skimr::skim(dept.1_short)


#write.csv(dept.1_short,"data/final_data_one_department.csv")
