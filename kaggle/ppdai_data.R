library(tidyverse)
library(data.table)
library(lubridate)
# list files
list.files("~/downloads/data")

# load train and test
train <- fread("~/downloads/data/train.csv")
test <- fread("~/downloads/data/test.csv")

# is due_amt equal to repay_amt
sum(train$due_amt == train$repay_amt)
length(unique(train$user_id))

# other information
listing_info <- fread("~/downloads/data/listing_info.csv")
user_info <- fread("~/downloads/data/user_info.csv")
user_taglist <- fread("~/downloads/data/user_taglist.csv")
user_behavior_logs <- fread("~/downloads/data/user_behavior_logs.csv")

# look at user_info
# choose the newest information
str(user_info)
user_info_d <- user_info %>% 
  mutate(insertdate = ymd(insertdate)) %>% 
  group_by(user_id) %>% 
  filter(insertdate == max(insertdate))
# fwrite(user_info_d, "~/downloads/ppdai/user_info_d.csv")

# user taglist
length(unique(user_taglist$user_id))
user_taglist_d <- user_taglist %>% 
  mutate(insertdate = ymd(insertdate)) %>% 
  group_by(user_id) %>% 
  filter(insertdate == max(insertdate))

# join test data
test1 <- test %>% 
  left_join(listing_info, by = c("listing_id", "user_id")) %>% 
  rename(auditing_date = auditing_date.x) %>% 
  select(-auditing_date.y)
test2 <- test1 %>% 
  left_join(user_info_d, by = "user_id") %>% 
  left_join(user_taglist_d, by = "user_id")
test2 <- test2 %>% 
  select(-starts_with("insert"))
# fwrite(test2, "~/downloads/ppdai/test_new.csv")

# join train data
train1 <- train %>% 
  left_join(listing_info, by = c("listing_id", "user_id")) %>% 
  rename(auditing_date = auditing_date.x) %>% 
  select(-auditing_date.y)
train2 <- train1 %>% 
  left_join(user_info_d, by = "user_id") %>% 
  left_join(user_taglist_d, by = "user_id") %>% 
  select(-starts_with("insert"))
# fwrite(train2, "~/downloads/ppdai/train_new.csv")

# logs data
user_behavior_logs <- fread("~/downloads/data/user_behavior_logs.csv")

behavior_count <- user_behavior_logs %>% 
  group_by(user_id, behavior_type) %>% 
  summarise(behavior_count = n())

user_behavior <- behavior_count %>% 
  spread(behavior_type, behavior_count)
# fwrite(user_behavior, "~/downloads/ppdai/user_behavor.csv")

# repay data
user_repay_logs <- fread("~/downloads/data/user_repay_logs.csv")

# new train and test
train_new <- fread("~/downloads/ppdai/train_new.csv")
test_new <- fread("~/downloads/ppdai/test_new.csv")

train_final <- train_new %>% 
  left_join(user_behavior, by = "user_id")
fwrite(train_final, "~/downloads/ppdai/train_new.csv")

test_final <- test_new %>% 
  left_join(user_behavior, by = "user_id")
fwrite(test_final, "~/downloads/ppdai/test_new.csv")
