# load required packages
library(pacman)
p_load(tidyverse, readxl, xgboost, caret)

# load index file
index <- read_xlsx("happiness/happiness_index.xlsx")

# import train and test data
train_complete <- read_csv("happiness/happiness_train_complete.csv")
test_complete <- read_csv("happiness/happiness_test_complete.csv")

# clean data
data_all <- train_complete %>% 
  # bind train and test set
  bind_rows(test_complete) %>% 
  # drop `_other` columns
  select(-ends_with("_other")) %>% 
  # replace minus values with NA
  mutate_each(~ replace(., . < 0, NA)) %>% 
  # fill happiness NA values with median
  mutate_at(vars(happiness), ~ replace_na(., median(., na.rm = TRUE)))

# check NA values
have_na <- map_df(data_all, ~ sum(is.na(.))) %>% 
  gather() %>% 
  filter(value > 0) %>% 
  arrange(desc(value))

# look at variables
ggplot(data_all, aes(marital)) + 
  geom_bar()
ggplot(data_all, aes(minor_child)) + 
  geom_bar()

# generate new variable and fill NA values
data_all <- data_all %>% 
  mutate(
    # count age
    age = 2015 - birth, 
    property = property_1, 
    nationality = replace_na(nationality, 1), 
    religion = replace_na(religion, 0), 
    edu = replace_na(edu, 6), 
    edu_status = replace_na(edu_status, 4), 
    political = replace_na(political, 1), 
    media = rowMeans(select(., starts_with("media_")), na.rm = TRUE), 
    leisure = rowMeans(select(., starts_with("leisure_")), na.rm = TRUE), 
    insur_1 = replace_na(insur_1, 1), 
    insur_2 = replace_na(insur_2, 1), 
    insur_3 = replace_na(insur_3, 2), 
    insur_4 = replace_na(insur_4, 2), 
    house = replace_na(house, 1), 
    car = replace_na(car, 1), 
    m_income = replace_na(s_income, 0), 
    children = rowSums(select(., c("son", "daughter")), na.rm = TRUE), 
    invest = invest_1, 
    minor_child = replace_na(minor_child, 0), 
    trust = rowMeans(select(., starts_with("trust_")), na.rm = TRUE), 
    public = rowMeans(select(., starts_with("public_")), na.rm = TRUE), 
  ) %>% 
  mutate_each(~ replace_na(., median(., na.rm = TRUE))) %>% 
  select(-c(
    "id", "survey_time", "birth", "county", 
    "religion_freq", "edu_yr", "join_party", 
    "hukou_loc", "son", "daughter", "inc_exp"
    ), 
    -starts_with("property_"), -starts_with("media_"), 
    -starts_with("leisure_"), -starts_with("social_"), 
    -starts_with("work_"), -starts_with("invest_"), 
    -starts_with("marital_"), -starts_with("s_"), 
    -starts_with("f_"), -starts_with("m_"), 
    -starts_with("trust_"), -starts_with("public_")
  )

# check NA values again
map_dbl(data_all, ~ sum(is.na(.)))

# convert some variables to factor
# data_all <- data_all %>% 
#   mutate_at(names(select(., -c("income", "floor_area", "height_cm", "weight_jin", 
#                         "family_income", "family_m", "house", "car", "minor_child", 
#                         "age", "media", "leisure", "children", "trust", "public"))), 
#             ~ as.factor(.))

# split data into train and test
train_data <- data_all[1:8000, ]
test_data <- data_all[8001:10968, ] %>% 
  select(-happiness)

# set train control
tr_control <- trainControl(method = "cv", 
                           number = 10, 
                           verboseIter = TRUE)

# tune grid
# tune_grid <- expand.grid(alpha = 0:1, 
#                          lambda = seq(0.0001, 1, length = 100))

# build glmnet model
# happy_model <- train(happiness ~ ., 
#                      train_data, 
#                      method = "glmnet", 
#                      preProcess = c("center", "scale"), 
#                      trControl = tr_control, 
#                      tuneGrid = tune_grid)

# tune_grid <- expand.grid(nrounds=c(100,200,300,400), 
#                          max_depth = c(3:10),
#                          eta = c(0.05, 1),
#                          gamma = c(0.01),
#                          colsample_bytree = c(0.75),
#                          subsample = c(0.50),
#                          min_child_weight = c(0))

tune_grid <- expand.grid(nrounds=200, 
                         max_depth = 4,
                         eta = 0.05,
                         gamma = 0.01,
                         colsample_bytree = 0.75,
                         subsample = 0.50,
                         min_child_weight = 0)

# xgboost
begin_time <- Sys.time()
happy_model <- train(happiness ~ ., 
                     train_data, 
                     method = "xgbTree", 
                     preProcess = c("center", "scale"), 
                     trControl = tr_control, 
                     tuneGrid = tune_grid)
end_time <- Sys.time()
use_time <- end_time - begin_time

# prediction based on glmnet model
pred <- predict(happy_model, test_data)
submit0617 <- tibble(id = test_complete$id, happiness = pred)
write_csv(submit0617, "happiness/submit0617.csv")
