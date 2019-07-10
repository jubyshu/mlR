library(pacman)
p_load(tidyverse, readxl, caret, nnet, glmnet)

# load index file
index <- read_xlsx("happiness/happiness_index.xlsx")

# load train and test complete file and bind them
train_complete <- read_csv("happiness/happiness_train_complete.csv")
test_complete <- read_csv("happiness/happiness_test_complete.csv")
test_complete$happiness <- 0
data_all <- bind_rows(train_complete, test_complete) %>% 
  # replace minus values with NA
  mutate_each(~ replace(., . < 0, NA))

have_na <- map_dbl(data_all, ~ sum(is.na(.)))
have_na[have_na > 0]

# -- useful columns --
# survey_type, province, city, gender, birth, nationality, religion, 
# religion_freq, edu, edu_status, income, political, floor_area, *property, 
# height_cm, weight_jin, health, heath_problem, depression, hukou, *media, 
# *leisure, *social, equity, class, work_exper, *insur, family_income, family_m,
# house, car, *invest, *child, marital, status_peer, view, inc_ability, 
# *trust, *public

# merge and create variables  
data_complete <- data_all %>% 
  mutate(age = 2015 - birth,
         property = property_1, 
         media = rowMeans(select(., starts_with("media_")), na.rm = TRUE), 
         leisure = rowMeans(select(., starts_with("leisure_")), na.rm = TRUE), 
         child = son + daughter, 
         trust = rowMeans(select(., starts_with("trust_")), na.rm = TRUE), 
         public = rowMeans(select(., starts_with("public_")), na.rm = TRUE)
  )

use_cols <- c("survey_type", "province", "gender", "age", "religion", "edu", "income", "political", 
              "floor_area", "property", "health", "height_cm", "weight_jin", "depression", "media", 
              "socialize", "equity", "class", "work_exper", "family_income", "family_m", "house", 
              "car", "child", "marital", "status_peer", "view", "trust", "public", "happiness")
factor_cols <- c("survey_type", "province", "gender", "religion", "edu", "political", "property", 
                 "health", "depression", "socialize", "equity", "class", "work_exper", "car", 
                 "marital", "status_peer", "view", "happiness")

test_factor <- c("survey_type", "province", "gender", "religion", "edu", "political", "property", 
                 "health", "depression", "socialize", "equity", "class", "work_exper", "car", 
                 "marital", "status_peer", "view")

data_abbr <- data_complete %>% 
  select(use_cols)
  

have_na <- map_dbl(data_abbr, ~ sum(is.na(.)))
have_na[have_na > 0]

# missing values fill


# split train and test
train_data <- data_abbr[1:8000, ]
test_data <- data_abbr[8001:10968, ]
test_data$happiness <- NULL

train_data <- train_data %>% 
  mutate_each(~ replace_na(., median(., na.rm = TRUE))) %>% 
  mutate_at(factor_cols, as.factor)

test_data <- test_data %>% 
  mutate_each(~ replace_na(., median(., na.rm = TRUE))) %>% 
  mutate_at(test_factor, as.factor)

# build model
model <- train(
  happiness ~ ., 
  train_data, 
  method = "glmnet", 
  preProcess = c("center", "scale"), 
  trControl = trainControl(
    method = "repeatedcv",
    number = 6,
    repeats = 5, # 20 is too high
    verboseIter = TRUE
  )
)

pred <- predict(model, test_data)
submit <- tibble(id = data_all[8001:10968, ]$id, happiness = pred)
write_csv(submit, "happiness/submit0611.csv")
