# Titanic prediction using random forest
library(pacman)
p_load(tidyverse, randomForest)

train <- read_csv("~/downloads/train.csv")
test <- read_csv("~/downloads/test.csv")

# check na
map_dbl(train, ~ sum(is.na(.)))
map_dbl(test, ~ sum(is.na(.)))

# bind data
data <- bind_rows(train, test)

# fill NA
data <- data %>% 
  mutate(
    Embarked = replace_na(Embarked, "S"), 
    Fare = replace_na(Fare, median(Fare, na.rm = TRUE)), 
    Age = replace_na(Age, median(Age, na.rm = TRUE))
  ) %>% 
  mutate_at(vars(Pclass, Sex, Embarked), ~ as.factor(.)) %>% 
  select(-c("PassengerId", "Name", "Cabin", "Ticket"))

# new train and test
train_new <- data[1:891,] %>% 
  mutate(Survived = as.factor(Survived))
test_new <- data[892:1309,] %>% 
  select(-Survived)

# build model
model <- randomForest(Survived ~ ., train_new, )
pred <- predict(model, test_new)
submission <- tibble(PassengerId = test$PassengerId, Survived = pred)
write_csv(submission, "~/downloads/submission.csv")
