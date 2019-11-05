library(tidyverse)
library(caret)
library(rpart)

# load data
train <- read_csv("~/downloads/train.csv")
test <- read_csv("~/downloads/test.csv")
data_all <- bind_rows(train, test)

# check NA
map_dbl(data_all, ~ sum(is.na(.)))

# Fare is NA
data_all[which(is.na(data_all$Fare)), ]

# median and mean of Fare grouped by Pclass
data_all %>% 
  group_by(Pclass) %>% 
  summarise(fare_mean = mean(Fare, na.rm = TRUE),
            fare_median = median(Fare, na.rm = TRUE))

# fill NA
data <- data_all %>% 
  mutate(Fare = replace_na(Fare, 8.05), # median=8.05, mean=13.3
         Embarked = replace_na(Embarked, "S"), 
         Family = SibSp + Parch + 1)

# substract Title from names
data <- data %>% 
  separate(Name, into = c("f_name", "Title", "l_name"), 
           sep = "[,.]", remove = FALSE) %>% 
  mutate(Title = trimws(Title)) %>% 
  select(-f_name, -l_name)

table(data$Title)

mr <- c("Capt", "Col", "Don", "Dr", "Jonkheer", "Major", 
        "Master", "Mr", "Rev", "Sir", "the Countess")
mrs <- c("Dona", "Mme", "Mrs", "Ms")
miss <- c("Lady", "Mlle", "Miss")

data <- data %>% 
  mutate(Title = case_when(
    PassengerId == 797 ~ "Mrs", 
    Title %in% mr ~ "Mr", 
    Title %in% mrs ~ "Mrs", 
    Title %in% miss ~ "Miss"
  ))
data[797, ]

# predict Age
age_train <- data %>% 
  filter(!is.na(Age))
age_test <- data %>% 
  filter(is.na(Age))

age_model <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title, 
                   age_train, 
                   method = "anova")
age_pred <- predict(age_model, age_test)
age_test$Age <- age_pred

data <- bind_rows(age_train, age_test) %>% 
  arrange(PassengerId)

# split train and test
train_new <- data[1:891, ] %>% 
  mutate(Survived = as.factor(Survived))
test_new <- data[892:1309, ] %>% 
  select(-Survived)

# build model
model <- train(Survived ~ Pclass + Sex + SibSp + Parch + Family + Fare + Embarked + Title, 
               train_new, 
               method = "rf", 
               trControl = trainControl(
                 method = "cv", 
                 number = 9
               ))
               
# make prediction
pred <- predict(model, test_new)
submission <- tibble(PassengerId = test_new$PassengerId, 
                     Survived = pred)
write_csv(submission, "~/downloads/my_submission6.csv")
