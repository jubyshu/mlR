# load packages
library(pacman)
p_load(rpart, rpart.plot, RColorBrewer, tidyverse)

# load dataset
train <- read_csv("titanic/train.csv")
test <- read_csv("titanic/test.csv")
test$Survived <- 0

# cleaning data
combi <- bind_rows(train, test)
# passenger 514's name will partly discarded
combi <- combi %>% separate(Name, into = c("Name_first", "Title", "Name_last"), 
                   sep = "[,.]", remove = FALSE) %>% 
  select(-c("Name_first", "Name_last")) %>% 
  mutate(
    Title = trimws(Title), 
    Title = case_when(
      PassengerId == 797 ~ "Mrs", 
      Sex == "female" & Title %in% c("Dona", "Mrs") ~ "Mrs", 
      Sex == "female" & !(Title %in% c("Dona", "Mrs")) ~ "Miss", 
      Sex == "male" ~ "Mr"), 
    Title = as.factor(Title), 
    # passenger 62 and 830 do not have embarked value
    Embarked = ifelse(is.na(Embarked), "S", Embarked), 
    Embarked = as.factor(Embarked), 
    # replace Fare NA value using median on row 1044
    Fare = ifelse(is.na(Fare), median(Fare, na.rm = TRUE), Fare),
    # add a new column family_size
    family_size = SibSp + Parch + 1)

# fill in missing Age values
# using a dt model and giving method = "anova" for continuous variables
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size,
                       data = combi[!is.na(combi$Age), ], method = "anova")
combi$Age[is.na(combi$Age)] <- predict(predicted_age, combi[is.na(combi$Age), ])

# split train  and test
train_new <- combi[1:891, ]
test_new <- combi[892:1309, ]
test_new$Survived <- NULL

# build dt model
survived_tree <- rpart(Survived ~ Age + Sex + Pclass + family_size, data = train_new, 
                       method = "class", control = rpart.control(cp = 0.0001))
# plot tree
fancyRpartPlot(survived_tree, caption = "Who will survived?")

# make prediction
survived_prediction <- predict(survived_tree, test_new, type = "class")
head(survived_prediction)

# new tibble for predcition
prediction_output <- tibble(PassengerId = test_new$PassengerId, Survived = survived_prediction)

# how to evaluate dt model
# confusion matrix: accuracy, precision, recall
# ROC curve: library(ROCR)