library(pacman)
p_load(class, caret, e1071, tidyverse)

# load data
heart <- read_csv("heart.csv")

# normalize
normalize <- function(x) {
  x <- (x - min(x)) / (max(x) - min(x))
}

cols_norm <- c("age", "trestbps", "chol", "thalach", "oldpeak")
heart_norm <- heart[, cols_norm] %>% 
  map(normalize) %>% 
  as_tibble() %>% 
  bind_cols(select(heart, -cols_norm)) %>% 
  mutate_each(as.factor, -cols_norm)

# split train and test
# select 90% data as train set
set.seed(10)
sp <- sample(1:nrow(heart), round(0.9 * nrow(heart)))
train <- heart_norm[sp, ]
test <- heart_norm[-sp, ]

# build knn model
heart_knn <- knn(train, test, cl = train$target, k = 10)

# make confusion matrix
conf <- table(heart_knn, test$target)

# calculate accuracy
acc <- sum(diag(conf)) / sum(conf)

# cross validation
cv_control <- trainControl(method = "cv", number = 5)

heart_knn_cv <- train(target ~ ., method = "knn", tuneGrid = expand.grid(k = 1:20), 
                      trControl = cv_control, metric = "Accuracy", data = heart_norm)
heart_knn_cv

