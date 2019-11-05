library(magrittr)
library(data.table)
library(ggplot2)
library(caret)

# load data set
train <- fread("~/downloads/train.csv")
test <- fread("~/downloads/test.csv")

# sale price of train data
sale_price <- train$SalePrice
test_id <- test$Id
# bind data
data_all <- rbind(train, test, fill = TRUE)

# check missing values
na <- lapply(data_all, function(x) sum(is.na(x))) %>% 
  setDT() %>% melt(variable.factor = FALSE)
na[value != 0]

# remove columns containing too many NAs
data_all[, na[value > 1000, variable] := NULL]

# fill NAs
fill_char_na <- function(dt) {
  for (col in names(dt)) {
    if (typeof(dt[, get(col)]) == "character") {
      dt[is.na(get(col)), (col) := names(dt[, which.max(table(get(col)))])]
    }
    if (typeof(dt[, get(col)]) == "integer") {
      dt[is.na(get(col)), (col) := dt[, median(get(col), na.rm = TRUE)]]
    }
  }
}
fill_char_na(data_all)

# split data
data_all[, Id := NULL]
train_new <- data_all[1:1460][, SalePrice := sale_price]
test_new <- data_all[1461:2919]

# build model
tr_control <- trainControl(method = "repeatedcv", 
                           number = 10, 
                           repeats = 5, 
                           verboseIter = TRUE)

tune_grid <- expand.grid(nrounds=c(100,200,300,400),
                         max_depth = c(3:10),
                         eta = c(0.05, 1),
                         gamma = c(0.01),
                         colsample_bytree = c(0.75),
                         subsample = c(0.50),
                         min_child_weight = c(0))

model <- train(SalePrice ~ ., 
               train_new, 
               method = "xgbTree", 
               preProcess = c("center", "scale"), 
               trControl = tr_control, 
               tuneGrid = tune_grid)

pred <- predict(model, test_new)
submission <- data.table(Id = test_id, SalePrice = pred)
fwrite(submission, "~/downloads/my_submission.csv")
