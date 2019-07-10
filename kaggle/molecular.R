library(data.table)
library(caret)

trainDT <- fread("../../downloads/molecular/train.csv")
structuresDT <- fread("../../downloads/molecular/structures.csv")
testDT <- fread("../../downloads/molecular/test.csv")
id <- testDT$id

str(trainDT)
str(structuresDT)

# merge(trainDT, structuresDT, by.x = c("molecule_name", "atom_index_0"), 
#       by.y = c("molecule_name", "atom_index"), all.x = TRUE)

# structuresDT[trainDT, on = .(molecule_name, atom_index = atom_index_0)]

trainDT[structuresDT, on = .(molecule_name, atom_index_0 = atom_index), 
        `:=`(x_0 = i.x, y_0 = i.y, z_0 = i.z)]
# trainDT[, `:=`(x = NULL, y = NULL, z =NULL)]
trainDT[structuresDT, on = .(molecule_name, atom_index_1 = atom_index), 
        `:=`(x_1 = i.x, y_1 = i.y, z_1 = i.z)]

testDT[structuresDT, on = .(molecule_name, atom_index_0 = atom_index), 
        `:=`(x_0 = i.x, y_0 = i.y, z_0 = i.z)]

testDT[structuresDT, on = .(molecule_name, atom_index_1 = atom_index), 
        `:=`(x_1 = i.x, y_1 = i.y, z_1 = i.z)]


trainDT[, `:=`(id = NULL, molecule_name = NULL)]
testDT[, `:=`(id = NULL, molecule_name = NULL)]

# tr_control <- trainControl(method = "cv", 
#                            number = 10, 
#                            verboseIter = TRUE)
# 
# begin_time <- Sys.time()
# model <- train(scalar_coupling_constant ~ ., 
#                      trainDT, 
#                      method = "xgbTree", 
#                      preProcess = c("center", "scale"), 
#                      trControl = tr_control)
# end_time <- Sys.time()
# use_time <- end_time - begin_time

model <- train(scalar_coupling_constant ~ ., 
               trainDT,
               method = "glmnet", 
               preProcess = c("center", "scale"))

pred <- predict(model, testDT)
submission <- data.table(id = id, scalar_coupling_constant = pred)
fwrite(submission, "~/downloads/my_submission.csv")
