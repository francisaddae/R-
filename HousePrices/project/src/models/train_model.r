library(data.table)
library(Metrics)
library(caret)

traindata <- fread('project/volume/data/raw/Stat_380_train.csv')
testdata <- fread('project/volume/data/raw/Stat_380_test.csv') 

#### Data Cleaning ####
#traindata <- na.omit(traindata)

#selecting all the columns except the Id
traindata[,Id := NULL]

#factoring the traindata test.
traindata[,BldgType:= factor(BldgType)]
traindata[,Heating := factor(Heating)]
traindata[,CentralAir := factor(CentralAir)]
testdata[,BldgType:= factor(BldgType)]
testdata[,Heating := factor(Heating)]
testdata[,CentralAir := factor(CentralAir)]
testdata$SalePrice <- 0

#DummyVariables 
dums <- dummyVars(SalePrice ~., data = traindata)


#extact training labels
train_label  <- traindata$SalePrice
traindata <- predict(dums, newdata = traindata)
testdata  <- predict(dums, newdata = testdata)

summary(traindata)
summary(testdata)

#### XGBOOST ####
library(xgboost)

#creating an xgboost dataset 

train_matrix <- xgb.DMatrix(data = as.matrix(traindata), label = train_label, missing = NA)
test_matrix  <- xgb.DMatrix(data = as.matrix(testdata), missing = NA)

#xgboost modeling (Simple modeling) 
#with hyperparameters 
#depths at 3, 5, 10
param <- list(  objective         = 'reg:squarederror',
                booster           = 'gbtree',
                eval_metric       = 'rmse',
                tree_method       = 'hist',
                max_depth         = 3,
                eta               = 0.001,
                subsample         = 1.0,
                colsample_bytree  = 1.0,
                gamma             = 0.02,
                min_child_weight  = 1)


model1 <- xgb.cv(data = train_matrix,params = param,
                 nrounds = 70000, nfold = 5, missing = NA)
model1

#max.depth at 2  test-rmse:21724.353906+416.938046 nrounds = 30000
#max.depth at 3  test-rmse:21557.380859+253.112025 nrounds = 30000 <- best round so far 
#max.depth at 5  test-rmse:21675.546484+202.481978 nrounds = 30000
#max.depth at 3  test-rmse:21619.364453+379.169244 nrounds = 35000
#max.depth at 3  test-rmse:21561.246875+191.755067 nrounds = 35000
#max.depth at 3  test-rmse:21553.275000+317.276365 nrounds = 40000
#max.depth at 3  test-rmse:21595.921485+263.257642 nrounds = 45000
#max.depth at 3  test-rmse:21597.274609+244.803782 nrounds = 50000

bst_mod <- xgb.train(params = param, nrounds = 70000, 
                     data = train_matrix, missing = NA)

pred <- predict(bst_mod, newdata = test_matrix ) 
fin <- data.table(Id = 1:5000, SalePrice = pred)

#### SUBMISSION.CSV ####
fwrite(fin, file = "project/volume/data/processed/submission.csv") 


