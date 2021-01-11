library(data.table)
library(Metrics)
set.seed(5000)
traindata <- fread('project/volume/data/raw/Stat_380_train.csv')
testdata <- fread('project/volume/data/raw/Stat_380_test.csv') 

#### Data Cleaning ####
#traindata <- na.omit(traindata)

#selecting all the columns except the Id
triandata[,Id := NULL]

#factoring the traindata test.
traindata[,BldgType:= factor(BldgType)]
traindata[,Heating := factor(Heating)]
traindata[,CentralAir := factor(CentralAir)]



# taking the sample of the traindata(raw)
n = nrow(testdata)
indx = sample(1:n,n/2)
traindt <- traindata[indx]


### Varaible Selection 1 #### 
#Determing which column is good for prediction Adjusted R^2 = 0.6735
mod1 <- lm(SalePrice ~ LotFrontage + LotArea + BldgType + OverallQual + OverallCond + FullBath + HalfBath + 
             TotRmsAbvGrd + YearBuilt + TotalBsmtSF + BedroomAbvGr + Heating + GrLivArea + PoolArea + YrSold, data = traindt)

summary(mod1)


#TotRmsAbvGrd Eliminated Adjusted R^2 = 0.6736
mod2 <- lm(SalePrice ~ LotFrontage + LotArea + BldgType + OverallQual + OverallCond + FullBath + HalfBath + 
             YearBuilt + TotalBsmtSF + BedroomAbvGr + Heating + GrLivArea + PoolArea + YrSold, data = traindt)
summary(mod2)

pred_price <- predict.lm(mod2,newdata = testdata[,.(LotFrontage,LotArea,BldgType,OverallQual,OverallCond,FullBath,HalfBath,
                                                  YearBuilt,TotalBsmtSF,BedroomAbvGr,Heating,GrLivArea,PoolArea,YrSold)])

finaldata <- data.table(Id = 1:5000, SalePrice = pred_price)
finaldata[is.na(SalePrice), SalePrice := mean(traindt$SalePrice)]




### Varaible Selection 2 #### 
#Using forward selection {selecting variables with the biggest T-test}
sel1  <- lm(SalePrice ~ LotFrontage + LotArea + BldgType + OverallQual + OverallCond + FullBath + HalfBath + 
                      TotRmsAbvGrd + YearBuilt + TotalBsmtSF + BedroomAbvGr + Heating + GrLivArea + PoolArea + YrSold, data = traindt)

#1.OverallQual t-test 61.507
sel2 <- lm(SalePrice ~ LotFrontage + LotArea + BldgType + OverallCond + FullBath + HalfBath + 
             TotRmsAbvGrd + YearBuilt + TotalBsmtSF + BedroomAbvGr + Heating + GrLivArea + PoolArea + YrSold, data = traindt)

#2.GrLivArea t-test 44.482
sel3 <- lm(SalePrice ~ LotFrontage + LotArea + BldgType + OverallCond + FullBath + HalfBath + 
             TotRmsAbvGrd + YearBuilt + TotalBsmtSF + BedroomAbvGr + Heating + PoolArea + YrSold, data = traindt)

#3.TotalBsmtSF t-test 24.384
sel4 <- lm(SalePrice ~ LotFrontage + LotArea + BldgType + OverallCond + FullBath + HalfBath + 
             TotRmsAbvGrd + YearBuilt + BedroomAbvGr + Heating + PoolArea + YrSold, data = traindt)
summary(sel4)

#4.YearBuilt t-test 20.492
sel5 <- lm(SalePrice ~ LotFrontage + LotArea + BldgType + OverallCond + FullBath + HalfBath + 
             TotRmsAbvGrd + BedroomAbvGr + Heating + PoolArea + YrSold, data = traindt)
summary(sel5)

#5.LotArea t-test 7.241 
sel6 <- lm(SalePrice ~ LotFrontage + BldgType + OverallCond + FullBath + HalfBath + 
             TotRmsAbvGrd + BedroomAbvGr + Heating + PoolArea + YrSold, data = traindt)
summary(sel6)

#6. OverallCond t-test 6.820 
sel7 <- lm(SalePrice ~ LotFrontage + BldgType + FullBath + HalfBath + 
             TotRmsAbvGrd + BedroomAbvGr + Heating + PoolArea + YrSold, data = traindt)
summary(sel7)

#7.HalfBath t-test 5.409
sel8 <- lm(SalePrice ~ LotFrontage + BldgType + FullBath +
             TotRmsAbvGrd + BedroomAbvGr + Heating + PoolArea + YrSold, data = traindt)
summary(sel8)

#8.FullBath t-test 5.203
sel9 <- lm(SalePrice ~ LotFrontage + BldgType + TotRmsAbvGrd + BedroomAbvGr + Heating + PoolArea + YrSold, data = traindt)
summary(sel9)

#9.TotRmsAbvGrd t-test 1.840
sel10 <- lm(SalePrice ~ LotFrontage + BldgType + BedroomAbvGr + Heating + PoolArea + YrSold, data = traindt)
summary(sel10)
#Best predictors are (OverallQual + GrLivArea + TotalBsmtSF + YearBuilt + LotArea + OverallCond + HalfBath + FullBath + TotRmsAbvGrd)
bestpred <- lm(SalePrice ~ OverallQual + GrLivArea + TotalBsmtSF + YearBuilt + LotArea 
               + OverallCond + HalfBath + FullBath + TotRmsAbvGrd,data = traindt)
PredSale <- predict(bestpred,newdata = testdata[,.(OverallQual,GrLivArea,TotalBsmtSF,YearBuilt,LotArea, 
                                                    OverallCond,HalfBath,FullBath,TotRmsAbvGrd)])


fin <- data.table(Id = 1:5000, SalePrice = PredSale)


#### RMSE ####
rmse(traindt$SalePrice, finaldata$SalePrice) #- 28731.03765
rmse(traindt$SalePrice, fin$SalePrice) #- 28731.03765

#### SUBMISSION.CSV ####
fwrite(fin, file = "project/volume/data/processed/submission.csv") #<--- best one


