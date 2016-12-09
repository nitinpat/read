# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
training <- read.csv(file = "D://kaggle//House Prices//train.csv", header = T)
cleanTrain <- training[, colSums(is.na(training))==0]
# Itun[ , colSums(is.na(Itun)) == 0]


# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(cleanTrain[,1:61], cleanTrain[,62], sizes=c(1:61), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))


impVar <- c("GrLivArea","Neighborhood","OverallQual","TotalBsmtSF",
            "X1stFlrSF","X2ndFlrSF","BsmtFinSF1","GarageCars","GarageArea",
            "LotArea","ExterQual","YearBuilt","Fireplaces","CentralAir",
            "MSZoning","KitchenQual","MSSubClass","OverallCond",
            "FullBath","YearRemodAdd","HouseStyle", "SalePrice")

finalTrain <- cleanTrain[, impVar]
lmModel <- lm(formula = SalePrice~., data = finalTrain, na.action = na.pass)

testing <- read.csv(file = "D://kaggle//House Prices//test.csv", header = T)
impVar1 <- c("GrLivArea","Neighborhood","OverallQual","TotalBsmtSF",
            "X1stFlrSF","X2ndFlrSF","BsmtFinSF1","GarageCars","GarageArea",
            "LotArea","ExterQual","YearBuilt","Fireplaces","CentralAir",
            "MSZoning","KitchenQual","MSSubClass","OverallCond",
            "FullBath","YearRemodAdd","HouseStyle")

newTesting <- testing[, impVar1]
sum(is.na(newTesting))
# newTesting[, sapply(newTesting, is.integer)] <- -1
# newTesting[, sapply(newTesting, is.factor)] <- "unknown"
# sum(is.na(newTesting))

# library(mice)
# newtest <- mice(newTesting, m=1, maxit=10, method='cart', seed=500)
# sum(is.na(newtest))


colSums(sapply(newTesting, is.na))
newTesting$TotalBsmtSF[is.na(newTesting$TotalBsmtSF)] <- mean(newTesting$TotalBsmtSF, na.rm = T)
newTesting$GarageCars[is.na(newTesting$GarageCars)] <- mean(newTesting$GarageCars, na.rm = T)
newTesting$GarageArea[is.na(newTesting$GarageArea)] <- mean(newTesting$GarageArea, na.rm = T)

newTesting$MSZoning <- as.character(newTesting$MSZoning)
newTesting$MSZoning[is.na(newTesting$MSZoning)] <- as.character(max(newTesting$MSZoning))
newTesting$MSZoning <- as.factor(newTesting$MSZoning)

newTesting$KitchenQual <- as.character(newTesting$KitchenQual)
newTesting$KitchenQual[is.na(newTesting$KitchenQual)] <- max(newTesting$KitchenQual)
newTesting$KitchenQual <- as.factor(newTesting$KitchenQual)

pred <- data.frame(testing$Id, predict.lm(object = lmModel, newdata = newTesting))
sum(is.na(pred))