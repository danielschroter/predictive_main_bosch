library(data.table)
library(Matrix)
library(caret)
library(xgboost)


dt <- fread("case4_train_numeric.csv",header= TRUE)
set.seed(123)
dt <- dt[sample(nrow(dt), 200000),]
Y  <- dt$Response
dt[ , Response := NULL]

for(col in names(dt)) set(dt, j = col, value = dt[[col]] + 2)
for(col in names(dt)) set(dt, which(is.na(dt[[col]])), col, 0)

#Matrix with sparse = T reduces the storage needed
dt[1:5, 1:5]
X <- Matrix(as.matrix(dt), sparse = T)
rm(dt)

folds <- createFolds(as.factor(Y), k = 6)
valid <- folds$Fold1
model <- c(1:length(Y))[-valid]

#Param for XGBoost, learning rate 0.01, base score (default 0.5) as we have fifty damaged dataset,
param <- list(objective = "binary:logistic",
              eval_metric = "auc",
              eta = 0.01,
              base_score = 0.005,
              col_sample = 0.5) 

#Transformations for Model application
dmodel <- xgb.DMatrix(X[model,], label = Y[model])
dvalid <- xgb.DMatrix(X[valid,], label = Y[valid])


m1 <- xgb.train(data = dmodel, param, nrounds = 20,
                watchlist = list(mod = dmodel, val = dvalid), verbose = 1)


imp <- xgb.importance(model = m1, feature_names = colnames(X))

cols <- imp$Feature
imp[1:10]
length(cols)

head(cols, 10)

#Entfernt alle Variablen außer Cols
rm(list = setdiff(ls(), "cols"))

###Apply xgboost on good features

#Only read the detected important cols from the total dataset
dt <- fread("case4_train_numeric.csv",
            select = c(cols, "Response"),
            showProgress = T)

Y  <- dt$Response
dt[ , Response := NULL]

for(col in names(dt)) set(dt, j = col, value = dt[[col]] + 2)
for(col in names(dt)) set(dt, which(is.na(dt[[col]])), col, 0)

X <- Matrix(as.matrix(dt), sparse = T)
rm(dt)


#Apply XGBoost

set.seed(7579)
folds <- createFolds(as.factor(Y), k = 6)
valid <- folds$Fold3
model <- c(1:length(Y))[-valid]

param <- list(objective = "binary:logistic",
              eval_metric = "auc",
              eta = 0.01,
              max_depth = 2,
              colsample_bytree = 0.5,
              base_score = 0.005) 

dmodel <- xgb.DMatrix(X[model,], label = Y[model])
dvalid <- xgb.DMatrix(X[valid,], label = Y[valid])


m1 <- xgb.train(data = dmodel, param, nrounds = 30,
                watchlist = list(mod = dmodel, val = dvalid))

pred <- predict(m1, dvalid)
summary(pred)

imp <- xgb.importance(model = m1, feature_names = colnames(X))

head(imp, 30)


## Select threshold to optimize according to MCC

mc <- function(actual, predicted) {
  
  tp <- as.numeric(sum(actual == 1 & predicted == 1))
  tn <- as.numeric(sum(actual == 0 & predicted == 0))
  fp <- as.numeric(sum(actual == 0 & predicted == 1))
  fn <- as.numeric(sum(actual == 1 & predicted == 0))
  
  numer <- (tp * tn) - (fp * fn)
  denom <- ((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)) ^ 0.5
  
  numer / denom
}

matt <- data.table(thresh = seq(0.0, 0.998, by = 0.001))

matt$scores <- sapply(matt$thresh, FUN =
                        function(x) mc(Y[valid], (pred > x) * 1))


opt <- matt[which.max(matt$scores), ]
print(opt)
pred_bin <- ifelse((pred > opt$thresh), 1, 0)
table(Y[valid], pred_bin)
mc(Y[valid],pred_bin)

###Apply Random Forest in Contrast

sum(data$Response)

trainSize <- round(nrow(data)*0.7)


#random selection of training and test data
set.seed(123)
training_indices <- sample(seq_len(nrow(data)), size = trainSize)
trainData <- data[training_indices,]
testData <- data[-training_indices,]


sum(testData$Response)
#Drop Response column of test Data
Response_test <-testData$Response
testData$Response <- NULL

trainData$Response <- as.factor(trainData$Response)


library(randomForest)
model <- randomForest(formula = Response~., ntree=400, data = trainData, importance = T, do.trace = T)
#model <- rpart(formula = Response~., data = trainData, method = "class")

varImp(model)

#Make Predictions and clalulate mcc
predictions <- predict(model, newdata = testData)
predictions <- as.numeric(predictions)
predictions[predictions == 1] <- 0
predictions[predictions == 2] <- 1

table(predictions, Response_test)
mcc(predictions,Response_test)


