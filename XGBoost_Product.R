library(data.table)
library(Matrix)
library(caret)
library(xgboost)


#Read the data
data <- fread("product.csv", header = TRUE, data.table = FALSE)

#Drop id
data <- data[2:198]

dt<-as.data.table(data)

Y  <- dt$Response
dt[ , Response := NULL]

X <- Matrix(as.matrix(dt), sparse = T)

#train and testData Set size
trainSize <- round(nrow(data)*0.7)

#random selection of training and test data
set.seed(123)
training_indices <- sample(seq_len(nrow(data)), size = trainSize)

dmodel <- xgb.DMatrix(X[training_indices,], label = Y[training_indices])
dvalid <- xgb.DMatrix(X[-training_indices,], label = Y[-training_indices])


param <- list(objective = "binary:logistic",
              eval_metric = "auc",
              eta = 0.01,
              max_depth = 2,
              colsample_bytree = 0.5,
              base_score = 0.005) 



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
                        function(x) mc(Y[-training_indices], (pred > x) * 1))


opt <- matt[which.max(matt$scores), ]
print(opt)
pred_bin <- ifelse((pred > opt$thresh), 1, 0)
table(pred_bin, Y[-training_indices])
mc(Y[-training_indices],pred_bin)
