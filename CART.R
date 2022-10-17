library(data.table)
library(caret)
library(mltools)
library(factoextra)
library(plyr)
library(rpart)
library(rpart.plot)

#Read the data
data <- fread("product.csv", header = TRUE, data.table = FALSE)

#Drop id
data <- data[2:198]
#data <- data[,c("L1_S24_F1723", "L1_S24_F1846", "L1_S24_F1695", "L1_S24_F1758", "L2_S26_F3077", "Response")]

#trainSize size
trainSize <- round(nrow(data)*0.7)

#random selection of training and test data
set.seed(123)
training_indices <- sample(seq_len(nrow(data)), size = trainSize)
trainData <- data[training_indices,]
testData <- data[-training_indices,]

#Drop Response column of test Data
Response_test <-testData$Response
testData$Response <- NULL

#Apply PCA

pca_data <- trainData
Response <- trainData$Response
pca_data$Response <- NULL

#Apply PCA first,
pca <- prcomp(pca_data, scale = TRUE)

#Explore output of PCA
fviz_eig(pca)

#Calculate predicted variances
pr_var = (pca$sdev)^2
pro_var_ex = pr_var/sum(pr_var)

#Plot predicted variance for each component
plot(pro_var_ex, xlim=c(0,60), type = "b")
plot(cumsum(pro_var_ex), xlim = c(0,60), ylab = "Cumulated explained variance", xlab = "Principal Components")
cumsum(pro_var_ex)

#create the dataframes with the principal components
trainData$Response <- NULL
trainData <- data.frame(Response = Response, pca$x)
testData <- as.data.frame(predict(pca, newdata = testData))
rm(pca_data)

#Only take 106 features, to cover 99% of explained varaince
trainData <- trainData[,1:107]
testData <- testData[,1:106]



###### Apply Decision Tree

#Transform the Response variable into a vector
trainData$Response <- factor(trainData$Response)

#Define our metrics which should be optimized, here Matthew correlation coefficient
mccSummary <- function (data, lev = NULL, model = NULL){
  
  tp <- as.numeric(sum(data$obs == 1 & data$pred == 1))
  tn <- as.numeric(sum(data$obs == 0 & data$pred == 0))
  fp <- as.numeric(sum(data$obs == 0 & data$pred == 1))
  fn <- as.numeric(sum(data$obs == 1 & data$pred == 0))
  
  numer <- (tp * tn) - (fp * fn)
  denom <- ((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)) ^ 0.5
  out <- numer/denom
  names(out) <-"mcc"
  out
}

#Define False negative rate
fnrSummary <- function (data, lev = NULL, model = NULL){
  
  tp <- as.numeric(sum(data$obs == 1 & data$pred == 1))
  tn <- as.numeric(sum(data$obs == 0 & data$pred == 0))
  fp <- as.numeric(sum(data$obs == 0 & data$pred == 1))
  fn <- as.numeric(sum(data$obs == 1 & data$pred == 0))
  
  out <- fn/(tp+tn+fp+fn)
  names(out) <-"fnr"
  out
}


#For ROC in twoClassSummary level names should not be 0 1
levels(trainData$Response)
#trainData$Response <- mapvalues(trainData$Response, from = c("0", "1"), to = c("level0", "level1"))

#mccSummary only works if levels have name 0 and 1
#trainData$Response <- mapvalues(trainData$Response, from = c("level0", "level1"), to = c("0", "1"))

# Use Caret package with glm method and cross validation on the trainData, we will then evaluate the model with our testData

# define traininControl with 10-fold-cross validation
train_control<- trainControl(method="cv", number=10, summaryFunction = mccSummary, savePredictions = T)
#train_control<- trainControl(method="cv", number=10, summaryFunction = fnrSummary)
#train_control<- trainControl(method="cv", number=10, summaryFunction = twoClassSummary, classProbs = T)

# train the model, define family as binomial for logistic regression
model<- train(Response~., data=trainData, metric = "mcc",  trControl=train_control, method="rpart", minsplit=5, maximize = T)
#model<- train(Response~., data=trainData, metric = "fnr",  trControl=train_control, method="rpart", maximize = F)
#model<- train(Response~., data=trainData, metric ="ROC",  trControl=train_control, method="rpart", maximize = T)

# print cv scores
model

rpart.plot(model$finalModel)
varImp(model)

# Make Predictions and Calculate MCC
predictions <- predict(model, newdata = testData)
predictions <- as.numeric(predictions)
predictions[predictions == 1] <- 0
predictions[predictions == 2] <- 1

table(predictions, Response_test)
mcc(predictions,Response_test)



####### Rpart Without Cross Validation

model <- rpart(formula = Response~., data = trainData, minsplit=20, method = "class")
rpart.plot(model)

#VarImp 
varImp(model)

#Make Predictions and clalulate mcc
predictions <- predict(model, newdata = testData,  type = "class")
predictions <- as.numeric(predictions)
predictions[predictions == 1] <- 0
predictions[predictions == 2] <- 1

table(predictions, Response_test)
mcc(predictions,Response_test)




####### Rpart with missclassificationcost adjustments 

#Define lossMatrix
lossMatrix <- matrix(c(0,6,1,0), nrow = 2)
(t(lossMatrix))

model <- rpart(formula = Response~., data = trainData, method = "class", parms=list(split = "gini", loss =  lossMatrix))

rpart.plot(model)

#VarImp
varImp(model)

#Make Predictions and clalulate mcc
predictions <- predict(model, newdata = testData, type = "class")
predictions <- as.numeric(predictions)
predictions[predictions == 1] <- 0
predictions[predictions == 2] <- 1

table(predictions, Response_test)
mcc(predictions,Response_test)



