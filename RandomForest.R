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



###### Apply Random Forest

#Transform the Response variable into a vector
trainData$Response <- factor(trainData$Response)

#model <- randomForest(formula = Response~., ntree=400, data = trainData, importance = T, do.trace = F, sampsize = c(30,50))
model <- randomForest(formula = Response~., ntree=400, data = trainData, importance = T, do.trace = T)
#model <- randomForest(formula = Response~., ntree=400, data = trainData, importance = T, do.trace = T, cutoff=c(0.92,0.08))

varImp(model)

#Make Predictions and clalulate mcc
predictions <- predict(model, newdata = testData,  type = "class")
predictions <- as.numeric(predictions)
predictions[predictions == 1] <- 0
predictions[predictions == 2] <- 1

table(predictions, Response_test)
mcc(predictions,Response_test)

