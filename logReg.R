library(data.table)
library(caret)
library(mltools)
library(factoextra)
library(plyr)

#Read the data
data <- fread("product.csv", header = TRUE, data.table = FALSE)

#Drop id
data <- data[2:198]

#trainSize size
trainSize <- round(nrow(data)*0.7)

#random selection of training and test data
set.seed(321)
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



###### Apply Logistic Regression

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

#Logistic Regression

#For ROC in twoClassSummary level names should not be 0 1
#levels(trainData$Response)
#trainData$Response <- mapvalues(trainData$Response, from = c("0", "1"), to = c("level0", "level1"))

#mccSummary only works if levels have name 0 and 1
#trainData$Response <- mapvalues(trainData$Response, from = c("level0", "level1"), to = c("0", "1"))

# Use Caret package with glm method and cross validation on the trainData, we will then evaluate the model with our testData

# define traininControl with 10-fold-cross validation
train_control<- trainControl(method="cv", number=10, summaryFunction = mccSummary)
#train_control<- trainControl(method="cv", number=10, summaryFunction = fnrSummary)
#train_control<- trainControl(method="cv", number=10, summaryFunction = twoClassSummary)

# train the model, define family as binomial for logistic regression
model<- train(Response~., data=trainData, metric = "mcc",  trControl=train_control, method="glm", family = binomial(link = "logit"), maximize = T)
#model<- train(Response~., data=trainData, metric ="ROC",  trControl=train_control, method="glm", family=binomial(link = "logit"), maximize = T)
#model<- train(Response~., data=trainData, metric = "fnr",  trControl=train_control, method="glm", family=binomial(link = "logit"), maximize = F)

# print cv scores
model
summary(model)
varImp(model)


#make Predictions and calculate mcc
predictions_logReg <- predict(model, newdata = testData)
predictions_logReg <- as.numeric(predictions_logReg)
predictions_logReg[predictions_logReg == 1] <- 0
predictions_logReg[predictions_logReg == 2] <- 1

table(predictions_logReg, Response_test)
mcc(predictions_logReg,Response_test)


## GLM Without Cross Validation
model_logReg <- glm(formula = Response~., family = binomial(link = "logit"), data = trainData)
summary(model_logReg)
anova(model_logReg, test = "Chisq")

#Type response leads to probabilites instead of the logOdds
predictions_logReg <- predict(model_logReg, newdata = testData, type = "response")
predictions_logReg[1:5]
predictions_logReg_binary <- ifelse(predictions_logReg>0.5, 1, 0)

table(predictions_logReg_binary, Response_test)
mcc(predictions_logReg_binary,Response_test)

#Probabilities of 1 occurs.
max(predictions_logReg)
sum(predictions_logReg==max(predictions_logReg)) 




