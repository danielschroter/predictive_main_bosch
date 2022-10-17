library(data.table)
library(readr)

trainData <- fread("case4_train_numeric.csv", header= TRUE, data.table = FALSE)


#select the damaged parts
damaged_only <- trainData[trainData$Response == 1,]
write_csv(damaged_only,"damaged_only.csv", col_names = TRUE)

#select the functioning parts
functioning_only <- trainData[trainData$Response == 0,]

#trainSize equals to 80% of the data
trainSize = round(0.8*nrow(damaged_only))


#random selection of train and test data
set.seed(123)
training_indices <- sample(seq_len(nrow(damaged_only)), size = trainSize)
trainDamaged <- damaged_only[training_indices,]
testDamaged <- damaged_only[-training_indices,]

#Create A subsample where a fraction of 10% is damaged
set.seed(123)
Ten_damaged_train <- functioning_only[sample(nrow(functioning_only), 49527),]
Ten_damaged_train <- rbind(trainDamaged, Ten_damaged_train)
set.seed(123)
Ten_damaged_train <- Ten_damaged_train[sample(nrow(Ten_damaged_train)),]
write_csv(Ten_damaged_train,"ten_damaged_train.csv", col_names = TRUE)


#Create a subsample where a fraction of 50% is damaged
set.seed(123)
Fifty_damaged_train <- functioning_only[sample(nrow(functioning_only), 5503),]
Fifty_damaged_train <- rbind(trainDamaged, Fifty_damaged_train)
set.seed(123)
Fifty_damaged_train <- Fifty_damaged_train[sample(nrow(Fifty_damaged_train)),]
write_csv(Fifty_damaged_train,"fifty_damaged_train.csv", col_names = TRUE)

#Create a test subsample with the original balance of 0.58112%
set.seed(321)
testData <- functioning_only[sample(nrow(functioning_only), 235408),]
testData <- rbind(testDamaged, testData)
set.seed(123)
testData <- testData[sample(nrow(testData)),]
write_csv(testData,"testData.csv", col_names = TRUE)

#Create Train data from Product Group
product<-fread("product1_numeric.csv", header = TRUE, data.table = FALSE)
trainSize = round(0.7*nrow(product))


#random selection of train and test data
set.seed(123)
training_indices <- sample(seq_len(nrow(product)), size = trainSize)
product_train <- product[training_indices,]
product_test <- product[-training_indices,]
write_csv(product_train,"product_train.csv", col_names = TRUE)
write_csv(product_test,"product_test.csv", col_names = TRUE)
sum(product_train$Response)
sum(product_test$Response)
sum(sample_50k$Response)

#make a subsample of 50k records
set.seed(123)
sample_50k <- trainData[sample(nrow(trainData),50000),]
sample_50k_train <- 
write_csv(sample_50k,"sample_50k.csv", col_names = TRUE)

