library(data.table)
library(plyr)

trainData <- fread("case4_train_numeric.csv", header = TRUE, data.table = FALSE)

#Create a subset for the first 500k records
set.seed(123)
training_indices <- sample(seq_len(nrow(trainData)), size = 500000)
sample_500k <- trainData[training_indices,]

#Set values to 1 and missing values to 0 
sample_500k[sample_500k <= 1 & sample_500k >= -1] <- 1
sample_500k[is.na(sample_500k)] <- 0

#Drop Id and Response
sample_500k_duplicates  <- sample_500k[,2:969]

#Count the number of duplicates of each unique row based on a subset
batch <- sample_500k_duplicates[1:100000,]
aggregation <- aggregate(list(numdup = rep(1,nrow(batch))) , batch, length)

#Check the max, mean and min number of duplicates
max(aggregation$numdup)
min(aggregation$numdup)
mean(aggregation$numdup)

#Select the characteristic pattern of 0 and 1 from the major product group

select_max_row <- aggregation[aggregation$numdup == max(aggregation$numdup),] 

select_max_row_search <- select_max_row

select_max_row_search$numdup

select_max_row_search$numdup <- NULL

#Extract the records from the total numeric dataset, This is done with the first 500k records
batch_size = 20000
res_duplicates<-data.frame()

for(i in 1:25){
  batch <- sample_500k[1+((i-1)*batch_size):((i)*batch_size),]
  res <- batch[which(apply(batch,1,function(x) all(select_max_row_search == x[2:969]))),]
  res_duplicates <- rbind(res_duplicates, res)
}



#Afterwards it is done for the next 680k parts
sample_500k <- trainData[-training_indices,]

for(i in 1:34){
  batch <- sample_500k[1+((i-1)*batch_size):((i)*batch_size),]
  res <- batch[which(apply(batch,1,function(x) all(select_max_row_search == x[2:969]))),]
  res_duplicates <- rbind(res_duplicates, res)
}

#Include remaining 3.747 records
batch <- batch <- sample_500k[1+((59)*batch_size):1183747,]
res <- batch[which(apply(batch,1,function(x) all(select_max_row_search == x[2:969]))),]
res_duplicates <- rbind(res_duplicates, res)


#Drop Columns that only contain missing values
nas <- apply(res_duplicates,2,function(x) sum(length(which(is.na(x)))))
data <- res_duplicates[, nas != nrow(res_duplicates)]

#drop column that contain 0 variance
data <- data[,apply(data, 2, function(x) var(x)!= 0)]

write_csv(data, "product_1.csv", col_names = TRUE)


