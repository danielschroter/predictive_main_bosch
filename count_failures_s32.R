library(data.table)

#Load train_numeric and safe Response

trainData <- fread(file = "case4_train_numeric.csv", header = TRUE, data.table = FALSE)
Response <- trainData$Response
trainData$Response <- NULL

#Set values to 1 and na to 0, add Response column

trainData[trainData <= 1 & trainData >= -1] <- 1
trainData[is.na(trainData)] <- 0
trainData <- cbind(trainData, Response)

#Drop id
trainData  <- trainData[,2:970]

#Get subset of records wich passed through s32

records_s32 <- trainData[trainData$L3_S32_F3850 == 1,]

#calculate fraction of failed parts
num_failures_s32 <- sum(records_s32$Response == 1)
fraction_failures_s32 <- num_failures_s32/nrow(records_s32)
fraction_failures_s32
