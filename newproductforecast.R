# Upload and check dataset
existing_products <- read_csv("existing_products.csv")
summary(existing_products)
str(existing_products)
is.na(existing_products)
library(caret)
set.seed(998)
library(readr)
--------------------------------------------------------------------------------------------
# Dummify the data
processed.existing <- dummyVars(" ~ .", data = existing_products) 
readyData <- data.frame(predict(processed.existing, newdata = existing_products))
str(readyData)
summary(readyData)
--------------------------------------------------------------------------------------------
# Finding the Correlation
# Delete any attribute that has missing information ('NA')
readyData$BestSellersRank <- NULL    

# Use the cor() function to build the correlation matrix
corrData <- cor(readyData)
corrData
--------------------------------------------------------------------------------------------
# Generate a heat map using corrplot package
install.packages("corrplot")
library(corrplot)
corrplot(corrData)
--------------------------------------------------------------------------------------------
# Remove some colinearity features
# Colinearity: 1StarReview with 2StarReviews & 3StarReviews with 4StarReviews
# Check your attributes# within your data set
names(readyData) 
# Subsetting
newData1 <- readyData[,c(1:28)]
newData2 <- readyData[,c(1:12,14,16,18,20,21,22,23,28)]
newData3 <- readyData[,c(1:12,14,15,16,18,20,21,22,23,28)]
newData4 <- readyData[,c(1:12,16,18,20,21,23,28)]
newData5 <- readyData[,c(1:12,16,18,20,21,28)]
--------------------------------------------------------------------------------------------
# Creating and Testing Dataset
trainSize <- round(nrow(readyData) * 0.8)
testSize <- nrow(readyData) - trainSize
trainSize
testSize
training_indices<-sample(seq_len(nrow(readyData)),size =trainSize)
trainSet<-readyData[training_indices,]
testSet<-readyData[-training_indices,] 
--------------------------------------------------------------------------------------------
# Names your attributes within your data set
names(readyData) 
--------------------------------------------------------------------------------------------
# Support Vector Machine (SVM)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 2)
svm_model <- train(Volume ~., data= newData5, method = "svmLinear", trControl=fitControl)
svm_model
#   RMSE      Rsquared 
# 1009.514   0.7530682

prediction <- predict(svm_model, testSet)  
summary(prediction)
postResample(prediction, testSet$Volume)    
--------------------------------------------------------------------------------------------
# Use Random Forest with 10-fold cross validation and manually tune 5 different mtry values
  
# 10 fold cross validation (RandomForest)
  
fitControl <- trainControl(method = "repeatedcv", number=10, repeats = 1)

# Dataframe for manual tuning of mtry
rfGrid <- expand.grid(mtry=c(1,2,3,4,5))

# Note the system time wrapper. system.time()
# This is used to measure process execution time 
system.time(rfFitm1 <- train(Volume~., data = newData5, method = "rf", trControl=fitControl, tuneGrid=rfGrid, importance=TRUE))

# Training results
rfFitm1  
#   RMSE         Rsquared 
# 649.0967023   0.8756764
varImp(rfFitm1) 

prediction <- predict(rfFitm1, testSet)
summary(prediction)
postResample(prediction, testSet$Volume)
--------------------------------------------------------------------------------------------
# Gradient Boosting Machines
  
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
gbm_model <- train(Volume ~., data= newData5, method = "gbm", distribution = "gaussian", trControl=fitControl)
gbm_model
#   RMSE      Rsquared 
# 811.7072   0.7774938

prediction <- predict(gbm_model, testSet)
summary(prediction)
postResample(prediction, testSet$Volume)

--------------------------------------------------------------------------------------------
# Import new_products dataset (final forecasting)
new_products <- read_csv("new_products.csv")

# Dummify the data 
processed.new <- dummyVars(" ~ .", data = new_products) 
readyData2 <- data.frame(predict(processed.new, newdata = new_products))
str(readyData2)
summary(readyData2)
names(readyData2)
--------------------------------------------------------------------------------------------
# Eiliminate the NA colume and Subsetting
readyData2$BestSellersRank <- NULL   
reduced_new <- readyData2[,c(1:12,16,18,20,21,28)]
names(reduced_new)
str(reduced_new)

# Make a prediction based on RandomForest (new_products)
finalPred <- predict(rfFitm1, reduced_new)
summary(finalPred)

# Output the prediction for new products
output1 <- new_products
output1$predictions <- finalPred
write.csv(output, file="C2.T3output1.csv", row.names = TRUE)



