library(caret)
set.seed(998)
library(readr)
------------------------------------------------------------------
# Dummify the data
processed.existing <- dummyVars(" ~ .", data = existing_products) 
readyData <- data.frame(predict(processed.existing, newdata = existing_products))
str(readyData)
summary(readyData)
------------------------------------------------------------------
# Finding the Correlation
  # Delete any attribute that has missing information ('NA')
readyData$BestSellersRank <- NULL    
  # Use the cor() function to build the correlation matrix
corrData <- cor(readyData)
corrData
------------------------------------------------------------------
# Generate a heat map for your correlation matrix we'll use corrplot package
install.packages("corrplot")
library(corrplot)
corrplot(corrData)
------------------------------------------------------------------
# Remove some colinearity features
#Colinearity: 1StarReview with 2StarReviews & 3StarReviews with 4StarReviews
  
names(readyData) # Check your attributes# within your data set.
# Subsetting
newData1 <- readyData[,c(1:28)]
newData2 <- readyData[,c(1:12,14,16,18,20,21,22,23,28)]
newData3 <- readyData[,c(1:12,14,15,16,18,20,21,22,23,28)]
newData4 <- readyData[,c(1:12,16,18,20,21,23,28)]
newData5 <- readyData[,c(1:12,16,18,20,21,28)]
------------------------------------------------------------------
# Creating and Testing Dataset
trainSize <- round(nrow(readyData) * 0.8)
testSize <- nrow(readyData) - trainSize
trainSize
testSize
training_indices<-sample(seq_len(nrow(readyData)),size =trainSize)
trainSet<-readyData[training_indices,]
testSet<-readyData[-training_indices,] 
---------------------------------------------------------
names(readyData) # Names your attributes within your data set.
------------------------------------------------------------
# Linear Regression Model
LinearModel1 <- lm(Volume ~., newData5)
summary(LinearModel1) # Check for key metrics of your model

or

lm2 <- train(Volume~., data = newData5, method = "lm", trControl = fitControl)
lm2
------------------------------------------------------------
predict(LinearModel1, newData5) #View your prediction

------------------------------------------------------------
# Use this LinearModel1 to predict the testSet
prediction <- predict(LinearModel1, testSet)
summary(prediction)

------------------------------------------------------------
## WE DONOT USE ConfusionMatrix here as it is ONLY for classification problem, not a regression problem! 
### And then use the prediction in the testSet to predict the Y value in the testSet (same level)
# PostResample
postResample(prediction, testSet$Volume)
------------------------------------------------------------
# Support Vector Machine (SVM)
install.packages("e1071")
library(e1071)
library(caret)
set.seed(998)
-------------------------------------------------------------
# This is an old model, better skip and use the next method (use caret)
svm_model <- svm(Volume ~., newData5)
svmfit=svm(Volume ~., newData5, kernel="linear",cost=1.0,epsilon=0.1, scale=FALSE )
svmfit
predictions <- predict(svmfit, testSet)
summary(predictions)
postResample(predictions, testSet$Volume)
------------------------------------------------------------
##！Please use this model for SVM
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
svm_model <- train(Volume ~., data= newData5, method = "svmLinear", trControl=fitControl)
svm_model

prediction <- predict(svm_model, testSet)  
summary(prediction)
postResample(prediction, testSet$Volume)    
------------------------------------------------------------

### Use RandomForest with 10-fold cross validation and manually tune 5 different mtry values
  
# 10 fold cross validation (RandomForest)
  
fitControl <- trainControl(method = "repeatedcv", number=10, repeats = 1)

# Dataframe for manual tuning of mtry
rfGrid <- expand.grid(mtry=c(1,2,3,4,5))

# Train Random Forest Regression model
# Note the system time wrapper. system.time()
# This is used to measure process execution time 
system.time(rfFitm1 <- train(Volume~., data = newData5, method = "rf", trControl=fitControl, tuneGrid=rfGrid, importance=TRUE))

# Training results
rfFitm1  # Check for Accuracy & Kappa Value
varImp(rfFitm1) 

prediction <- predict(rfFitm1, testSet)
summary(prediction)
postResample(prediction, testSet$Volume)
-----------------------------------------------------------------------------
# Gradient Boosting Machines
  
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
gbm_model <- train(Volume ~., data= newData5, method = "gbm", distribution = "gaussian", trControl=fitControl)
gbm_model

prediction <- predict(gbm_model, testSet)
summary(prediction)
postResample(prediction, testSet$Volume)

---------------------------------------------------------------------
# Import new_products dataset ( for final forecasting)
new_products <- read_csv("new_products.csv")

# Dummify the data 
processed.new <- dummyVars(" ~ .", data = new_products) 
readyData2 <- data.frame(predict(processed.new, newdata = new_products))
str(readyData2)
summary(readyData2)
names(readyData2)
---------------------------------------------------------------------
# Eiliminate the NA colume and Subsetting
readyData2$BestSellersRank <- NULL   
reduced_new <- readyData2[,c(1:12,16,18,20,21,28)]
names(reduced_new)

### Make a prediction based on RandomForest (new_products)
finalPred <- predict(rfFitm1, reduced_new)
summary(finalPred)

output1 <- new_products
output1$predictions <- finalPred
write.csv(output, file="C2.T3output1.csv", row.names = TRUE)

[,c(1:12,14,15,16,18,20,21,22,23,28)]-----newData3--output
[,c(1:12,16,18,20,21,28)]----newData5----newData5---output1


