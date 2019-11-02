## Indoor WIFI Locationing Project

library(caret)
library(readr)
library(plotly)
processed_trainingData <- read_csv("processed_trainingData.csv")

## Inspect the dataset
summary(processed_trainingData)
str(processed_trainingData)    
head(processed_trainingData)
tail(processed_trainingData)

#################################################################################
## Preprocessing

## Removing Near Zero Variance from the dataset
rzv_training <- processed_trainingData[, sapply(processed_trainingData, var) != 0]  
str(rzv_training)
## Check if all zero variance columns have been removed
which(sapply(rzv_training, var) == 0)
which(sapply(rzv_training, var) != 0)

## Remove extra dependent variables
names(rzv_training)
rzv_training <- rzv_training[, -c(470:474)]

## Using dplyr to create a new attribute
library(tidyr)

## To create a single unique identifier (new column attribute) that combine 4 other attributes function
newDF <- unite(rzv_training, "LOCATION", c(FLOOR, BUILDINGID, SPACEID, RELATIVEPOSITION), remove = FALSE, sep ="-")
newDF$LOCATION <- as.factor(newDF$LOCATION)  

## Subsetting the Building 0-2
trainingBUD0 <- subset(newDF, BUILDINGID== "0")
trainingBUD1 <- subset(newDF, BUILDINGID== "1")
trainingBUD2 <- subset(newDF, BUILDINGID== "2")

## Remove Floor, BuildingID,SPACEID, RELATIVELOCATION from these subsets
str(trainingBUD1)
trainingBUD0[,2:5] <- NULL
trainingBUD1[,2:5] <- NULL
trainingBUD2[,2:5] <- NULL

## Apply Factorizing again after subsetting in order to drop factor levels in a subsetted data frame
trainingBUD0$LOCATION <- factor(trainingBUD0$LOCATION)
trainingBUD1$LOCATION <- factor(trainingBUD1$LOCATION)
trainingBUD2$LOCATION <- factor(trainingBUD2$LOCATION)
-----------------------------------------------------------------------------------------------------
set.seed(520)

## 10 fold cross validation 
fitControl <- trainControl(method = "cv", number = 10)

## Define 75%/25% train/test split of the dataset to BUILDING 0
inTraining0 <- createDataPartition(trainingBUD0$LOCATION, p = .75, list = FALSE)
training0 <- trainingBUD0[inTraining0,]
testing0 <- trainingBUD0[-inTraining0,]

## C5.0 model 
C50_BUD0 <- train(LOCATION~., data = training0, method = "C5.0", trControl=fitControl)

## Training results
summary(C50_BUD0)

## Testing 
prediction_C50BUD0 <- predict(C50_BUD0, testing0)
## Evaluate the model 
cm_C50_BUD0 <- confusionMatrix(prediction_C50BUD0, testing0$LOCATION)
cm_C50_BUD0
postResample(prediction_C50BUD0, testing0$LOCATION)
# PostResample Accuracy     Kappa 
#             0.7286512  0.7275444

-----------------------------------------------------------------------------------------------------
## Use RandomForest with 10-fold cross validation 

rf_BUD0 <- train(LOCATION~., data = training0, method = "rf", trControl=fitControl)
rf_BUD0            #  Accuracy      Kappa 
                  # 0.756292658  0.7552386
## Testing 
prediction_rfBUD0<- predict(rf_BUD0, testing0)
## Evaluate the model 
cm_rf_BUD0 <- confusionMatrix(prediction_rfBUD0, testing0$LOCATION)
cm_rf_BUD0
postResample(prediction_rfBUD0, testing0$LOCATION)
# PostResample Accuracy     Kappa 
#             0.7741421 0.7732169
-----------------------------------------------------------------------------
## Use K Nearest Neighbour with 10-fold cross validation
KNN_BUD0 <- train(LOCATION~., data = training0, method = "knn", trControl=fitControl)
KNN_BUD0             #  Accuracy      Kappa 
                    # 0.5411246    0.5391965
## Testing
prediction_KNNBUD0 <- predict(KNN_BUD0, testing0)
## Evaluation the model
cm_KNN_BUD0 <- confusionMatrix(prediction_KNNBUD0, testing0$LOCATION)
cm_KNN_BUD0
postResample(prediction_KNNBUD0, testing0$LOCATION)
# PostResample Accuracy     Kappa 
#             0.5554669  0.5536445

## Resample
resample_BUD0 <- resamples( list(C50 = C50_BUD0, RF = rf_BUD0, KNN = KNN_BUD0))
summary(resample_BUD0)
#############################################################################################
# Building 1

# Define 75%/25% train/test split of the dataset to BUILDING 1
inTraining1 <- createDataPartition(trainingBUD1$LOCATION, p = .75, list = FALSE)
training1 <- trainingBUD1[inTraining1,]
testing1 <- trainingBUD1[-inTraining1,]

## 10 fold cross validation 
fitControl <- trainControl(method = "cv", number = 10)

## Random Forest
rf_BUD1 <- train(LOCATION~., data = training1, method = "rf", trControl = fitControl)
rf_BUD1
# Testing
prediction_rfBUD1 <- predict(rf_BUD1, testing1)
# Evalute the model
cm_rf_BUD1 <- confusionMatrix(prediction_rfBUD1, testing1$LOCATION)
postResample(prediction_rfBUD1, testing1$LOCATION)
# postResample Accuracy     Kappa 
#             0.8595779  0.8587076
############################################################################################
## Building 2

# Define 75%/25% train/test split of the dataset to BUILDING 2
inTraining2 <- createDataPartition(trainingBUD2$LOCATION, p = .75, list = FALSE)
training2 <- trainingBUD2[inTraining2,]
testing2 <- trainingBUD2[-inTraining2,]

# 10 fold cross validation 
fitControl <- trainControl(method = "cv", number = 10)

# Random Forest
rf_BUD2 <- train(LOCATION~., data = training2, method = "rf", trControl = fitControl)
rf_BUD2
# Testing
prediction_rfBUD2 <- predict(rf_BUD2, testing2)
# Evalute the model
cm_rf_BUD2 <- confusionMatrix(prediction_rfBUD2, testing2$LOCATION)
postResample(prediction_rfBUD2, testing2$LOCATION)
# postResample Accuracy     Kappa 
#             0.9223986  0.9221561
############################################################################################
## Export the Confusion Matrix

resample_matrix <- resample_BUD0$values
resample_matrix
write.table(resample_matrix, file = "resample_matrix.csv", row.names = F, sep =",")
summary_resample <- summary(resample_BUD0)
resample_statistics <- summary_resample$statistics
write.table(resample_statistics, file = "resample_statistics.csv", row.names = F, sep =",")






