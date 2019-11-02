## Parallel Processing
install.packages("doParallel")
library(doParallel)

library(caret)
library(readr)
library(plotly)
library(dplyr)
library(tidyr) 
library(ggplot2) 

## Find how many cores are on your machine
detectCores() 

## Create Cluster with desired number of cores. 
cl <- makeCluster(4)

## Register Cluster
registerDoParallel(cl)

## Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() 

## Stop Cluster. After performing your tasks, stop your cluster. 
stopCluster(cl)  # DON'T STOP CLUSTER WHEN COMPUTING, will lose connection 

## Importing iphone dataset
iphoneDF <- read_csv("C:/Dev/Data Analysis/Course 4/Task 3/iphone_smallmatrix_labeled_8d.csv")
summary(iphoneDF)
str(iphoneDF)
plot_ly(iphoneDF, x= ~iphoneDF$iphonesentiment, type='histogram')

## Check for missing values
is.na(iphoneDF)  

# Check for correlation & Generate a heat map for your correlation matrix we'll use corrplot package
library(corrplot)
corrData <- cor(iphoneDF) 
corrplot(corrData) 
options(max.print=1000000)
names(iphoneDF)
iphoneDF$iphonesentiment <- as.factor(iphoneDF$iphonesentiment)
----------------------------------------------------------------------------------------------
## Create a new data set and remove features highly correlated with the dependent. This is the classification 
## question, so actually we do not need to remove the collinearity. 
iphoneCOR <- iphoneDF[,c(1:4,7:10,12,13:15,17:20,22,25,27:35,37:40,42:43,45,47,48,50,52:59)] 

## Examine Feature Variance 

## NearZeroVar() with saveMetrics = TRUE returns an object containing a table including: frequency ratio, 
# percentage unique, zero variance and near zero variance 
nzvMetrics <- nearZeroVar(iphoneDF, saveMetrics = TRUE)
nzvMetrics
## NearZeroVar() with saveMetrics = FALSE returns an vector 
nzv <- nearZeroVar(iphoneDF, saveMetrics = FALSE) 
nzv 
## Create a new data set and remove near zero variance features
iphoneNZV <- iphoneDF[,-nzv]
str(iphoneNZV)

## Recursive Feature Elimination 
## Let's sample the data before using RFE
set.seed(123)
iphoneSample <- iphoneDF[sample(1:nrow(iphoneDF), 1000, replace=FALSE),]

## Set up rfeControl with randomforest, repeated cross validation and no updates
ctrl <- rfeControl(functions = rfFuncs, 
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

## Use rfe and omit the response variable (attribute 59 iphonesentiment) 
rfeResults <- rfe(iphoneSample[,1:58], 
                  iphoneSample$iphonesentiment, 
                  sizes= c(1:58), 
                  rfeControl= ctrl)

## Get results
rfeResults

## Plot results
plot(rfeResults, type=c("g", "o"))

## Create new data set with rfe recommended features
iphoneRFE <- iphoneDF[,predictors(rfeResults)]

## Add the independent variable to iphoneRFE
iphoneRFE$iphonesentiment <- iphoneDF$iphonesentiment

## Review outcome
str(iphoneRFE)

iphoneCOR$iphonesentiment <- as.factor(iphoneCOR$iphonesentiment)
iphoneNZV$iphonesentiment <- as.factor(iphoneNZV$iphonesentiment)
iphoneRFE$iphonesentiment <- as.factor(iphoneRFE$iphonesentiment)

------------------------------------------------------------------------------------
## Define an 70%/30% train/test split of the iphoneDF
inTraining <- createDataPartition(iphoneDF$iphonesentiment, p = .70, list = FALSE)
training <- iphoneDF[inTraining,]
testing <- iphoneDF[-inTraining,]

## 10 fold cross validation (C5.0 algorithm)
fitControl <- trainControl(method = "cv", number = 10)
## C5.0
C50 <- train(iphonesentiment~., data = training, method = "C5.0", trControl=fitControl)
## Training results
C50
## Check for variable importance for regression and classificiation models
varImp(C50) 

# Accuracy    Kappa
# 0.7754080  0.5641816

## Testing 
prediction_C50 <- predict(C50, testing)
## Evaluate the model 
cm_C50 <- confusionMatrix(prediction_C50, testing$iphonesentiment)
cm_C50  # Accuracy : 0.765  & 95% CI : (0.7514, 0.7783)  Kappa: 0.5427
postResample(prediction_C50, testing$iphonesentiment)
# PostResample Accuracy     Kappa 
#             0.7650386 0.5427491
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
## Use RandomForest with 10-fold cross validation 
rf <- train(iphonesentiment~., data = training, method = "rf", trControl=fitControl)
rf
varImp(rf)
#  Accuracy    Kappa
#  0.7755111  0.5684646

## Testing 
prediction_rf<- predict(rf, testing)
## Evaluate the model 
cm_rf <- confusionMatrix(prediction_rf, testing$iphonesentiment)
cm_rf # Accuracy : 0.7699 & 95% CI : (0.7564, 0.7831) Kappa: 0.5551
postResample(prediction_rf, testing$iphonesentiment)
## PostResample Accuracy     Kappa 
#             0.7699229  0.5551264  
------------------------------------------------------------------------------------
## Use SVM with 10-fold cross validation 
svm <- train(iphonesentiment~., data = training, method = "svmLinear", trControl=fitControl)
svm
varImp(svm)
#     Accuracy    Kappa
#    0.7116581  0.4178888
## Testing 
prediction_svm<- predict(svm, testing)
## Evaluate the model 
cm_svm <- confusionMatrix(prediction_svm, testing$iphonesentiment)
cm_svm # Accuracy : 0.7167  & 95% CI : (0.7023, 0.7308) Kappa: 0.4303
postResample(prediction_svm, testing$iphonesentiment)
# PostResample Accuracy     Kappa 
#             0.7167095  0.4302806      
---------------------------------------------------------------------------------------
## Use KKNN with 10-fold cross validation 
kknn <- train(iphonesentiment~., data = training, method = "kknn", trControl=fitControl)
kknn
varImp(kknn)
#     Accuracy    Kappa
#    0.3309455  0.1626048
## Testing 
prediction_kknn<- predict(kknn, testing)
# Evaluate the model 
cm_kknn <- confusionMatrix(prediction_kknn, testing$iphonesentiment)
cm_kknn # Accuracy : 0.3283   &  95% CI : (0.3135, 0.3433)  Kappa : 0.1688 
postResample(prediction_kknn, testing$iphonesentiment)
# PostResample Accuracy     Kappa 
#            0.3282776   0.1687898 

############################################################################################
## Define an 70%/30% train/test split of the iphoneCOR
inTraining_iphoneCOR <- createDataPartition(iphoneCOR$iphonesentiment, p = .70, list = FALSE)
training_COR <- iphoneCOR[inTraining,]
testing_COR <- iphoneCOR[-inTraining,]

## Use RandomForest with 10-fold cross validation on iphoneCOR
rf_COR <- train(iphonesentiment~., data = training_COR, method = "rf", trControl=fitControl)
rf_COR
varImp(rf_COR)
#  Accuracy    Kappa
# 0.7549243  0.5226978

## Testing 
prediction_rf_COR<- predict(rf_COR, testing_COR)
## Evaluate the model 
cm_rf_COR <- confusionMatrix(prediction_rf_COR, testing_COR$iphonesentiment)
cm_rf_COR # Accuracy : 0.7517 & 95% CI : (0.7378, 0.7652)  Kappa : 0.5145 
postResample(prediction_rf_COR, testing_COR$iphonesentiment)
# PostResample Accuracy     Kappa 
#             0.7516710  0.5145394 
#############################################################################################
## Define an 70%/30% train/test split of the iphoneNZV
inTraining_iphoneNZV <- createDataPartition(iphoneNZV$iphonesentiment, p = .70, list = FALSE)
training_NZV <- iphoneNZV[inTraining,]
testing_NZV <- iphoneNZV[-inTraining,]

## Use RandomForest with 10-fold cross validation on iphoneCOR
rf_NZV <- train(iphonesentiment~., data = training_NZV, method = "rf", trControl=fitControl)
rf_NZV
varImp(rf_NZV)
#  Accuracy    Kappa
# 0.7619739  0.5308519

## Testing 
prediction_rf_NZV<- predict(rf_NZV, testing_NZV)
## Evaluate the model 
cm_rf_NZV <- confusionMatrix(prediction_rf_NZV, testing_NZV$iphonesentiment)
cm_rf_NZV # Accuracy : 0.7537 & 95% CI : (0.7399, 0.7672) Kappa: 0.5134
postResample(prediction_rf_NZV, testing_NZV$iphonesentiment)
# PostResample Accuracy     Kappa 
#             0.7537275  0.5134429
############################################################################################
## Define an 70%/30% train/test split of the iphoneRFE
inTraining_iphoneRFE <- createDataPartition(iphoneRFE$iphonesentiment, p = .70, list = FALSE)
training_RFE <- iphoneRFE[inTraining,]
testing_RFE <- iphoneRFE[-inTraining,]

## Use RandomForest with 10-fold cross validation on iphoneCOR
rf_RFE <- train(iphonesentiment~., data = training_RFE, method = "rf", trControl=fitControl)
rf_RFE
varImp(rf_RFE)
#    Accuracy    Kappa
#   0.7728775  0.5645542

# Testing 
prediction_rf_RFE<- predict(rf_RFE, testing_RFE)
# Evaluate the model 
cm_rf_RFE <- confusionMatrix(prediction_rf_RFE, testing_RFE$iphonesentiment)
cm_rf_RFE # Accuracy : 0.7681 & 95% CI : (0.7545, 0.7813)   Kappa: 0.5531
postResample(prediction_rf_RFE, testing_RFE$iphonesentiment)
# PostResample Accuracy     Kappa 
#             0.7681234 0.5530927
###########################################################################################

# Resample
resample_RF <- resamples( list(rf,rf_COR,rf_NZV,rf_RFE))
summary(resample_RF)

###########################################################################################
# Feature Engineering-recode()

## Create a new dataset that will be used for recoding sentiment
iphoneRC <- iphoneDF
## Recode sentiment to combine factor levels 0 & 1 and 4 & 5
iphoneRC$iphonesentiment <- recode(iphoneRC$iphonesentiment, '0' = 1, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 4) 
## Inspect results
summary(iphoneRC)
str(iphoneRC)
## Make iphonesentiment a factor
iphoneRC$iphonesentiment <- as.factor(iphoneRC$iphonesentiment)
str(iphoneRC)

## Define an 70%/30% train/test split of the iphoneRC
inTrainingRC <- createDataPartition(iphoneRC$iphonesentiment, p = .70, list = FALSE)
training_RC <- iphoneRC[inTraining,]
testing_RC <- iphoneRC[-inTraining,]

## 10 fold cross validation 
fitControl <- trainControl(method = "cv", number = 10)

## Use The Best RandomForest with 10-fold cross validation on Recoding the Dependant variable
rf_RC <- train(iphonesentiment~., data = training_RC, method = "rf", trControl=fitControl)
rf_RC
varImp(rf_RC)
#   Accuracy     Kappa
#  0.8522535  0.6360303

# Testing 
prediction_rf_RC<- predict(rf_RC, testing_RC) 
# Evaluate the model 
cm_rf_RC <- confusionMatrix(prediction_rf_RC, testing_RC$iphonesentiment)
cm_rf_RC # Accuracy : 0.8542 & 95% CI : (0.8428, 0.8652) Kappa: 0.6408
summary(cm_rf_RC)
postResample(prediction_rf_RC, testing_RC$iphonesentiment)
# PostResample Accuracy     Kappa 
#             0.8542416  0.6408144
----------------------------------------------------------------------------------------------
## Principle Component Analysis
  
## data = training and testing from iphoneDF (no feature selection) 
## create object containing centered, scaled PCA components from training set
## excluded the dependent variable and set threshold to .95
preprocessParams <- preProcess(training[,-59], method=c("center", "scale", "pca"), thresh = 0.95)
print(preprocessParams)

# use predict to apply pca parameters, create training, exclude dependant
train.pca <- predict(preprocessParams, training[,-59])

# add the dependent to training
train.pca$iphonesentiment <- training$iphonesentiment

# use predict to apply pca parameters, create testing, exclude dependant
test.pca <- predict(preprocessParams, testing[,-59])

# add the dependent to testing
test.pca$iphonesentiment <- testing$iphonesentiment

# inspect results
str(train.pca)
str(test.pca)

# 10 fold cross validation 
fitControl <- trainControl(method = "cv", number = 10)

### Use The Best RandomForest with 10-fold cross validation on Principal Component Analysis 
rf_pca <- train(iphonesentiment~., data = train.pca, method = "rf", trControl=fitControl)
rf_pca
varImp(rf_pca)
#   Accuracy    Kappa
#  0.7641705  0.5473995

# Testing 
prediction_rf_pca<- predict(rf_pca, test.pca)
# Evaluate the model 
cm_rf_pca <- confusionMatrix(prediction_rf_pca, test.pca$iphonesentiment)
cm_rf_pca # Accuracy : 0.7568 & 95% CI : (0.743, 0.7702) Kappa: 0.5314
postResample(prediction_rf_pca, test.pca$iphonesentiment)
# PostResample Accuracy     Kappa 
#             0.7568123 0.5313744  

##################################################################################################
##################################################################################################

# Apply Model to Large Matrix (22461 Observations)
iphoneLargeMatrix <- read_csv("C:/Dev/Data Analysis/Course 4/Task 3/iphoneLargeMatrix.csv")
str(iphoneLargeMatrix)
summary(iphoneLargeMatrix)

# Remove the 1st column id from iphoneLargeMatrix 
iphoneLargeMatrix$id <- NULL
str(iphoneRC)

# Apply Random Forest Feature Engineering-Recode() method to iphoneLargeMatrix--This is the best method in this task! 

# 10 fold cross validation 
fitControl <- trainControl(method = "cv", number = 10)

# Testing 
finalPred_iphone <- predict(rf_RC, iphoneLargeMatrix)
summary(finalPred_iphone)
#######################################################################################################
#######################################################################################################
## Importing SamsungDF 
samsungDF <- read_csv("C:/Dev/Data Analysis/Course 4/Task 3/galaxy_smallmatrix_labeled_9d.csv")
summary(samsungDF)
str(samsungDF)
plot_ly(samsungDF, x= ~samsungDF$galaxysentiment, type='histogram')

## Check for missing values
is.na(samsungDF)  # no missing values

## Check for correlation & Generate a heat map for your correlation matrix we'll use corrplot package
library(corrplot)
corrData_samsung <- cor(samsungDF) 
corrplot(corrData_samsung) 
options(max.print=1000000)
names(samsungDF)
samsungDF$galaxysentiment <- as.factor(samsungDF$galaxysentiment)
----------------------------------------------------------------------------------------------
## Create a new data set and remove features highly correlated with the dependant. This is the classification 
## question, so actually we do not need to remove the colinearity. 
samsungCOR <- samsungDF[,c(1:4,7:10,12,13:15,17:20,22,25,27:35,37:40,42:43,45,47,48,50,52:59)] 

## Examine Feature Variance for Samsung DF

## NearZeroVar() with saveMetrics = TRUE returns an object containing a table including: frequency ratio, 
## percentage unique, zero variance and near zero variance 
nzvMetrics_samsung <- nearZeroVar(samsungDF, saveMetrics = TRUE)
nzvMetrics_samsung
## nearZeroVar() with saveMetrics = FALSE returns an vector 
nzv_samsung <- nearZeroVar(samsungDF, saveMetrics = FALSE) 
nzv_samsung 
## Create a new data set and remove near zero variance features
samsungNZV <- samsungDF[,-nzv_samsung]
str(samsungNZV)

## Recursive Feature Elimination set.seed(123)
samsungSample <- samsungDF[sample(1:nrow(samsungDF), 1000, replace=FALSE),]
## Let's sample the data before using RFE for Samsung DF


## Set up rfeControl with randomforest, repeated cross validation and no updates
ctrl_samsung <- rfeControl(functions = rfFuncs, 
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

## Use rfe and omit the response variable (attribute 59 iphonesentiment) 
rfeResults_samsung <- rfe(samsungSample[,1:58], 
                  samsungSample$galaxysentiment, 
                  sizes= c(1:58), 
                  rfeControl= ctrl_samsung)

## Get results
rfeResults_samsung

## Plot results
plot(rfeResults_samsung, type=c("g", "o"))

## Create new data set with rfe recommended features
samsungRFE <- samsungDF[,predictors(rfeResults_samsung)]

## Add the independent variable to samsungRFE
samsungRFE$galaxysentiment <- samsungDF$galaxysentiment

## Review outcome
str(samsungRFE)

samsungCOR$galaxysentiment <- as.factor(samsungCOR$galaxysentiment)
samsungNZV$galaxysentiment <- as.factor(samsungNZV$galaxysentiment)
samsungRFE$galaxysentiment <- as.factor(samsungRFE$galaxysentiment)

------------------------------------------------------------------------------------
## Define an 70%/30% train/test split of the samsungDF
inTraining_samsung <- createDataPartition(samsungDF$galaxysentiment, p = .70, list = FALSE)
training_samsung <- samsungDF[inTraining_samsung,]
testing_samsung <- samsungDF[-inTraining_samsung,]

## 10 fold cross validation (C5.0 algorithm)
fitControl <- trainControl(method = "cv", number = 10)
## C5.0 on SamsungDF
C50_samsung <- train(galaxysentiment~., data = training_samsung, method = "C5.0", trControl=fitControl)

## Training results
C50_samsung
## Check for variable importance for regression and classificiation models
varImp(C50_samsung) 

# Accuracy    Kappa
# 0.7664810  0.5335700

## Testing 
prediction_C50_samsung <- predict(C50_samsung, testing_samsung)
## Evaluate the model 
cm_C50_samsung <- confusionMatrix(prediction_C50_samsung, testing_samsung$galaxysentiment)
cm_C50_samsung  # Accuracy : 0.7629  & 95% CI : (0.7491, 0.7762)  Kappa: 0.5205
postResample(prediction_C50_samsung, testing_samsung$galaxysentiment)
# PostResample Accuracy     Kappa 
#             0.7628520  0.5205043 
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
## Use RandomForest with 10-fold cross validation on SamsungDF
rf_samsung <- train(galaxysentiment~., data = training_samsung, method = "rf", trControl=fitControl)
rf_samsung
varImp(rf_samsung)
#  Accuracy    Kappa
# 0.7667071  0.5363298

## Testing 
prediction_rf_samsung<- predict(rf_samsung, testing_samsung)
## Evaluate the model 
cm_rf_samsung <- confusionMatrix(prediction_rf_samsung, testing_samsung$galaxysentiment)
cm_rf_samsung # Accuracy : 0.7585 & 95% CI : (00.7447, 0.7719)  Kappa : 0.5163  
postResample(prediction_rf_samsung, testing_samsung$galaxysentiment)
# PostResample Accuracy     Kappa 
#             0.7584603  0.5163465
------------------------------------------------------------------------------------
## Use SVM with 10-fold cross validation on SamsungDF 
svm_samsung <- train(galaxysentiment~., data = training_samsung, method = "svmLinear", trControl=fitControl)
svm_samsung
#     Accuracy    Kappa
#    0.7197992  0.4127767
## Testing 
prediction_svm_samsung<- predict(svm_samsung, testing_samsung)
## Evaluate the model 
cm_svm_samsung <- confusionMatrix(prediction_svm_samsung, testing_samsung$galaxysentiment)
cm_svm_samsung # Accuracy : 0.7145  & 95% CI : (0.7, 0.7287)  Kappa : 0.399 
postResample(prediction_svm_samsung, testing_samsung$galaxysentiment)
# PostResample Accuracy     Kappa 
#            0.7145440   0.3990401         
---------------------------------------------------------------------------------------
## Use KKNN with 10-fold cross validation on SamsungDF
kknn_samsung <- train(galaxysentiment~., data = training_samsung, method = "kknn", trControl=fitControl)
kknn_samsung
varImp(kknn_samsung)
#     Accuracy    Kappa
#    0.7465822  0.5042626
## Testing 
prediction_kknn_samsung<- predict(kknn_samsung, testing_samsung)
## Evaluate the model 
cm_kknn_samsung <- confusionMatrix(prediction_kknn_samsung, testing_samsung$galaxysentiment)
cm_kknn_samsung # Accuracy : 0.744   & 95% CI : (0.7299, 0.7577) Kappa : 0.4941  
postResample(prediction_kknn_samsung, testing_samsung$galaxysentiment)
# PostResample Accuracy     Kappa 
#            0.7439938   0.4941093  

############################################################################################
## Define an 70%/30% train/test split of the samsungCOR 
inTraining_COR_samsung <- createDataPartition(samsungCOR$galaxysentiment, p = .70, list = FALSE)
training_COR_samsung <- samsungCOR[inTraining_COR_samsung,]
testing_COR_samsung <- samsungCOR[-inTraining_COR_samsung,]

## Use RandomForest with 10-fold cross validation on samsungCOR
rf_COR_samsung <- train(galaxysentiment~., data = training_COR_samsung, method = "rf", trControl=fitControl)
rf_COR_samsung
varImp(rf_COR_samsung)
#  Accuracy    Kappa
# 0.7504465  0.4960626

## Testing 
prediction_rf_COR_samsung<- predict(rf_COR_samsung, testing_COR_samsung)
## Evaluate the model 
cm_rf_COR_samsung <- confusionMatrix(prediction_rf_COR_samsung, testing_COR_samsung$galaxysentiment)
cm_rf_COR_samsung # Accuracy : 0.7528 & 95% CI : (0.7389, 0.7663)  Kappa : 0.5009
postResample(prediction_rf_COR_samsung, testing_COR_samsung$galaxysentiment)     
# PostResample Accuracy     Kappa 
#             0.7527771  0.5008982
#############################################################################################
## Define an 70%/30% train/test split of the samsungNZV
inTraining_samsungNZV <- createDataPartition(samsungNZV$galaxysentiment, p = .70, list = FALSE)
training_NZV_samsung <- samsungNZV[inTraining_samsungNZV,]
testing_NZV_samsung <- samsungNZV[-inTraining_samsungNZV,]

## Use RandomForest with 10-fold cross validation on iphoneCOR
rf_NZV_samsung <- train(galaxysentiment~., data = training_NZV_samsung, method = "rf", trControl=fitControl)
rf_NZV_samsung
varImp(rf_NZV_samsung)
#  Accuracy    Kappa
# 0.7506627  0.4883227

## Testing 
prediction_rf_NZV_samsung<- predict(rf_NZV_samsung, testing_NZV_samsung)
## Evaluate the model 
cm_rf_NZV_samsung <- confusionMatrix(prediction_rf_NZV_samsung, testing_NZV_samsung$galaxysentiment)
cm_rf_NZV_samsung # Accuracy : 0.7587 & 95% CI : (0.7449, 0.7721) Kappa: 0.508
postResample(prediction_rf_NZV_samsung, testing_NZV_samsung$galaxysentiment)
# PostResample Accuracy     Kappa 
#             0.7587187  0.5079924
############################################################################################
## Define an 70%/30% train/test split of the samsungRFE
inTraining_samsungRFE <- createDataPartition(samsungRFE$galaxysentiment, p = .70, list = FALSE)
training_RFE_samsung <- samsungRFE[inTraining_samsungRFE,]
testing_RFE_samsung <- samsungRFE[-inTraining_samsungRFE,]

## Use RandomForest with 10-fold cross validation on samsungRFE
rf_RFE_samsung <- train(galaxysentiment~., data = training_RFE_samsung, method = "rf", trControl=fitControl)
rf_RFE_samsung
varImp(rf_RFE_samsung)  
#    Accuracy    Kappa
#   0.7633793  0.5301638

## Testing 
prediction_rf_RFE_samsung<- predict(rf_RFE_samsung, testing_RFE_samsung)
## Evaluate the model 
cm_rf_RFE_samsung <- confusionMatrix(prediction_rf_RFE_samsung, testing_RFE_samsung$galaxysentiment)
cm_rf_RFE_samsung # Accuracy : 0.7683 & 95% CI : (0.7547, 0.7815) Kappa: 0.5387
postResample(prediction_rf_RFE_samsung, testing_RFE_samsung$galaxysentiment)
# PostResample Accuracy     Kappa 
#             0.7682769   0.5386639
###########################################################################################

# Resample
resample_RF_samsung <- resamples( list(rf_samsung,rf_COR_samsung,rf_NZV_samsung,rf_RFE_samsung))
summary(resample_RF_samsung)

###########################################################################################
## Feature Engineering-recode()

## Create a new dataset that will be used for recoding sentiment
samsungRC <- samsungDF
## Recode sentiment to combine factor levels 0 & 1 and 4 & 5
samsungRC$galaxysentiment <- recode(samsungRC$galaxysentiment, '0' = 1, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 4) 
## Inspect results
summary(samsungRC)
str(samsungRC)
## Make iphonesentiment a factor
samsungRC$galaxysentiment <- as.factor(samsungRC$galaxysentiment)
str(samsungRC)

## Define an 70%/30% train/test split of the samsungRC
inTrainingRC_samsung <- createDataPartition(samsungRC$galaxysentiment, p = .70, list = FALSE)
training_RC_samsung <- samsungRC[inTrainingRC_samsung,]
testing_RC_samsung <- samsungRC[-inTrainingRC_samsung,]

## 10 fold cross validation 
fitControl <- trainControl(method = "cv", number = 10)

## Use The Best RandomForest with 10-fold cross validation on Recoding the Independant variable
rf_RC_samsung <- train(galaxysentiment~., data = training_RC_samsung, method = "rf", trControl=fitControl)
rf_RC_samsung
varImp(rf_RC_samsung)
#  Accuracy    Kappa
# 0.8441211  0.5961513

## Testing 
prediction_rf_RC_samsung<- predict(rf_RC_samsung, testing_RC_samsung) 
## Evaluate the model 
cm_rf_RC_samsung <- confusionMatrix(prediction_rf_RC_samsung, testing_RC_samsung$galaxysentiment)
cm_rf_RC_samsung # Accuracy : 0.8417 & 95% CI : (0.8298, 0.853) Kappa: 0.5916 
postResample(prediction_rf_RC_samsung, testing_RC_samsung$galaxysentiment)
# PostResample Accuracy     Kappa 
#             0.8416839  0.5915825 
----------------------------------------------------------------------------------------------
## Principle Component Analysis 
  
## data = training and testing from samsungDF (no feature selection) 
## create object containing centered, scaled PCA components from training set
## excluded the dependent variable and set threshold to .95
preprocessParams_samsung <- preProcess(training_samsung[,-59], method=c("center", "scale", "pca"), thresh = 0.95)
print(preprocessParams_samsung)

## Use predict to apply pca parameters, create training, exclude dependant
train.pca_samsung <- predict(preprocessParams_samsung, training_samsung[,-59])

## Add the dependent to training
train.pca_samsung$galaxysentiment <- training_samsung$galaxysentiment

## Use predict to apply pca parameters, create testing, exclude dependant
test.pca_samsung <- predict(preprocessParams_samsung, testing_samsung[,-59])

## Add the dependent to testing
test.pca_samsung$galaxysentiment <- testing_samsung$galaxysentiment

## Inspect results
str(train.pca_samsung)
str(test.pca_samsung)

## 10 fold cross validation 
fitControl <- trainControl(method = "cv", number = 10)

## Use The Best RandomForest with 10-fold cross validation on Principal Component Analysis 
rf_pca_samsung <- train(galaxysentiment~., data = train.pca_samsung, method = "rf", trControl=fitControl)
rf_pca_samsung
varImp(rf_pca_samsung)
#  Accuracy    Kappa
# 0.7561984  0.5162022

## Testing 
prediction_rf_pca_samsung<- predict(rf_pca_samsung, test.pca_samsung)
## Evaluate the model 
cm_rf_pca_samsung <- confusionMatrix(prediction_rf_pca_samsung, test.pca_samsung$galaxysentiment)
cm_rf_pca_samsung # Accuracy : 0.7476 & 95% CI : (0.7336, 0.7612) Kappa: 0.4956 
postResample(prediction_rf_pca_samsung, test.pca_samsung$galaxysentiment)
# PostResample Accuracy     Kappa 
#             0.7476104   0.4956318 

###################################################################################################
###################################################################################################
## Create a data frame for plotting
## You can add more sentiment levels if needed
## Replace sentiment values 
library(plotly)
packageVersion('plotly')
summary(finalPred_iphone)
pieData <- data.frame(COM = c("negative", "somewhat negative", "somewhat positive", "positive"), 
                      values = c(9467, 614, 1407, 10790 ))

# Create pie chart
plot_ly(pieData, labels = ~COM, values = ~ values, type = "pie",
              textposition = 'inside',
              textinfo = 'label+percent', 
              insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text',
              text = ~paste( values),
              marker = list(colors = colors,
                            line = list(color = '#FFFFFF', width = 1)),
              showlegend = F) %>%
  layout(title = 'iPhone Sentiment From Large Matrix', 
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# Create two pie charts (side by side)
# summary(iphoneRC) # get last column of iphonesentiment count and put it in below values vector
pieData_iphoneRC <- data.frame(COM = c("negative", "somewhat negative", "somewhat positive", "positive"), 
                    values = c( 2352, 454, 1188, 8979 ))
#summary(samsungRC) # get last column of galaxysentiment count and put it in below values vector
pieData_samsungRC <- data.frame(COM = c("negative", "somewhat negative", "somewhat positive", "positive"), 
                              values = c( 2078, 450, 1175, 9208 ))

plot_ly(pieData_iphoneRC, labels = ~COM, values = ~ values, type = "pie", title = 'iPhone Sentiment', 
        domain = list(x = c(0, 0.5), y = c(0, 1))) %>%
  add_trace(data = pieData_samsungRC, labels = ~COM, values = ~ values, type = "pie", title = 'Samsung Sentiment', 
            domain = list(x = c(0.52, 1.02), y = c(0, 1)))

