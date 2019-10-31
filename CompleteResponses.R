# Dataframe = CompleteResponse
# Y Value = brand
install.packages("caret", dependencies = c("Depends", "Suggests")) # Install the Caret Packages
str(CompleteResponses)
summary(CompleteResponses)

# Load library and set seed
library(caret)
set.seed(998)

# Create a 20% sample of the data
CompleteResponses <- CompleteResponses[sample(1:nrow(CompleteResponses), 9898,replace=FALSE),]

# Define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(CompleteResponses$brand, p = .75, list = FALSE)
training <- CompleteResponses[inTraining,]
testing <- CompleteResponses[-inTraining,]
------------------------------------------------------------------------------------
# 10 fold cross validation (C5.0 algorithm)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

### Train Decison Tree (C5.0) model with a tuneLenght = 1 (trains with 1 mtry value for C5.0)
C50 <- train(brand~., data = training, method = "C5.0", trControl=fitControl, tuneLength = 2)

# Training results
C50

# Check for variable importance for regression and classificiation models
varImp(C50) or C50imp <-varImp(C50) 
C50 or C50imp
------------------------------------------------------------------------------------

### Use RandomForest with 10-fold cross validation and manually tune 5 different mtry values

# 10 fold cross validation (RandomForest)

fitControl <- trainControl(method = "repeatedcv", number=10, repeats = 1)

# Dataframe for manual tuning of mtry
rfGrid <- expand.grid(mtry=c(1,2,3,4,5))

# Train Random Forest Regression model
# Note the system time wrapper. system.time()
# This is used to measure process execution time 
system.time(rfFitm1 <- train(brand~., data = training, method = "rf", trControl=fitControl, tuneGrid=rfGrid))

# Training results
RFimp <-varImp(rfFitm1) 
rfFitm1  # Check for Accuracy & Kappa Value
RFimp  # Random Variable Importance
-----------------------------------------------------------------------------
# Check for attributes
attributes(rfFitm1)
-----------------------------------------------------------------------------
### 10 fold cross validation ( with twoClassSummary)
fitControl <- trainControl(method = "repeatedcv", 
                             repeats =3,
                             classProbs = TRUE,
                             summaryFunction = twoClassSummary)

### Train Decison Tree (pls) model with a tuneLenght = 2 ----->codes below are not correct!
plsFit <- train(brand~., data = training, 
                method = "pls",                 
                tuneLength = 2,
                trControl= fitControl, 
                metric= "ROC",
                preProc = c("center", "scale"))
                
# Training results
#rfFit1
plsFit

-----------------------------------------------------------------
str(SurveyIncomplete)
summary(SurveyIncomplete)

# Load library and set seed
# library(caret)
# set.seed(998)

# Create a 20% sample of the data
CompleteResponses <- CompleteResponses[sample(1:nrow(CompleteResponses), 9898,replace=FALSE),]

# Define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(CompleteResponses$brand, p = .75, list = FALSE)
training <- CompleteResponses[inTraining,]
testing <- CompleteResponses[-inTraining,]

# 10 fold cross validation (C5.0 algorithm)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

### Train Decison Tree (C5.0) model with a tuneLenght = 1 (trains with 1 mtry value for C5.0)
C50 <- train(brand~., data = training, method = "C5.0", trControl=fitControl, tuneLength = 2)

# Training results
C50

# Check for variable importance for regression and classificiation models
varImp(C50) 
------------------------------------------------------------------------------------
# Make a prediction based on C50 ( SurveyIncomplete)
prediction <- predict(C50, SurveyIncomplete)
summary(prediction)

# Confusion Matrix 
confusionMatrix(prediction, SurveyIncomplete$brand)

# PostResample
postResample(prediction, SurveyIncomplete$brand)

-------------------------------------------------------
# Make a prediction based on C50 (25% testing from CompleteResponse)
prediction <- predict(C50, testing)
summary(prediction)

# Confusion Matrix
confusionMatrix(prediction, testing$brand)

# PostResample
postResample(prediction, testing$brand)

--------------------------------------------------------
# Make a prediction based on rfFitm1 ( SurveyIncomplete)-----Below codings are optional
prediction <- predict(rfFitm1, SurveyIncomplete)
summary(prediction)

# Confusion Matrix
confusionMatrix(prediction, SurveyIncomplete$brand)

# PostResample
postResample(prediction, SurveyIncomplete$brand)
--------------------------------------------------------
# Make a prediction based on rfFitm1 (25% testing from CompleteResponse)----Below codings are optional
prediction <- predict(rfFitm1, testing)
summary(prediction)

# Confusion Matrix
confusionMatrix(prediction, testing$brand)

# PostResample
postResample(prediction, testing$brand)









