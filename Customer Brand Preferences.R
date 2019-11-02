# Dataframe = CompleteResponse
# Y Value = brand
install.packages("caret", dependencies = c("Depends", "Suggests")) 
# Loading and preprocssing the complete dataset
str(CompleteResponses)
summary(CompleteResponses)

# Make sure below attributes are factors
CompleteResponses$elevel <- as.factor(CompleteResponses$elevel)
CompleteResponses$car <- as.factor(CompleteResponses$car)
CompleteResponses$zipcode <- as.factor(CompleteResponses$zipcode)
CompleteResponses$brand <- as.factor(CompleteResponses$brand)

# Load library and set seed
library(caret)
set.seed(998)

# Define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(CompleteResponses$brand, p = .75, list = FALSE)
training <- CompleteResponses[inTraining,]
testing <- CompleteResponses[-inTraining,]
--------------------------------------------------------------------------------------------
# 10 fold cross validation 
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

### Train Decison Tree (C5.0) model with a tuneLenght = 1 (trains with 1 mtry value for C5.0)
C50 <- train(brand~., data = training, method = "C5.0", trControl=fitControl, tuneLength = 1)

# Training results
C50  #  Accuracy     Kappa
     # 0.9214723  0.8341472

# Check for variable importance for regression and classificiation models
C50imp <-varImp(C50) 
C50imp
--------------------------------------------------------------------------------------------

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
rfFitm1  # Accuracy       Kappa 
         # 0.8997812  0.7869493517
RFimp  # Random Variable Importance
---
# Check for attributes
attributes(rfFitm1)
#################################################################################

# Importing and preprocessing the incomplete dataset
library(readr)
SurveyIncomplete <- read_csv("SurveyIncomplete.csv")
SurveyIncomplete$brand <- as.factor(SurveyIncomplete$brand)
SurveyIncomplete$elevel <- as.factor(SurveyIncomplete$elevel)
SurveyIncomplete$car <- as.factor(SurveyIncomplete$car)
SurveyIncomplete$zipcode <- as.factor(SurveyIncomplete$zipcode)
str(SurveyIncomplete)
summary(SurveyIncomplete)

# Load library and set seed
library(caret)
set.seed(998)
--------------------------------------------------------------------------------------------
# Make a prediction based on C50 (SurveyIncomplete)
prediction <- predict(C50, SurveyIncomplete)
summary(prediction)

# Confusion Matrix 
confusionMatrix(prediction, SurveyIncomplete$brand)

# PostResample
postResample(prediction, SurveyIncomplete$brand)
#  Accuracy      Kappa 
# 0.39280000  0.01241225  # No Ground Truth lead to Low Accuracy & Kappa value
--------------------------------------------------------------------------------------------
# Make a prediction based on C50 (25% testing from CompleteResponse)
prediction_testing <- predict(C50, testing)
summary(prediction_testing)

# Confusion Matrix
confusionMatrix(prediction_testing, testing$brand)

# PostResample
postResample(prediction_testing, testing$brand)
#  Accuracy    Kappa 
# 0.9199677 0.8296477 








