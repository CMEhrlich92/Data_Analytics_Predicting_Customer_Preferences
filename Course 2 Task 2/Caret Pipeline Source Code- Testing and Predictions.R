#C2T2 Caret Pipeline- Customer Brand Predictions


#-----INSTALL/LOAD PACKAGES-----#
install.packages("caret")
install.packages("corrplot")
install.packages("readr")
require(caret)
require(corrplot)
require(mlbench)
require(readr)


#-----IMPORT/LOAD DATASETS-----#
#Complete Surveys - CSurvey
library(readxl)
CSurvey <- read_excel("D:/Data Analytics Course Tasks/Course 2 Task 2- Predicting Outcomes by Classification/Data/Completed Surveys- Training Data.xlsx", 
                      sheet = "Survey Results Complete")
#Incomplete Surveys - ICSurvey
library(readr)
ICSurvey <- read_csv("D:/Data Analytics Course Tasks/Course 2 Task 2- Predicting Outcomes by Classification/Data/Incomplete Surveys- Test Data.csv")


#-----DATA EVALUATION-----#
#CSurvey
str(CSurvey)
summary(CSurvey)
str(ICSurvey)
summary(ICSurvey)


#-----PREPROCESSING-----#
#CSurvey
CSurvey$elevel <- as.factor(CSurvey$elevel)
CSurvey$car <- as.factor(CSurvey$car)
CSurvey$zipcode <- as.factor(CSurvey$zipcode)
CSurvey$brand <- as.factor(CSurvey$brand)
#ICSurvey
ICSurvey$elevel <- as.factor(ICSurvey$elevel)
ICSurvey$car <- as.factor(ICSurvey$car)
ICSurvey$zipcode <- as.factor(ICSurvey$zipcode)
ICSurvey$brand <- as.factor(ICSurvey$brand)

# save dataset
write.csv(CSurvey)


#-----TRAIN/TRAIN CONTROL-----#
# set random seed
set.seed(123)
#createDataPartition
inTrain <- createDataPartition(y=CSurvey$brand, p = .75, list = FALSE)
# create training/testing dataset
trainSet <- CSurvey[inTrain,]
testSet <- CSurvey[-inTrain,]
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)


#-----TRAIN MODELS-----#
#train Random Forest Model
set.seed(123)
rfFit1 <- train(brand~., data = trainSet, method = "rf", trControl=fitControl)
#predictor variables
predictors(rfFit1)
#make predictions
testPredrf <- predict(rfFit1, testSet)
#performance measurement
postResample(testPredrf, testSet$brand)
#plot predicted verses actual
plot(testPredrf, testSet$brand)

#train KNN Model
set.seed(123)
knnFit1 <- train(brand~., data = trainSet, method = "knn", trControl=fitControl)
#predictor variables
predictors(knnFit1)
#make predictions
testPredknn <- predict(knnFit1, testSet)
#performance measurement
postResample(testPredknn, testSet$brand)
#plot predicted verses actual
plot(testPredknn, testSet$brand)


#-----Applying Model to Test Set-----#
#make predictions
testPredrf1 <- predict(rfFit1, ICSurvey)
#performance measurement
postResample(testPredrf1, ICSurvey$brand)
