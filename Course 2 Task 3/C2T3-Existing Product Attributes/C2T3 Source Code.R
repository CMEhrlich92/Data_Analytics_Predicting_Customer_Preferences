#dataframe = epa
#Y Value = Volume
set.seed(998)

#Install/Load Caret
install.packages("caret")
install.packages("corrplot")
install.packages("readr")
require(caret)
require(corrplot)
require(mlbench)
require(readr)
library(caret)
library(corrplot)

#Import new and existing product attribute files
npa <- read.csv("new product attributes.csv")
epa <- read.csv("existing product attributes.csv")

#Check Dataset- Cleaning and Preprocessing
str(epa)
summary(epa)

# dummify the data
dmy <- dummyVars(" ~ .", data = epa)
readyData <- data.frame(predict(dmy, newdata = epa))
readyData
str(epa)
str(readyData)
summary(readyData)

#Feature Engineering- corrplot, corr, dim red, collinearity.
corrData <- cor(readyData)
corrData
corrplot(corrData)

#Remove 5 and 3 star review atributes because of their high correlation with dependent variable Volume
readyData$x5StarReviews <- NULL
readyData$x3StarReviews <- NULL

#Create Training and Testing sets
inTraining <- createDataPartition(epa$Volume, p = .75, list = FALSE)
training <- epa[inTraining,]
testing <- epa[-inTraining,]
set.seed(998)

#---Linear Regression Model---#
#LM model
LMFit1 <- lm(Volume~., training)
LMFit1


#---Random Forest---#
#Install RF Package
install.packages("randomForest")
library(randomForest)

#Create Training and Testing sets
trainSize <- round(nrow(readyData)*0.75)
testSize <- nrow(readyData)-trainSize
trainSize
testSize
training_indices <- sample(seq_len(nrow(readyData)),size = trainSize)
trainSet <- readyData[training_indices,]
testSet <- readyData[-training_indices,]

#Random Forest Model
rfFit1 <- randomForest(Volume~., trainSet, ntree = 500)
rfFit1

#Random Forest Predictions
rfPred <- predict(rfFit1,testSet)
rfPred
summary(rfPred)


#---Support Vector Machine---#
#e1071 package for SVM model
library(e1071)

#SVM Model
svmFit1 <- svm(Volume~., data = trainSet, kernel = "linear", cost = .1, scale = FALSE)
svmFit1

#SVM Predictions
svmPred <- predict(svmFit1,testSet)
svmPred
summary(svmPred)


#---Predicting New Products Data---#
#Dummify variables in newproductattributes dataset
NewProd <- dummyVars("~.", data = npa)
readyData2 <- data.frame(predict(NewProd, newdata = npa))
readyData2
str(npa)
str(readyData2)
summary(readyData2)

#Remove attributes in newproductattributes that were also removed in existingproductattributes
readyData2$x5StarReviews <- NULL
readyData2$x3StarReviews <- NULL

#Predict randomForest, preferred model
finalPred <- predict(rfFit1, readyData2)
finalPred

#Write predictions to new CSV file which also takes all existing data from ReadyExisProd2 (newproductattributes data)
output <- readyData2
output$predictions <- finalPred
write.csv(output, file="C2.T3output.csv", row.names = TRUE)
