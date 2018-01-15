library(AppliedPredictiveModeling)
library(e1071)
library(caret)
library(corrplot)
library(dplyr)
library(ipred)

# Labs --------------------------------------------------------------------
data(twoClassData)

set.seed(1)

trainingRows <- createDataPartition(classes, p = 0.8, list = F)
head(trainingRows)

trainPredictors <- predictors[trainingRows,]
trainClasses <- classes[trainingRows]
testPredictors <- predictors[-trainingRows,]
testClasses <- classes[-trainingRows]


repeated_splits <- createDataPartition(classes, p = 0.8, times = 3)



createFolds(trainClasses, k = 10, returnTrain = T)
createMultiFolds(trainClasses, k = 5, time = 2)

trainPredictors <- as.matrix(trainPredictors)
knnFit <- knn3(x = trainPredictors, y = trainClasses, k = 5)
knnFit
predict(knnFit, newdata = testPredictors, type = "class")










