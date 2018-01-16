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

# parameter tuning
library(caret)
data("GermanCredit")

classes <- GermanCredit %>%
  select(Class) %>%
  unlist()

trainingRows <- createDataPartition(classes, p = 0.8, list = F)
data_train <- GermanCredit[trainingRows,]
data_test <- GermanCredit[-trainingRows,]

svmFit <- train(Class ~ ., 
                data = data_train,
                method = "svmRadial",
                preProc = c("center","scale"),
                tuneLength = 10,
                trControl = trainControl(method = "repeatedcv",
                                         repeats = 5))


plot(svmFit, scales = list(x = list(log = 2)))

predict(svmFit, newdata = data_test, type = "prob")

# between model comparison

logisticReg <- train(Class ~ .,
                     data = data_train,
                     method = "glm",
                     trControl = trainControl(method = "repeatedcv", repeats = 5))

logisticReg

resamp <- resamples(list(SVM = svmFit, Logistic = logisticReg))
summary(resamp)

# Exercise ----------------------------------------------------------------

# 4.1
# Use CV

# 4.2
data("permeability")

hist(permeability)

# 4.3
library(AppliedPredictiveModeling)
library(caret)
library(tidyverse)

data("ChemicalManufacturingProcess")

# Remove NA in observation
ChemicalManufacturingProcess <-
  ChemicalManufacturingProcess[, unlist(apply(ChemicalManufacturingProcess, 2,
                                     function(x)
                                       sum(is.na(x)) == 0))]

plsProfileChemMod <- train(Yield ~ .,
                           data = ChemicalManufacturingProcess,
                           method = "pls",
                           preProc = c("center", "scale"),
                           tuneLength = 10,
                           trControl = trainControl(method = "repeatedcv", repeats = 5))


R2values <- plsProfileChemMod$results[, c("ncomp", "Rsquared", "RsquaredSD")]
R2values$RsquaredSEM <- R2values$RsquaredSD/sqrt(length(plsProfileChemMod$control$index))

ggplot(R2values, aes(ncomp, ymin = Rsquared - RsquaredSEM,ymax = Rsquared)) +
  geom_errorbar() +
  theme_bw()

bestR2 <- subset(R2values, ncomp == which.max(R2values$Rsquared))
R2values$tolerance <- (R2values$Rsquared - bestR2$Rsquared)/bestR2$Rsquared * 100


# 4.4
data(oil)

fattyAcids
table(oilType)

barplot(table(oilType))

sample(oilType,size = 60) %>% table() %>% barplot()






