library(AppliedPredictiveModeling)
library(e1071)
library(caret)
library(corrplot)
library(dplyr)


# Labs --------------------------------------------------------------------

data("segmentationOriginal")
segData <- subset(segmentationOriginal, Case == "Train")

cellID <- segData$Cell
class <- segData$Class
case <- segData$Case

segData <- segData[,-(1:3)]
segData <- segData %>%
  select(-contains("Status"))












