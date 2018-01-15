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

skewness(segData$AvgIntenCh1)
apply(segData, 2, skewness) %>% head()

library(caret)
ChiAreaTrans <- BoxCoxTrans(segData$AreaCh1)
ChiAreaTrans
segData$AreaCh1 %>% head()
predict(ChiAreaTrans, segData$AreaCh) %>% head()

pcaObject <- prcomp(segData, center = T, scale. = T)
pcaObject$sdev^2/sum(pcaObject$sdev^2)*100

head(pcaObject$x[, 1:5])

head(pcaObject$rotation[,1:3])
trans <- preProcess(segData,
                    method = c("BoxCox", "center", "scale", "pca"))
trans

transformed <- predict(trans, segData)

nearZeroVar(segData)

correlations <- cor(segData)
correlations[1:4, 1:4]

corrplot(correlations, order = "hclust")

highCorr <- findCorrelation(correlations, cutoff = 0.75)
length(highCorr)
head(highCorr)
filteredSegData <- segData[, -highCorr]

data("cars")
head(cars)
levels(cars$type)

