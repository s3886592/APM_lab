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

library(caret)
ChiAreaTrans <- BoxCoxTrans(segData$AreaCh1)
ChiAreaTrans
segData$AreaCh1 %>% head()
predict(ChiAreaTrans, segData$AreaCh) %>% head()

# Exercises ---------------------------------------------------------------

# 3.1
library(mlbench)
data(Glass)
str(Glass)

Glass_label <- Glass[,10]
Glass_pred <- Glass[,-10]

# check skewness
apply(Glass_pred, 2, skewness) %>% plot() # the skewness is high, a data transformation is needed

# check if PCA is needed
pcaObject <- prcomp(Glass_pred,
                    center = T, scale. = T)

pcaObject$sdev^2/sum(pcaObject$sdev^2)*100 # the last 4 PC variance is small

# pre-processing
trans <- preProcess(Glass_pred, 
                    method = c("BoxCox", "center", "scale")) # without PCA

transformed <- predict(trans, Glass_pred)

# check the responses
library(corrplot)
data_cor <- cor(transformed)
corrplot(data_cor, order = "hclust") # correlation is not strong

# Visualizatoin
data_joint <- cbind(transformed, Glass_label)
featurePlot(x = data_joint[, 1:7], 
            y = data_joint$Glass_label, 
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))


# 3.2
library(mlbench)
data("Soybean")
?Soybean

# check the distribution 
apply(Soybean, 2, function(x) sum(is.na(x))/length(x)*100) %>% plot

# 3.3
data(BloodBrain)

str(bbbDescr)
library(corrplot)
data_cor <- cor(bbbDescr)
corrplot(data_cor, order = "hclust") 

# pre-processing to remove correlation using PCA
trans <- preProcess(bbbDescr, 
                    method = c("BoxCox", "center", "scale", "pca")) # without PCA

transformed <- predict(trans, bbbDescr)

# Remove correlated predictors
highCorr <- findCorrelation(bbbDescr, cutoff = 0.75)
filtered <- bbbDescr[, -highCorr]




