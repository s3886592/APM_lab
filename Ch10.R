library(AppliedPredictiveModeling)
library(caret)
library(tidyverse)
library(plyr)
library(doParallel)

cl <- makeCluster(detectCores())
registerDoParallel(cl)

data(concrete)

featurePlot(x = concrete[, -9],
            y = concrete$CompressiveStrength,
            between = list(x = 1, y = 1),
            type = c("g","p", "smooth"))

averaged <- ddply(mixtures,
                  .(
                    Cement,
                    BlastFurnaceSlag,
                    FlyAsh,
                    Water,
                    Superplasticizer,
                    CoarseAggregate,
                    FineAggregate,
                    Age
                  ),
                  function(x)
                    c(CompressiveStrength =
                        mean(x$CompressiveStrength)))

set.seed(975)

inTrain <- createDataPartition(averaged$CompressiveStrength, p = 3/4)[[1]]

training <- averaged[ inTrain,]
testing  <- averaged[-inTrain,]

ctrl <- trainControl(method = "repeatedcv", repeats = 5, number = 10)

modForm <- paste("CompressiveStrength ~ (.)^2 + I(Cement^2) + I(BlastFurnaceSlag^2) +",
                 "I(FlyAsh^2)  + I(Water^2) + I(Superplasticizer^2)  +",
                 "I(CoarseAggregate^2) +  I(FineAggregate^2) + I(Age^2)")
modForm <- as.formula(modForm)

controlObject <- trainControl(method = "repeatedcv",
                              repeats = 5,
                              number = 10)

set.seed(669)
lmFit <- train(modForm, data = training,
                               method = "lm",
                               trControl = ctrl)

lmFit

set.seed(669)
lsFit <- train(
  modForm,
  data = training,
  method = "pls",
  preProc = c("center", "scale"),
  tuneLength = 15,
  trControl = ctrl
)
plsModel

lassoGrid <- expand.grid(lambda = c(0, .001, .01, .1),
                         fraction = seq(0.05, 1, length = 20))
set.seed(669)
lassoFit <- train(
  modForm,
  data = training,
  method = "enet",
  preProc = c("center", "scale"),
  tuneGrid = lassoGrid,
  trControl = ctrl
)

set.seed(669)
earthFit <- train(
  CompressiveStrength ~ .,
  data = training,
  method = "earth",
  tuneGrid = expand.grid(degree = 1,
                         nprune = 2:25),
  trControl = ctrl
)

set.seed(669)
svmRFit <- train(
  CompressiveStrength ~ .,
  data = training,
  method = "svmRadial",
  tuneLength = 15,
  preProc = c("center", "scale"),
  trControl = ctrl
)
svmRFit

nnetGrid <- expand.grid(
  decay = c(0.001, .01, .1),
  size = seq(1, 27, by = 2),
  bag = FALSE
)

set.seed(669)
nnetFit <- train(
  CompressiveStrength ~ .,
  data = training,
  method = "avNNet",
  tuneGrid = nnetGrid,
  preProc = c("center", "scale"),
  linout = TRUE,
  trace = FALSE,
  maxit = 1000,
  allowParallel = FALSE,
  trControl = ctrl
)

set.seed(669)
rpartFit <- train(CompressiveStrength ~ .,
                                     data = training,
                                     method = "rpart",
                                     tuneLength = 30,
                                     trControl = ctrl)

set.seed(669)
treebagFit <- train(CompressiveStrength ~ .,
                                         data = training,
                                         method = "treebag",
                                         trControl = ctrl)

set.seed(669)
ctreeFit <- train(
  CompressiveStrength ~ .,
  data = training,
  method = "ctree",
  tuneLength = 10,
  trControl = ctrl
)

set.seed(669)
rfFit <- train(
  CompressiveStrength ~ .,
  data = training,
  method = "rf",
  tuneLength = 10,
  ntrees = 1000,
  importance = TRUE,
  trControl = ctrl
)

rfFit

set.seed(669)

gbmFit <- train(
  CompressiveStrength ~ .,
  data = training,
  method = "gbm",
  tuneLength = 10,
  verbose = FALSE,
  trControl = ctrl
)
gbmFit

cbGrid <- expand.grid(committees = c(1, 5, 10, 50, 75, 100),
                      neighbors = c(0, 1, 3, 5, 7, 9))

set.seed(669)
cbFit <- train(
  CompressiveStrength ~ .,
  data = training,
  method = "cubist",
  tuneGrid = cbGrid,
  trControl = ctrl
)
cbFit

stopCluster(cl)
mtFit <- train(CompressiveStrength ~ .,
                              data = training,
                               method = "M5",
                               trControl = ctrl)

earthFit <- train(
  CompressiveStrength ~ .,
  data = training,
  method = "earth",
  tuneLength = 10,
  trControl = ctrl
)

rs <- resamples(
  list(
    "Linear Reg" = lmFit,
    "
    PLS" = plsFit,
    "Elastic Net" = lassoFit,
    MARS = earthFit,
    SVM = svmRFit,
    "Neural Networks" = nnetFit,
    CART = rpartFit,
    "Cond Inf Tree" = ctreeFit,
    "Bagged Tree" = treebagFit,
    "Boosted Tree" = gbmFit,
    "Random Forest" = rfFit,
    Cubist = cbFit
  )
)
parallelplot(rs)
parallelplot(rs, metric = "Rsquared")


nnetPred <- predict(nnetFit, testing)
gbmPred <- predict(gbmFit, testing)
cbPred <- predict(cbFit, testing)

testResults <- rbind(postResample(nnetPred, testing$CompressiveStrength),
                                           postResample(gbmPred, testing$CompressiveStrength),
                                           postResample(cbPred, testing$CompressiveStrength))

testResults <- as.data.frame(testResults)
testResults$Model <- c("Neural Networks", "Boosted Tree", "Cubist")
testResults <- testResults[order(testResults$RMSE),]



