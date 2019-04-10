library("ggplot2")
library(caret)
dataSet<-readRDS("train.rds")
newDataSet <- dataSet[!(dataSet$x == 0 | dataSet$y == 0 | dataSet$z == 0), ]
newDataSet <- newDataSet[newDataSet$y != max(newDataSet$y),]
newDataSet <- newDataSet[newDataSet$z != max(newDataSet$z),]
newDataSet <- newDataSet[newDataSet$carat < 2.2,]

dataSet$id <- NULL
newDataSet$depth <- NULL
newDataSet$table <- NULL
newDataSet$logPrice <-log(newDataSet$price)
newDataSet$price <- NULL

scaleVars <- c("x","y", "z","carat")
index <- names(newDataSet) %in% scaleVars
newDataSet[,index]<- scale(newDataSet[,index])

set.seed(321)
inTrain<- createDataPartition(y = newDataSet$logPrice, p = .70, list = FALSE)
trainSet <- newDataSet[inTrain,]
testSet <- newDataSet[-inTrain,]

ctrl1 <- trainControl(method = "repeatedcv", 
                      number=10, 
                      repeats=1
)
tunegrid <- expand.grid(k = c(2,3,4))
knnModel<- train(logPrice ~ ., 
                 data = trainSet, 
                 method = "knn",
                 tuneGrid=tunegrid,
                 trControl = ctrl1,
                 preProc = c("scale"))
knnModel

knnPredictResult <- predict(knnModel, newdata = testSet)
postResample(knnPredictResult,testSet$logPrice)
