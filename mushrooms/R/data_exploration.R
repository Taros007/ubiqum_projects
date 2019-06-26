
# Load libraries ----------------------------------------------------------
library(tidyverse)
library(magrittr)
library(caret)
library(doParallel)

# Load data ---------------------------------------------------------------
mushData <- read_rds('./input/train.rds')
mushData %<>% select(c("class", "cap.color"))

#veil.color - not very much difference
#veil.
#important: bruises, stalk, ring, spore
#Investigate whether gill.size and gill.color is available in verification data


table(mushData$class, mushData$gill.attachment)


#create weights matrix
matrix_weights <- list(c("e", "p"), c("e", "p"))
names(matrix_weights) <- c("predicted", "actual")
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2, dimnames = matrix_weights)

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

set.seed(541)
# train and test
train_ids <- createDataPartition(y = mushData$class,
                                 p = 0.75,
                                 list = F)
train <- mushData[train_ids,]
test <- mushData[-train_ids,]

# cross validation
ctrl <- trainControl(method = "repeatedcv",
                     number = 4,
                     repeats = 1,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE
)

rfFit1 <- caret::train(class~ .,
                       data = train,
                       method = "rpart",
                       trControl=ctrl,
                       metric = "ROC"
                       #preProcess = c("scale", "center") #only used for distance-modelling techniques (knn, SVM)
)

# Stop Cluster. 
stopCluster(cl)

fit <- rpart(class ~ ., data=mushData, method="class", 
             parms=list(split="information", loss=matrix(c(0,4,1,0), byrow=TRUE, nrow=2)), 
             control=rpart.control(usesurrogate=0,maxsurrogate=0))


# Predicting testset ================================
model <- fit
test$Predictions <- predict(model, test)
postResample(factor(test$Predictions), factor(test$class))

confusionMatrix(data = factor(test$Predictions), reference = factor(test$class))
