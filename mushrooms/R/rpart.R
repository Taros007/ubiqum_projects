
# Load libraries ----------------------------------------------------------
library(tidyverse)
library(magrittr)
library(caret)
library(doParallel)

# Load data ---------------------------------------------------------------
mushData <- read_rds('./input/train.rds')
#mushData %<>% select(-c("odor", contains("veil"), contains("gill"), ring.type, cap.surface, population, habitat))
mushData %<>% select(
  c(
    "class",
    "cap.color",
    "stalk.color.above.ring",
    "stalk.color.below.ring",
    "population",
    "bruises"
  )
)

#possible addback: ring.type

#important: bruises, stalk, ring, spore
#Investigate whether gill.size and gill.color is available in verification data

table(mushData$class, mushData$cap.color, exclude = NULL)

set.seed(541)
# train and test
train_ids <- createDataPartition(y = mushData$class,
                                 p = 0.75,
                                 list = F)
train <- mushData[train_ids, ]
test <- mushData[-train_ids, ]

fit <- rpart(
  class ~ .,
  data = mushData,
  method = "class",
  parms = list(split = "information", loss = matrix(
    c(0, 2, 1, 0), byrow = TRUE, nrow = 2
  )),
  control = rpart.control(usesurrogate = 0, maxsurrogate = 0)
)


# Predicting testset ================================
model <- fit
test$Predictions <- predict(model, test)
test %<>% mutate(Predictions = ifelse(Predictions[, "e"] == 1, "e", "p"))

postResample(factor(test$Predictions), factor(test$class))

confusionMatrix(data = factor(test$Predictions),
                reference = factor(test$class))

fit2 <-
  prune(fit, cp = fit$cptable[which.min(fit$cptable[, "xerror"]), "CP"])

save(fit, file = './output/modelToine4.rds')

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit2)
library(mlr)
train2 <- train %>% 
  mutate(
  class = factor(class),
  cap.color = factor(cap.color),
  # spore.print.color = factor(spore.print.color),
  # stalk.shape = factor(stalk.shape),
  # stalk.root = factor(stalk.root),
  # bruises = factor(bruises)
  )
classif.task = makeClassifTask(data = train2, target = "class", positive = "e")
classif.lrn = makeLearner("classif.knn", predict.type = "prob", fix.factors.prediction = TRUE)
mod = train(classif.lrn, classif.task)
task.pred = predict(mod, newdata = test2)
calculateConfusionMatrix(task.pred)

costsens.task = makeCostSensTask(data = train2, cost = matrix(c(0, 3, 1, 0), byrow = TRUE, nrow = 2
))

save(mod, file = './output/modelToine3.rds')
