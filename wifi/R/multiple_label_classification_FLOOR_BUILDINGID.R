
# 1.   Provided datasets contain empty values. Will treat them as NAs while loading data into R. This will also reduce many unwanted input features.
# 2.   Combine train and test datasets into one data frame to make our preprocessing and data cleaning easy and effective.
# 3.   Remove features that contain more than 50% NAs.
# 4.   Impute NAs in numerical features with 0 and Nas in categorical features with 'none'.
# 5.   Label encode all the categorical features with 'none' as 0, 'a' as 1, 'b' as 2 and so on. Then convert these features into numerical features.
# 6.   Remove features having variance 0, otherwise they will generate errors when building pricipal componets.
# 7.   By this step, we will have clean data. So, separate train & test data frames from our combined data frame and remove features having variance 0 in both train & test data frames.
# 8.   As our data contains more than 1300 columns, I will generate principal components to perform dimension reduction.
# 9.   Build our final train and test data frames with principal components that can explain 95% variation in the data.
# 10.  Using `mlr` package, build multilabel classification model and make our final predictions on test data.

# Load data ---------------------------------------------------------------
source('./R/data_loading.R')
source('./R/undummy.R')

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(fastDummies)
library(mlr)
library(parallelMap)

# Initiate clusters -------------------------------------------------------
## Starting parallelization in mode=socket with cpus=3
parallelStartSocket(2)

# Sample Data -------------------------------------------------------------
wifiData <- wifiData[sample(1:nrow(wifiData), 10000,
                           replace=FALSE),]

# Select variables --------------------------------------------------------
wifiData %<>% select(-c("LONGITUDE", "LATITUDE", "TIMESTAMP", "RRELATIVEPOSITION", "RELATIVEPOSITION", "SPACEID", "USERID", "PHONEID", "mapLAT", "mapLNG"))
label_vars <- c("BUILDINGID", "FLOOR")

#Create training and testing sets
## 75% of the sample size
set.seed(123)
train_ids <- sample(seq_len(nrow(wifiData)), size = floor(0.75 * nrow(wifiData)))

train <- wifiData[train_ids,]
test <- wifiData[-train_ids,]
train_orig <- train
test_orig <- test

# Remove variables with variance 0 ----------------------------------------
constantVars <- which(apply(train, 2, var)==0)

if(length(constantVars)>0){
  train <- train[,-constantVars]  
  }

train_labels <- train[,label_vars]
train_labels %<>% fastDummies::dummy_cols(remove_first_dummy = F) %>% select(-c("BUILDINGID", "FLOOR"))
train %<>% select(-label_vars)

pc <- prcomp(train, 
             center = TRUE, 
             scale. = TRUE)

#Identify the number of PCs required to explain 95% of variance 
#For that we need to calculate cummulative variance proportions, 
#which is calculation based on variance proportions, 
#which is calculated based on variance of PCs.
pcVar <- (pc$sdev)^2
propVar <- (pcVar/sum(pcVar))*100 #converting the proportions out of 100%
cumVar <- cumsum(propVar)

cat(length(cumVar[cumVar<95])+1,'Principal Components explain 95% of variance')

nPCs <- length(cumVar[cumVar<95])+1

#Build our final train and test principal components with variables that explain maximum variance
trainPC <- as.data.frame(pc$x)
trainPC <- trainPC[,1:nPCs]

testPC <- predict(pc, newdata=test)
testPC <- as.data.frame(testPC)
testPC <- as.data.frame(testPC[,1:nPCs])

#Multilabel classification
# 
# Create a Task: Loading data in the package
# Make a learner: Choosing an algorithm(learner) which learns from task(data)
# Train: Training the model

# When it comes to multilabel classification, there are two approaches in `mlr`.  
# 
# *    Algorithm adaptation methods
# +    In this, we treat the whole problem with a specific algorithm.
# +    Currently the available methods in algorithm adaptation approach are: 
# +    Multivariate random forest (randomForestSRC)
# +    Random ferns multilabel algorithm (rFerns)
# 
# *    Problem transformation methods
# +    In this, we transform the problem, so that simple binary classification algorithms can be applied.
# +    Currently the available methods in problem transformation approach are:
# +    Binary relevance
# +    Classifier chains
# +    Nested stacking
# +    Dependent binary relevance
# +    Stacking
# 
# Selected: Random Forest (algorithm adaptation approach) 

#Model building 
train_labels <- sapply(train_labels, as.logical)
trainPC <- cbind(trainPC, train_labels)

set.seed(2018)
labels <- colnames(train_labels)

#Step 1: Create a task.
data.task <- makeMultilabelTask(id='multi', data = trainPC, target = labels)

#Step 2: Make a learner (using algorithm adaptation method)
learn.rfsrc <- makeLearner("multilabel.randomForestSRC", ntree = 120, importance = TRUE)
learn.rfsrc

#Step 3: Train the model
modelRFSRC <- train(learn.rfsrc, data.task)#, subset = 1:(nrow(trainPC) * 0.8))
modelRFSRC

#Step 4: Do predictions
predRFSRC <- predict(modelRFSRC, task = data.task, subset = (nrow(trainPC) * 0.8 + 1):nrow(trainPC))

#Step 5: Performance check
performance <- performance(predRFSRC, measures = list(multilabel.hamloss, multilabel.subset01, multilabel.f1, multilabel.acc))
accuracy_rfsrc <- performance[4]
accuracy_rfsrc <- round(accuracy_rfsrc*100, 2)

cat('Accuracy:',accuracy_rfsrc)

#Predicting test data
predictions <- predict(modelRFSRC, newdata = testPC)
predictions <- as.data.frame(predictions)
colnames(predictions) <- labels

predictions$BUILDINGID <- undummify(c("BUILDINGID_0",  "BUILDINGID_1", "BUILDINGID_2"), predictions) -1
predictions$FLOOR <- undummify(c("FLOOR_0", "FLOOR_1", "FLOOR_2", "FLOOR_3", "FLOOR_4" ), predictions) -1

measureACC(test[,"BUILDINGID"], predictions[,"BUILDINGID"])
measureACC(test[,"FLOOR"], predictions[,"FLOOR"])

# Stop clusters -----------------------------------------------------------
parallelStop()
