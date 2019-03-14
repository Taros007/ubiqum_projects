

WholeYear = read.csv('./input/WholeYear.csv')

# Required
library(doParallel)

# Find how many cores are on your machine
#detectCores() # Result = Typically 4 to 6

# Create Cluster with desired number of cores. Don't use them all! Your computer is running other processes.
cl <- makeCluster(3)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
#getDoParWorkers() # Result 2


########################################################################

#caret model - Automatic Tuning Grid
#http://topepo.github.io/caret/bytag.html
#model training: http://topepo.github.io/caret/training.html
#model measurement: http://topepo.github.io/caret/other.html
#dataframe = WholeYear
#Y Value = SolarRad

#load library and set seed
library(caret)
set.seed(998)
#create a 20% sample of the data
WholeYear <- WholeYear[sample(1:nrow(WholeYear), 7000,replace=FALSE),]

# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(WholeYear$SolarRad, p = .75, list = FALSE)
training <- WholeYear[inTraining,]
testing <- WholeYear[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train Random Forest Regression model with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)
rfFit1 <- train(SolarRad~., data = training, method = "rf", trControl=fitControl, tuneLength = 1)

#training results
rfFit1


#################################################

#caret model - Automatic Tuning Grid
#http://topepo.github.io/caret/bytag.html
#model training: http://topepo.github.io/caret/training.html
#model measurement: http://topepo.github.io/caret/other.html
#dataframe = WholeYear
#Y Value = SolarRad

#load library and set seed
library(caret)
set.seed(998)

#create a 20% sample of the data
WholeYear <- WholeYear[sample(1:nrow(WholeYear), 7000,replace=FALSE),]

# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(WholeYear$SolarRad, p = .75, list = FALSE)
training <- WholeYear[inTraining,]
testing <- WholeYear[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train Linear Regression model with a tuneLenght = 2 (trains with 2 mtry values for RandomForest)
rfFit2 <- train(SolarRad~., data = training, method = "rf", trControl=fitControl, tuneLength = 2)

#training results
rfFit2

################################

#caret model - Manual Tuning Grid
#http://topepo.github.io/caret/bytag.html
#model training: http://topepo.github.io/caret/training.html
#model measurement: http://topepo.github.io/caret/other.html
#dataframe = WholeYear
#Y Value = SolarRad

#load library and set seed
library(caret)
set.seed(998)

#create a 20% sample of the data
WholeYear <- WholeYear[sample(1:nrow(WholeYear), 7000,replace=FALSE),]

# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(WholeYear$SolarRad, p = .75, list = FALSE)
training <- WholeYear[inTraining,]
testing <- WholeYear[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#dataframe for manual tuning of mtry
rfGrid <- expand.grid(mtry=c(1,2,3))

#train Random Forest Regression model
#note the system time wrapper. system.time()
#this is used to measure process execution time 
system.time(rfFitm1 <- train(SolarRad~., data = training, method = "rf", trControl=fitControl, tuneGrid=rfGrid))

2#training results
rfFitm1

############################################

#caret model - Random Tuning Search
#http://topepo.github.io/caret/bytag.html
#model training: http://topepo.github.io/caret/training.html
#model measurement: http://topepo.github.io/caret/other.html
#dataframe = WholeYear
#Y Value = SolarRad

#load library and set seed
library(caret)
set.seed(998)

#create a 20% sample fo the data
WholeYear <- WholeYear[sample(1:nrow(WholeYear), 7000,replace=FALSE),]

#define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(WholeYear$SolarRad, p = .75, list = FALSE)
training <- WholeYear[inTraining,]
testing <- WholeYear[-inTraining,]

#10 fold cross validation
rfitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1, search = 'random')

#train Random Forest Regression model
rfFitr2 <- train(SolarRad~., data = training, method = "rf", trControl=rfitControl)

#training results
rfFit2

########################################################

#caret models
#http://topepo.github.io/caret/bytag.html
#model training: http://topepo.github.io/caret/training.html
#model measurement: http://topepo.github.io/caret/other.html#dataframe = WholeYear
#Y Value = SolarRad

# Load library and set seed 

library(caret)
set.seed(998)

#create a 20% sample fo the data
WholeYear <- WholeYear[sample(1:nrow(WholeYear), 7000,replace=FALSE),]

# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(WholeYear$SolarRad, p = .75, list = FALSE)
training <- WholeYear[inTraining,]
testing <- WholeYear[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train Linear Regression model
LMFit1 <- train(SolarRad~., data = training, method = "lm", trControl=fitControl)

#check the results
LMFit1


#############################

# Stop Cluster. After performing your tasks, stop your cluster. 
stopCluster(cl)
