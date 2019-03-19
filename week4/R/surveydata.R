# Prepare clusters =================================
library(doParallel)
cl <- makeCluster(3)
registerDoParallel(cl)

# Load libraries =================================
#Import dataset
library(tidyverse)
library(plyr)
library(caret)
library(e1071)

# Load data =================================
surveyData = read.csv('./input/CompleteResponses.csv')

# Transformation =================================
#Transform variables
surveyData$elevel <- as.factor(surveyData$elevel)
surveyData$car <- as.factor(surveyData$car)
surveyData$zipcode <- as.factor(surveyData$zipcode)
surveyData$brand <- factor(surveyData$brand, 
                              levels = c(0,1),
                              labels = c("Acer", "Sony"))


##Bin age, with optimalization for plotting
lower_bound <- c(0,40,60)
surveyData$Age_Level<- findInterval(surveyData$age, lower_bound)
# Create Definition Table
Age_Level <- c(1,2,3)
Age_Brackets <- c("<40", "40-60", "60+")
Age_Table <- data.frame(Age_Level, Age_Brackets)
#Join with surveyData Frame
surveyData <- join(surveyData, Age_Table, by = "Age_Level")
surveyData$Age_Brackets <- as.factor(surveyData$Age_Brackets)
ggplot(surveyData, aes(Age_Brackets, fill = brand)) + 
  geom_bar() +
  ggtitle("Age Bin Distribution")

##Binning of Salary
#source('./R/evenbins.R')
#surveyData$salary_bin <- evenbins(surveyData$salary, 3)

str(surveyData)
summary(surveyData)

#no NAs found

# Initial data exploration=================================

# ##Explore variation
# #First categoricals and ordinals
# ggplot(surveyData) + geom_bar(mapping = aes(x = surveyData$elevel))
# ggplot(surveyData) + geom_bar(mapping = aes(x = surveyData$car))
# ggplot(surveyData) + geom_bar(mapping = aes(x = surveyData$zipcode))
# ggplot(surveyData) + geom_bar(mapping = aes(x = surveyData$brand))
# #Next, numerical
# ggplot(surveyData) + geom_histogram(mapping = aes(x = surveyData$salary))
# ggplot(surveyData) + geom_histogram(mapping = aes(x = surveyData$age))
# ggplot(surveyData) + geom_histogram(mapping = aes(x = surveyData$credit))
##FINDING: most variables have normal variance. No extreme attribute values (outliers)
#Exceptions: brand (many more 1), and two high occurances for age

# ##Explore collinearity and correlation
# library(corrplot)
# corrplot(cor(base::Filter(is.numeric, surveyData)))
# ##No collinearity and correlation between numeric variables
# 
# #Second for categorical and ordinals - vs salary
# ggplot(surveyData) + geom_boxplot(mapping = aes(x = reorder(car, salary, FUN = median), y = salary))
#ggplot(surveyData) + geom_boxplot(mapping = aes(x = reorder(zipcode, salary, FUN = median), y = salary))
#ggplot(surveyData) + geom_boxplot(mapping = aes(x = reorder(zipcode, age, FUN = median), y = age))
 # ggplot(surveyData) + geom_boxplot(mapping = aes(x = reorder(elevel, salary, FUN = median), y = salary))
# 

# ##Explore relations between independent and dependent variable
# #Ordinal and categorical variables first
# ggplot(data = surveyData) + geom_count(mapping = aes(x = car, y = brand))
# ggplot(data = surveyData) + geom_count(mapping = aes(x = zipcode, y = brand))
# ggplot(data = surveyData) + geom_count(mapping = aes(x = elevel, y = brand))
# 
# ggplot(surveyData) + geom_boxplot(mapping = aes(x = reorder(brand, salary, FUN = median), y = salary))
 ggplot(surveyData, aes(x = age, y = salary)) + geom_point() + geom_smooth(method='lm',formula=y~x)
# ggplot(surveyData) + geom_boxplot(mapping = aes(x = reorder(brand, credit, FUN = median), y = credit))
# ##FINDING: brand 1 is being bought by people with higher salaries

#MAKE SCATTER PLOT AGE, SALARY with colors for brand
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(surveyData, aes(x = salary, y = age, color = brand)) + 
  geom_point() +
  scale_color_manual(values = cbbPalette)
  
# Training of model =================================

#Create training and test set with 75%
set.seed(998)
inTraining <- createDataPartition(surveyData$brand, p = .75, list = FALSE)
training <- surveyData[inTraining,]
testing <- surveyData[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", 
                           number = 10, 
                           repeats = 2, 
                           sampling = "down", 
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)

#train Random Forest Regression model
rfFit1 <- train(brand~ salary + as.factor(Age_Level), #+ zipcode + car + elevel,
                data = training,
                method = "rf",
                preProcess = c("center"),
                trControl=fitControl,
                metric = "Kappa",
                tuneLength = 4)

# Predicting testset ================================

#Predict
predictions <- predict(rfFit1, testing)
confusionMatrix(predictions, testing$brand)

postResample(predictions, testing$brand)

#Check important variables
varTun <- varImp(rfFit1)
plot(varTun, main = "Top variance importance")

# Predict Survey incomplete ================================

##Apply model to new dataset
surveyIncom <- read.csv('./input/SurveyIncomplete.csv')

#Repeat preprocessing of data
#Transform variables
surveyIncom$elevel <- as.factor(surveyIncom$elevel)
surveyIncom$car <- as.factor(surveyIncom$car)
surveyIncom$zipcode <- as.factor(surveyIncom$zipcode)

##Bin age, with optimalization for plotting
lower_bound <- c(0,40,60)
surveyIncom$Age_Level<- findInterval(surveyIncom$age, lower_bound)
# Create Definition Table
Age_Level <- c(1,2,3)
Age_Brackets <- c("<40", "40-60", "60+")
Age_Table <- data.frame(Age_Level, Age_Brackets)
#Join with surveyData Frame
surveyIncom <- join(surveyIncom, Age_Table, by = "Age_Level")
surveyIncom$Age_Brackets <- as.factor(surveyIncom$Age_Brackets)
# ggplot(surveyIncom, aes(Age_Brackets, fill = brand)) + 
#   geom_bar() +
#   ggtitle("Age Bin Distribution")
#Predict & merge into file

#Binning of salary - turned out the be ineffective, hence commented out
##source('./R/evenbins.R')
##surveyIncom$salary_bin <- evenbins(surveyIncom$salary, 3)

surveyIncom$brand <- predict(rfFit1, surveyIncom)

#MAKE SCATTER PLOT AGE, SALARY with colors for predicted brand
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(surveyIncom, aes(x = salary, y = age, color = brand)) + 
  geom_point() +
  scale_color_manual(values = cbbPalette)

#Summary of predicted data
summary(surveyIncom)

# Closing actions ================================

#Save predictions
write.csv(surveyIncom, './output/SurveyIncompletePredicted.csv')
write.csv(surveyData, './output/SurveyData.csv')

#Save model to avoid future retraining
#saveRDS(rfFit1, './output/RF.rds')

#Load model again (learning)
#rfFit1 <- readRDS('./output/RF.rds')

# Stop Cluster. 
stopCluster(cl)                   
