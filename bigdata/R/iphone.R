
# Load libraries ----------------------------------------------------------
library(tidyverse)
library(corrr)
library(caret)
library(magrittr)
library(doParallel)

# Load data ---------------------------------------------------------------
iphoneData <- read_csv('./input/iphone_smallmatrix_labeled_8d.csv')
galaxyData <- read_csv('./input/galaxy_smallmatrix_labeled_8d.csv')


# Explore data ------------------------------------------------------------

iphoneData %>% 
  ggplot(aes(x = iphonesentiment)) +
  geom_histogram()

#Detect missing values -> none
sapply(iphoneData, function(x) sum(is.na(x)))

# Explore correlation -----------------------------------------------------

rs <- correlate(iphoneData)

rs %>%
  focus(iphonesentiment) %>%
  mutate(rowname = reorder(rowname, iphonesentiment)) %>%
  ggplot(aes(rowname, iphonesentiment)) +
  geom_col() + coord_flip()

correlate(iphoneData) %>%
  rearrange() %>% 
  shave(upper = FALSE) %>% 
  #fashion() %>% 
  rplot()


# Select variables --------------------------------------------------------
iphoneCS <- iphoneData %>%
  select(c("iphone",
           "ios",
           "iphonecampos",
           "iphonecamneg", 
           "iphonecamunc", 
           "iphonedispos", 
           "iphonedisneg",
           "iphonedisunc",
           "iphoneperpos",
           "iphoneperneg",
           "iphoneperunc",
           "iosperpos",
           "iosperneg",
           "iosperunc",
           "iphonesentiment")
  )

saveRDS(iphoneCS, './output/iphoneCS.rds')

iphoneCS %>% 
  ggplot(aes(y = iphone, x = iphonesentiment)) +
  geom_boxplot()


iphoneCS %<>% mutate(
  iphonesentiment = factor(iphonesentiment)
) 

#iphoneCS %<>% filter(iphone == 0)
iphoneCS <- unique(iphoneCS)
iphoneCS$sum <- rowSums(iphoneCS %>% select(-c(iphonesentiment)))
iphoneCS %<>% filter(sum != 0) %>% select(-"sum")

iphoneCS$sum %>% histogram

iphoneCS %>% 
  ggplot(aes(x = iosperpos)) +
  geom_histogram(stat = "count")

# Variance selection ------------------------------------------------------

# nearZeroVar() with saveMetrics = FALSE returns an vector
nzv <- nearZeroVar(iphoneData, saveMetrics = FALSE)
nzv

# create a new data set and remove near zero variance features
iphoneNZV <- iphoneData[,-nzv]
str(iphoneNZV)

saveRDS(iphoneNZV, './output/iphoneNZV.rds')

# RFE ---------------------------------------------------------------------

#Load clusters
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

# Let's sample the data before using RFE
set.seed(123)
iphoneSample <- iphoneData[sample(1:nrow(iphoneData), 1000, replace=FALSE),]

# Set up rfeControl with randomforest, repeated cross validation and no updates
ctrl <- rfeControl(functions = rfFuncs, 
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

# Use rfe and omit the response variable (attribute 59 iphonesentiment) 
rfeResults <- rfe(iphoneSample[,1:58],
                  iphoneSample$iphonesentiment,
                  sizes=(1:58),
                  rfeControl=ctrl)


# Stop Cluster. 
stopCluster(cl)

# Get results
rfeResults

# Plot results
plot(rfeResults, type=c("g", "o"))

# create new data set with rfe recommended features
iphoneRFE <- iphoneData[,predictors(rfeResults)]

# add the dependent variable to iphoneRFE
iphoneRFE$iphonesentiment <- iphoneData$iphonesentiment

#Save objects to avoid recalculation
saveRDS(rfe, './output/rfe_iphone.rds')
saveRDS(iphoneRFE, './output/iphoneRFE.rds')
