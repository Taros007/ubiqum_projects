library(caret)
library(tidyverse)
library(magrittr)
library(doParallel)

#Read the input File ---------------------------------------------------------
dataSet <- read.csv("input/iphone_smallmatrix_labeled_8d.csv",
                    header = TRUE, 
                    sep = ",")

# iphone selection --------------------------------------------------------

# selection of iphone to study its performance
iphone_set <- dataSet %>% select(starts_with("iphone"))


# train and test ----------------------------------------------------------

set.seed(107)
train_id <- createDataPartition(
  y = iphone_set$iphonesentiment,
  p = 0.8, 
  list = F
)
training <- iphone_set[train_id,]
testing <- iphone_set[-train_id,]


# pre process -------------------------------------------------------------

# unique observations
training %>% 
  distinct_all() %>% 
  nrow()
training %<>%
  distinct_all()

# check balance data
iphone_analysis <- training %>% 
  group_by(iphone_sentiment = factor(iphonesentiment)) %>% 
  summarise (n = n()) %>%
  mutate(freq = paste(round(n / sum(n),2)*100,"%")) 
iphone_analysis


# great! now let's try to balance our cleaned data
obs_iphone <- iphone_analysis$n[1:length(iphone_analysis$n)-1]
sentiments <- seq(0,5,1)

iphone_balanceSet <- training
dummy_set <- training

for (j in 1:length(obs_iphone)) {
  for (i in 1:round(914/obs_iphone[j])) {
    temp <- dummy_set %>% 
      filter(iphonesentiment == sentiments[j])
    iphone_balanceSet <- temp %>% 
      bind_rows(iphone_balanceSet)
  }
}

#Load clusters
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

# create model
mod_iphone_filtered <- train(
  factor(iphonesentiment) ~., 
  data = iphone_balanceSet,
  method = "C5.0"
)

# Stop Cluster. 
stopCluster(cl)

# check the metrics
results_mod <- predict(
  object = mod_iphone_filtered,
  newdata = testing
)
confusionMatrix(
  data = results_mod ,
  reference = factor(testing$iphonesentiment),
  dnn = c("Predictions","Reference")
)
