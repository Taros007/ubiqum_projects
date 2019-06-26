source('./R/run_model.R')

iphoneRFE <- readRDS('./output/iphoneRFE.rds')
iphoneCS <- readRDS('./output/iphoneCS.rds')

set.seed(621)

dataset <- iphoneCS
modeltype <- "xgbTree"
dependant <- "iphonesentiment"
  
# train and test
train_ids <- createDataPartition(y = dataset[[dependant]],
                                 p = 0.75,
                                 list = F)
train <- dataset[train_ids,]
test <- dataset[-train_ids,]
 
model <- run_model(train, test, modeltype, dependant)
model$model

 # find the errors
mod_errors <- train %>% 
  add_predictions(model = model$model, var = "pred") %>% 
  filter(iphonesentiment != pred)

mod_errors %>% 
  ggplot(aes(x = pred)) +
  geom_histogram(stat = "count")

# adding the errors in our data
train <- train %>%
  bind_rows(mod_errors) %>% select(-pred)

##Alternative upsampling
iphoneCS <- bind_rows(iphoneCS, iphoneCS %>% filter(iphonesentiment %in% c(1, 2, 3))) 

