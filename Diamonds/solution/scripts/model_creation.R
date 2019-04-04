# GOAL; create a simple script to create the final model 


# libraries ----
if (require(pacman) == FALSE) {
  install.packages('pacman')
}
pacman::p_load(tidyverse,modelr, magrittr, caret)


# import data ----

data <- read.csv2("HACKATHON files/train.csv")



# data quality + pre-process ----

source("scripts/function_preProcess.R")
data <- preProcess_data(data = data)


# modalization ----

# last modalization ----

# testing and training
training_id <- createDataPartition(y = data$price, 
                                   p = 0.8,
                                   list = F)
training <- data[training_id,]
testing <- data[-training_id,]

# defining the variables we want to use
relevant_var <- c("cut_Very Good","cut_Premium","cut_Ideal","cut_Good",
                  "cut_Fair","color_J","color_I","color_F","color_G","color_H",      
                  "color_E","color_D","clarity_SI2","clarity_VS2","clarity_VS1",
                  "clarity_VVS1","clarity_IF","clarity_SI1","clarity_VVS2",
                  "clarity_I1","log_price","log_carat" )

# creating the cross validation 
ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 3,
                     number = 5)

# using the function lm to create de model
mod_final <- train(log_price ~., 
                   data = training[relevant_var], 
                   method = "lm", 
                   trControl = ctrl)


# metrics check ----

# checking the results against the testing 
postResample(pred = predict(mod_final, testing),
             obs = testing$log_price)

# saving the model ----

save(mod_final, file = "scripts/model.rds")
