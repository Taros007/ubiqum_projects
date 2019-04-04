### EXPLORING THE DIAMONDS DATASET ###

# libraries ----

library(tidyverse)
library(modelr)
library(magrittr)
library(caret)

set.seed(123)
# 1st exploration ----

# subseting the data
diamonds_subset <- read.csv2("HACKATHON files/train.csv")

# decision tree to identify the variables related
dt_all <- rpart::rpart(price ~., 
                       data = diamonds_subset)

# visualizing the dt
rattle::fancyRpartPlot(dt_all, cex = 0.7, tweak = 1)

# Observations: carat is the most relevant variable to use it to predict price
plot_priceVScarat <- diamonds %>% 
  ggplot() + 
    geom_point(aes(x = carat, y = price))
# they have a explonential relation. FOr the moment we are not going to modify that


# 1st modalization ----

# using a linear model
mod.lm <- lm(price ~ carat, 
             data = diamonds_subset)

# checking the results
par(mfrow = c(2,2))
plot(mod.lm)
par(mfrow = c(1,1))
# there seems that there are some patterns inside the residuals that our model
# is not explaining well. They are centered in specific places, so it could be 
# that a categorical value can explain them. 


# 2nd exploration ----

# looking for the categorical variable that is explaining more about the model 
dt_categories <- rpart::rpart(price ~ cut + color + clarity, 
                              data = diamonds_subset,
                              control = c(minsplit = 20, cp = 0.004))

# ploting the decision tree
rattle::fancyRpartPlot(dt_categories, cex = 1)
# We are going to use color and clarity for our modalization process

# creating dummy variables 
diamonds_subset <- fastDummies::dummy_cols(.data = diamonds_subset,
                                            select_columns = c("cut", "color", "clarity"), 
                                            remove_first_dummy = F)


# 2nd modalization ----

# creating a lm with new features
mod_caratANDcategories <- lm(price ~ carat + color + clarity, 
                             data = diamonds_subset)

# checking the errors 
par(mfrow = c(2,2))
plot(mod_caratANDcategories)
par(mfrow = c(1,1))
# the errors has a cuadratic shape, so we have to look for a cuadratic relation. 
# In order to use the data into a linear relation model, I have to transform the
# data. Also, there are a several amount of outliers that we should take out.


# 3rd exploration ----

# plting the relation between carat aand price
plot_priceVScarat

# How can I transform a exponantial relation between two variables
ggplot(diamonds_subset) +
  geom_point(aes(x = log(carat), y = log(price)))

# adding a new price and carat tranformation
diamonds_subset %<>% 
  mutate(log_price = log(price), log_carat = log(carat))


# 3rd modalization ----

# lm with transformed price and carat
mod_transformed <- lm(log_price ~ log_carat + color + clarity, 
                      data = diamonds_subset)

# checking the errors
par(mfrow = c(2,2))
plot(mod_transformed)
par(mfrow = c(1,1))

# transform predictions in log to real and error plot
carat_treshold <- 1.90

diamonds_subset %>% 
  add_predictions(model = mod_transformed, var = "log_pred") %>% 
  mutate(pred = exp(log_pred), errors = price - pred) %>% 
  ggplot(aes(x = carat)) + 
    geom_hex(aes(y = price)) +
    geom_smooth(aes(y = pred), color = "goldenrod1", se = F) +
    geom_vline(xintercept=1.90, linetype="dashed", color = "red", size = 1.5) +
    labs(title = "Outliers detection") +
    geom_text(aes(x = carat_treshold, label = "  1.90 carat", y = 25000), 
              colour = "red", text = element_text(size = 20), hjust = 0) +
    geom_text(aes(x = 3.1, label = " model\npredictions", y = 20000),
              colour = "goldenrod1", hjust = 0)
 # With that graph we realize that the errors are being affected for outiers, 
# and it seems that it represents all the values that are bigger than 2.75.


# 4th  exploration ----

# Finding the % of outliers inside the data and taking them out

diamonds_subset %>% 
  filter(carat >= carat_treshold) %>% 
  summarise(OutliersPerc = paste0(round(n()/
                                   nrow(diamonds_subset),4)*100,"%"))
# There is only a 0.06% of the data that are outliers. So we decide to take 
# them out.
diamonds_subset %<>% 
  filter(carat <= carat_treshold)

# 4th modalization ----

# modalization after outliers extraction and normalization
mod_normANDout <- lm(log_price ~ log_carat + color + clarity + cut, 
                     data = diamonds_subset)

# plotting the errors 
par(mfrow = c(2,2))
plot(mod_normANDout)
par(mfrow = c(1,1))

# last modalization ----

# testing and training
training_id <- createDataPartition(y = diamonds_subset$price, 
                                   p = 0.8,
                                   list = F)
training <- diamonds_subset[training_id,]
testing <- diamonds_subset[-training_id,]

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

# checking the results against the testing 
postResample(pred = predict(mod_final, testing),
             obs = testing$log_price)

# saving the model 
save(mod_final, file = "scripts/model.rds")

