
# libraries -----

library(dplyr)
library(ggplot2)
library(magrittr)
library(modelr)
library(caret)

# data --------

data <- read.csv2("HACKATHON files/validation.csv")

# pre-process ----

source("scripts/function_preProcess.R")
data <- preProcess_data(data = diamonds)

# applying model ----

# loading the model 
load("scripts/model.rds")

# applying the model and storing the results 
results <- as.data.frame(predict(object = mod_final, newdata = data))
colnames(results) <- "log_pred"

# adding actual prices
results$price <- data$price
results$pred_price <- exp(results$log_pred)
results$carat <- data$carat
results$color <- data$color
results$clarity <- data$clarity
results$cut <- data$cut

# checking results ----

# regression metrics 
postResample(results$price, results$pred_price)

# ploting errors
plot(results$price ~ results$pred_price)
abline(a = 1, b = 1, col = "red")

# detecting the opportunities to buy
results %<>% 
  mutate(errors = price - pred_price) %>%
  arrange(errors) %>% as_tibble()

# detecting where are the errors 
results %>% 
  ggplot() +
    geom_point(aes(x = pred_price, y = price, color = (abs(errors) >=500)))

