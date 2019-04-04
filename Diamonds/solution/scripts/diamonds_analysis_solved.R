# # Libraries ----
require(tidyverse)
require(caret)
require(modelr)


# Dataset information ----
?diamonds

# Data insights ----

# The diamonds with a bad cut are in average more expensive
diamonds %>% 
  ggplot(aes(cut, price)) + 
  geom_boxplot()

# The diamonds with a bad color are also more expensive
diamonds %>% 
  ggplot(aes(color, price)) + 
  geom_boxplot()

# And the diamonds with a bad clarity have a higer price
diamonds %>% 
  ggplot(aes(clarity, price)) +
  geom_boxplot()

# Your task ----

# Why are so expensive the diamonds in relation to their quality? We would like to receive a model to predict their price and to know which ones are the most relevant features to do that. 



# Checking the initial assumption ----

# model creation (lm automatically dummify categorical variables)
mod_bq <- lm(price ~ color + cut + clarity,
             data = diamonds)

# checking the errors of the initial assumption
assumption <- diamonds %>% 
  add_predictions(model = mod_bq, var = "pred") %>% 
  add_residuals(model = mod_bq, var = "resd")

# checking the assumption metrics
postResample(assumption$pred, assumption$price)
# RMSE = 3,855.776
# r^2 = 0.06586875
# We can assume that this features are not going to help us explain the price



# 1st hypothesis ----

# The diamonds weight (carat) is affecting the price
diamonds %>% 
  ggplot(aes(carat, price)) + geom_hex(bins = 50)

# There are some ouliers above carat > 2.5
diamonds %>% 
  ggplot(aes(x = carat)) +
    geom_histogram(aes(
      y = ..count../sum(..count..)), bins = 30, color = "blue", fill = "white")

# Outliers extraction (3.5% of all the info)
diamonds2 <- diamonds %>% 
  filter(carat <= 2.5)

# Normalization of the data through logarithm tranformation, very useful if you want to create a linear model
diamonds2 <- diamonds2 %>% 
  mutate(
    lprice = log2(price), 
    lcarat = log2(carat))

# Plot of the linear relation btwn lcarat and lprice
diamonds2 %>%
  ggplot(aes(lcarat, lprice)) +
  geom_hex(bins = 50)

# First model creation
mod_diamonds <- lm(lprice ~ lcarat, data = diamonds2)

# Adding predictions
diamonds2 <- diamonds2 %>% 
  add_predictions(model = mod_diamonds,
                  var = "lpred1") %>% 
  mutate(pred1 = 2^lpred1)

# Metrics check
postResample(diamonds2$price, diamonds2$pred1)

# Model visualization
diamonds2 %>% 
  ggplot(aes(carat, price)) +
    geom_hex(bins = 50) +
    geom_line(aes(y = pred1), 
              color = "red", size = 1)
# Insight: our model is predicting for a carat > 2.25 (aprox) the diamonds should be more expensive, but they never goes up from 20.000$. So, the biggest diamands are relatively less expensive

# Addition and error visualization
diamonds2 <- diamonds2 %>% 
  add_residuals(model = mod_diamonds,
                var = "rpred1")
diamonds2 %>% 
  ggplot(aes(lcarat, rpred1)) +
    geom_hex(bins = 50)
# We are missing to explain some patterns, there are linear relations between the errors

# Plotting the errors against other variables. In this step we want to find patterns of our errors in relation to other variables (clarity, cut and color). If our errors are negative, it means that our model is predicting a higher price for this observations. If they are positive, it means it is predicting a lower price. (residuals = real - prediction)
# cut
diamonds2 %>%
  ggplot(aes(cut, rpred1)) +
  geom_boxplot()
# Our model is giving a higher price for a fair cut, we are missing to explain that
# color
diamonds2 %>%
  ggplot(aes(color, rpred1)) +
  geom_boxplot()
# model is overpricing the bad colors, (J the worst), missing to explain that pattern
# clarity
diamonds2 %>%
  ggplot(aes(clarity, rpred1)) +
  geom_boxplot()
# We can see a clear pattern that is not being explained by out model, the worst clarity is overpriced, and the best, underpriced. clarity is a clear feature that affects our price. 



# Hypothesis 2: carat and clarity are the main factors to explaing the price, but cut and color can also explain some patterns. 

# model creation
mod_diamonds2 <- lm(lprice ~ lcarat + clarity + color + cut,
                    data = diamonds2)

# Adding predictions and errors
diamonds2 <- diamonds2 %>% 
  add_predictions(model = mod_diamonds2, 
                  var = "lpred2") %>% 
  mutate(pred2 = round(2^lpred2)) %>% 
  add_residuals(model = mod_diamonds2,
                var = "rpred2")

# checking metrics
postResample(diamonds2$price, diamonds2$pred2)

# Error evaluation
ggplot(diamonds2, aes(lcarat, rpred2)) +
  geom_hex(bins = 50)
# Insights: residuals = price - pred. Negative values it means they should have a higher price in relation to a model ()
diamonds2 %>% 
  filter(abs(rpred2) > 1) %>% 
  select(price, pred2, carat, clarity, cut, color) %>% 
  arrange(desc(pred2))# good opportunity to buy if you have 10.000$




# Conclusion
# We can not understand the quality of a diamond without considering the carat, which is the most relevant variable

