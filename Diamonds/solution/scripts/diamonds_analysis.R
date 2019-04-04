# Libraries ----
require(tidyverse)


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



 


