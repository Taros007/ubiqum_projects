# Libraries ----
if (require(tidyverse) == FALSE) {
  install.packages('pacman')
}
pacman::p_load(tidyverse)

# import data 
training <- read_csv2('HACKATHON files/train.csv')

# Dataset information ----
?diamonds

# Data insights ----

# The diamonds with a bad cut are in average more expensive
training %>% 
  ggplot(aes(cut, price)) + 
    geom_boxplot()

# The diamonds with a bad color are also more expensive
training %>% 
  ggplot(aes(color, price)) + 
    geom_boxplot()

# And the diamonds with a bad clarity have a higer price
training %>% 
  ggplot(aes(clarity, price)) +
    geom_boxplot()

# Your task ----

# Why the diamonds that have a fair cut, bad color and a bad clarity are, in median, more expensive? We would like to receive a model to predict their price and to know which ones are the most relevant features to do that. 



 


