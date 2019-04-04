# This script automatically pre-process all the data to prepare it to run the model 

preProcess_data <- function(data) {
  # libraries ----
  if (require(pacman) == FALSE) {
    install.packages('pacman')
  }
  pacman::p_load(fastDummies,dplyr, magrittr)
  
  # pre-process ----
  
  # pre-process ----
  
  # dummify variables
  data <- dummy_cols(.data = data,
                      select_columns = c("cut", "color", "clarity"), 
                      remove_first_dummy = F)
  
  # logarithm transformation
  data %<>% 
    mutate(log_price = log(price), log_carat = log(carat))
  
  # outliers
  data %<>%
    filter(carat <= 1.95)
  
}

