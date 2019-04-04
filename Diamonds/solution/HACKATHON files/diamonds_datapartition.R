# diamonds data partition for the ML challenge

# add column with id
diamonds_id <- diamonds
diamonds_id$id <- seq(1:nrow(diamonds_id))
diamonds_id <- diamonds_id[,c(11, 1:10)]

# validation set
set.seed(98765432)
val_indices <- sample(nrow(diamonds_id), nrow(diamonds_id)/3)
validation <- diamonds_id[val_indices,]
mean(validation$price) #3879.937

# df for mentors to check results
validation_mentor <- validation[c("id", "price")]

validation$price <- NULL
write.csv2(validation, "~/validation.csv", 
           row.names = FALSE)

# train set
train <- diamonds_id[-val_indices,]
mean(train$price) #3959.231
write.csv2(train, "~/train.csv",
           row.names = FALSE)
 
