library(utiml)

# Create two partitions (train and test) of toyml multi-label dataset
ds <- create_holdout_partition(toyml, c(train=0.65, test=0.35))

# Create a Binary Relevance Model using e1071::svm method
brmodel <- br(ds$train, "KNN", seed=123)

# Predict
prediction <- predict(brmodel, ds$test)

# Show the predictions
head(as.bipartition(prediction))
head(as.ranking(prediction))

# Apply a threshold
newpred <- rcut_threshold(prediction, 2)

# Evaluate the models
result <- multilabel_evaluate(ds$tes, prediction, "bipartition")
thresres <- multilabel_evaluate(ds$tes, newpred, "bipartition")

# Print the result
print(round(cbind(Default=result, RCUT=thresres), 3))


model <- mlknn(ds$train, k=3)
pred <- predict(model, ds$test)
# Evaluate the models
result <- multilabel_evaluate(ds$tes, pred, "bipartition")
