#Load readr
library("readr")
library("ggplot2")

#Load cars.csv with readr
cars <- read.csv('./input/cars.csv')

#Transform name.of.car into character
cars$name.of.car <- as.character(cars$name.of.car)

#Set seed, which is start number for randomizing. This helps reproducing results
set.seed(123)

# Create size variables for train/test datasets (70/30%)
trainSize<-round(nrow(cars)*0.7) 
testSize<-nrow(cars)-trainSize

#Randomly sample the training set, and then create the actual sets
training_indices<-sample(seq_len(nrow(cars)),size =trainSize)
trainSet<-cars[training_indices,]
testSet<-cars[-training_indices,]

#Build lineair model
lmmodel <- lm(distance.of.car ~ speed.of.car, trainSet)

#Predict the training set
pred <- predict(lmmodel, testSet)

#Plot graph using ggplot, including lm
g <- ggplot(cars, aes(x=cars$speed.of.car, y = cars$distance.of.car)) + geom_point() + geom_smooth(method="lm", se = FALSE) + labs(title = "Car speed vs car distance", y = "Car distance", x = "Car speed")
plot(g)
