##installs readr package, but as this is already installed I have disabled it
#install.packages(readr)

#loads readr & ggplot2 package
library("readr")
library("ggplot2")

#adjusted to load from the right location in my project folder + added quotes
IrisDataset <- read.csv('./input/iris.csv')

#The statements below just print information
attributes(IrisDataset)
#Corrected a typo in variable name
summary(IrisDataset) 
#Corrected a type in variable name
str(IrisDataset)
names(IrisDataset)

#Not sure what we're trying to do here. Barplot would seem more appropriate for a categorical variable
#Prior to the hist statement, we need to covert the Species column to numeric
# IrisDataset$Species <- as.numeric(IrisDataset$Species)
# hist(IrisDataset$Species)

#Corrected a missing ')', and changed to Petal
plot(IrisDataset$Petal.Length)

#Added a variable to the dataframe to plot , and changed to Petal    
qqnorm(IrisDataset$Petal.Length)
   
#No changes
IrisDataset$Species<- as.numeric(IrisDataset$Species) 
set.seed(123)

#Training set seems a bit low, leaving it as is for now     
trainSize <- round(nrow(IrisDataset) * 0.7)
#changed variable from trainSet to trainSize
testSize <- nrow(IrisDataset) - trainSize

#Corrected typo  
trainSize
testSize

#Create new line to create training_indices
training_indices<-sample(seq_len(nrow(IrisDataset)),size =trainSize)
   
trainSet <- IrisDataset[training_indices, ]
testSet <- IrisDataset[-training_indices, ]

# #Useless command, as no more random commands are pasted after
# set.seed(405)
# 
# #Useless commands, already performed     
# trainSet <- IrisDataset[training_indices, ]
# testSet <- IrisDataset[-training_indices, ]

#Corrected testingSet in trainSet, as the model need to train on its own observations. 
#Furthermore, needed to add the trainSet separate to avoid warning message while predicting
#Also, the dependent variable should be Length, not width.
LinearModel<- lm(Petal.Length ~ Petal.Width, trainSet)

summary(LinearModel)

#Added comma
prediction<-predict(LinearModel, testSet)

#Corrected typo     
prediction

#Added csv write
write.csv(prediction, "./output/irispred.csv")

#Create scatter plot
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
g <- ggplot(IrisDataset, aes(x=Petal.Width, y = Petal.Length)) +
  geom_point(aes(colour=as.factor(Species))) + 
  geom_smooth(method="lm", se = FALSE) + 
  labs(title = "Petal.width(X) vs. Petal.Length(y)", y = "Petal.Length", x = "Petal.width") + 
  scale_color_manual(values = cbbPalette)
plot(g)
