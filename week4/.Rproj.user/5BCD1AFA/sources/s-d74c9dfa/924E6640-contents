#import libraries
library("readr")
library("dplyr")
library("tidyr")
library("ggplot2")

#import data
plantsData <- read.csv('./input/exercise.csv')

#remove observations with NA in 'Species'
plantsData <- plantsData[!is.na(plantsData$Species),]

summary(plantsData)

#Replace NA observations for non-categorical values with median
#cM <- colMeans(plantsData[1:4], na.rm=TRUE)
cM <- apply(plantsData[1:4], 2, median, na.rm = TRUE)
indx <- which(is.na(plantsData[1:4]), arr.ind=TRUE)
plantsData[1:4][indx] <- cM[indx[,2]]
#Alternative: #plantsData[is.na(plantsData$Sepal.Width)] <- median(plantsData$Sepal.Width, na.rm = TRUE)

boxplot(plantsData)

#METHOD 1 OUTLIERS
outliers <- boxplot(plantsData$Sepal.Length, plot = FALSE)$out
plantsData <- plantsData[-which(plantsData$Sepal.Length %in% outliers),]
outliers <- boxplot(plantsData$Sepal.Width, plot = FALSE)$out
plantsData <- plantsData[-which(plantsData$Sepal.Width %in% outliers),]
outliers <- boxplot(plantsData$Petal.Length, plot = FALSE)$out
plantsData <- plantsData[-which(plantsData$Petal.Length %in% outliers),]
outliers <- boxplot(plantsData$Petal.Width, plot = FALSE)$out
plantsData <- plantsData[-which(plantsData$Petal.Width %in% outliers),]

#METHOD 2 OUTLIERS (just the maximum value)
 # plantsData <- plantsData[-which(plantsData$Sepal.Length == max(plantsData$Sepal.Length)),]
 # plantsData <- plantsData[-which(plantsData$Sepal.Width == max(plantsData$Sepal.Width)),]
 # plantsData <- plantsData[-which(plantsData$Petal.Length == max(plantsData$Petal.Length)),]
 # plantsData <- plantsData[-which(plantsData$Petal.Width == max(plantsData$Petal.Width)),]
 
#Plot with both boxplot and ggplot (from stack overflow)
 boxplot(plantsData[,1:4])
 plantsData %>% dplyr::select(Species, everything()) %>% tidyr::gather("id", "value",2:5) %>% 
  ggplot(., aes(x = id, y = value))+geom_boxplot()

#Rename species column to plants
 names(plantsData)[names(plantsData) == 'Species'] <- 'Plants'

#Add Petal.Area
 plantsData$Petal.Area <- plantsData$Petal.Length * plantsData$Petal.Width
 
#Move Plants column to first position
 plantsData <- plantsData%>%select(Plants, everything())

#Plot correlation matrix with package corrplot
 library("corrplot")
 corrplot(cor(plantsData[2:5]))
 
#Alternative: Calculate correlation matrix
 cormatrix <- signif(cor(plantsData[2:5]),2)
  #Plot heatmap of cormatrix
 col<- colorRampPalette(c("blue", "white", "red"))(20)
 heatmap(cormatrix, col=col, symm=TRUE)
#Plot with ggplot
cor_tri <- as.data.frame(cormatrix) %>%
 mutate(Var1 = factor(row.names(.), levels=row.names(.))) %>%
 gather(key = Var2, value = value, -Var1, na.rm = TRUE, factor_key = TRUE)
ggplot(data = cor_tri, aes(Var2, Var1, fill = value)) +
  geom_tile()

# 1.- Plot Petal.Width vs Petal.Length and different colours according to species (now called plants)
 g <- ggplot(plantsData, aes(x = Petal.Length, y = Petal.Width, color = Plants)) +
   geom_point()
 plot(g)

 # 2.- Plot a histogram of Petal.Width variable
 g <- ggplot(plantsData, aes(x = Petal.Width)) + geom_histogram(binwidth = 0.25, color = "white")
 plot(g)
 