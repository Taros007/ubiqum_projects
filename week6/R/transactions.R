## Multiple regression - week 6
## Toine - March 2019

## Load libraries =================================
library(arulesViz)
library(arules)

# # Prepare clusters =================================
# cl <- makeCluster(3)
# registerDoParallel(cl)

## Import dataset =================================
elecTransactions <- read.transactions('./input/ElectronidexTransactions2017.csv', sep = ',', rm.duplicates = F, format = "basket")

## Explore dataset =================================
length(elecTransactions) #provides total amount of observations
size(elecTransactions) # provides vector with number of items per observation
LIST(elecTransactions) #Lists the transactions by conversion (LIST must be capitalized)
itemLabels(elecTransactions) # To see the item labels
summary(elecTransactions)

## Visualize dataset ===============================
#Visualization of most commonly bought items
itemFrequencyPlot(elecTransactions, topN = "15",
                  type="absolute",
                  horiz = T,
                  main="Absolute Item Frequency Plot"
                  )

#Visualization of transactions
arules::image(elecTransactions,
              xlab = "Items (Columns)",
              ylab = "Transactions (Rows)")

#Find out most sold items in transactions with only one item
oneItemSales <- subset(elecTransactions, size(elecTransactions) == 1)
itemFrequencyPlot(oneItemSales, topN = "15",
                  type="absolute",
                  horiz = T,
                  main="Absolute Item Frequency Plot"
                  )

#Find out average number of different items in transaction
mean(size(elecTransactions))

#Visualization of sample of transactions
image(sample(elecTransactions, 200))