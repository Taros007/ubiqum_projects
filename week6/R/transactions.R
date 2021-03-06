## Market basket analysis - week 6
## Toine - March 2019

## Load libraries =================================
library(arulesViz)
library(arules)

## Prepare clusters =================================
#cl <- makeCluster(3)
#registerDoParallel(cl)

## Create category variable which shows whether transaction is corporate or not (with dataframe) #######
df <- readr::read_csv('./input/ElectronidexTransactions2017.csv', col_names = F, trim_ws = T)

#Initialize category variable
corporation <- logical(dim(df)[1])

computers <- c(
  "LG Touchscreen Laptop",
  "Acer Aspire",
  "HP Laptop",
  "ASUS Chromebook",
  "Apple MacBook Pro",
  "Apple MacBook Air",
  "Dell Laptop",
  "Eluktronics Pro Gaming Laptop",
  "Alienware Laptop",
  "HP Notebook Touchscreen Laptop PC",
  "Lenovo Desktop Computer",
  "iMac",
  "HP Desktop",
  "ASUS Desktop",
  "Dell Desktop",
  "Intel Desktop",
  "Acer Desktop",
  "CYBERPOWER Gamer Desktop"
)

#Run in a loop over all transactions, and flag corporation TRUE if a transaction has >1 computer
for (i in 1:length(corporation)) {
  compcount <- 0
  for (j in 1:length(df[i,])) {
    if (df[i,j] %in% computers) {
      compcount <- compcount + 1
      if (compcount > 1) {
        corporation[i] <- TRUE; break
      }
    }
  }
}

#Code to screen for MS office in transaction  
# if (elecTransactions[i]@data[97]) {
#   category[i] <- "PrivateIndividual"    
# }

## Import dataset =================================
source('./R/replacer.R')
elecTransactions <- read.transactions('./input/replaced.csv', sep = ',', rm.duplicates = F, format = "basket")
#elecTransactions <- read.transactions('./input/ElectronidexTransactions2017.csv', sep = ',', rm.duplicates = F, format = "basket")

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

## Building of the model ====================================
rules <- apriori(elecTransactions, 
                 parameter = list(support = 0.05, confidence = 0.5, minlen = 2)
                )
inspect(sort(rules, by = "lift"))
plot(rules, method = "graph", shading = NA)


## Digging into Laptops =====================================
rules1 <- apriori(elecTransactions, 
                 parameter = list(support = 0.01, confidence = 0.2, minlen = 2),
                 appearance = list(default="rhs", lhs="Laptop")
                  )
inspect(sort(rules1, by = "lift"))
plot(rules1, method = "graph", shading = NA)

## Digging into Desktops =====================================
rules1 <- apriori(elecTransactions, 
                  parameter = list(support = 0.01, confidence = 0.2, minlen = 2),
                  appearance = list(default="rhs", lhs="Desktop")
)
inspect(sort(rules1, by = "lift"))
plot(rules1, method = "graph", shading = NA)

## Digging into Monitors =====================================
rules1 <- apriori(elecTransactions, 
                  parameter = list(support = 0.01, confidence = 0.2, minlen = 2),
                  appearance = list(default="rhs", lhs="Laptop")
)
inspect(sort(rules1, by = "lift"))
plot(rules1, method = "graph", shading = NA)


## Split transaction data into corporation and private individuals
elecTrans_corp<-elecTransactions[corporation]
elecTrans_private<-elecTransactions[!corporation]

rules <- apriori(elecTrans_corp, 
                 parameter = list(support = 0.05, confidence = 0.5, minlen = 2)
                )
inspect(sort(rules, by = "lift"))
plot(rules, method = "graph", shading = NA)

rules <- apriori(elecTrans_private, 
                 parameter = list(support = 0.01, confidence = 0.5, minlen = 2)
)
inspect(sort(rules, by = "count"))
plot(rules, method = "graph", shading = NA)
