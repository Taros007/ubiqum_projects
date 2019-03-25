## Multiple regression - week 6
## Toine - March 2019

## Load libraries =================================
load(arules)

# # Prepare clusters =================================
# cl <- makeCluster(3)
# registerDoParallel(cl)

## Import dataset =================================
elecTransactions <- read.transactions('./input/ElectronidexTransactions2017.csv', sep = ',', rm.duplicates = F, format = "basket")