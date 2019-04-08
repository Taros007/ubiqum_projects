## Load libraries ================================
library(tidyverse)
library(magrittr)

## Load preprocessed data ========================
powerData <- readRDS('./output/powerData.RDS')


summary(powerData)

#Explore NAs
sapply(powerData, function(x) sum(is.na(x)))

y <- powerData[!complete.cases(powerData), ]
#Which days have NAs
unique(as.Date(y$DateTime))
#How many NAs per day?
group_by(y, date(DateTime)) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  print(n = nrow(.))

#Fix NAs by taking last weeks' value (comparable day) ==================
powerDatacopy <- powerData #for testing purposes

##FAILED ATTEMPTS - how to make these work?:
#powerDatacopy[11] <- lag(powerDatacopy[1], 10080)
#powerDatacopy[1] <- ifelse(is.na(powerDatacopy[1]), powerDatacopy[11], powerDatacopy[1])

# ifelse(which(is.na(powerDatacopy[1])) >= 10080, which(is.na(powerDatacopy[1]))-10080, which(is.na(powerDatacopy[1])))
# newdata<-apply(powerDatacopy[,c(1:8)], MARGIN=2, function(x) {ifelse(is.na(x), is.na(x-10800),x)})

# Identify index of all NAs, and substract 10080 (minutes in a week) from it if possible
# Subsequently, replace all NAs
y <- ifelse(which(is.na(powerDatacopy[1])) >= 10080, 
            which(is.na(powerDatacopy[1]))-10080, 
            which(is.na(powerDatacopy[1])))
z <- which(is.na(powerDatacopy[1]))
powerDatacopy[z,c(1:8)] <- powerDatacopy[y,c(1:8)]
remove(y,z)

# Omit NAs that couldn't be imputed
powerDatacopy %<>% na.omit()

#Check start and end dates for every year =====================
group_by(powerData, year) %>% 
  summarize(min = min(DateTime), max = max(DateTime))
