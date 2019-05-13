
# Load data ---------------------------------------------------------------
source('./R/data_loading.R')
source('./R/data_loading_verification.R')
source('./R/BBC_style.R')

# Load libraries ----------------------------------------------------------
library(tidyverse)

# Remove variables with variance 0 ----------------------------------------
#TODO: is there a logic in how wifi devices are numbered - in case verification data contains removed WAP data
constantVars <- which(apply(wifiData, 2, var)==0)

if(length(constantVars)>0){
  wifiData <- wifiData[,-constantVars]  
  wifiVerification <- wifiVerification[,-constantVars] 
}

# TEMP: throw out data ----------------------------------------------------

## Random subset of data
# take a random sample of size 50 from a dataset mydata
# sample without replacement
set.seed(541)

wifiData <- wifiData[sample(1:nrow(wifiData), 2500,
                          replace=FALSE),]

# Check signal strength per user ------------------------------------------

# wifiData %>%
#   select(c(WAP001:WAP520, USERID)) %>%
#   gather(key = "WAP", value = "Signal", -USERID) %>%
#   ggplot(aes(x = USERID, y = Signal, color = USERID)) +
#   geom_jitter() +
#   geom_violin(scale = 'area', alpha = 0.7, fill = '#808000') +
#   labs(title = 'Explore signal strength per user') +
#   xlab('')


#Check which users collected info per floor
wifiData %>% 
  ggplot(aes(x = LONGITUDE, y = LATITUDE, color = as.factor(USERID)), alpha = 0.5) +
  geom_point() +
  facet_grid(BUILDINGID~FLOOR)

#Check amount of observations per floor per building
wifiData %>% 
  ggplot(aes(x = FLOOR)) +
  geom_bar(position = "dodge") +
  facet_grid(.~BUILDINGID)

#Check amount of observations per floor per building specified for corridor and in room
wifiData %>% 
  ggplot(aes(x = FLOOR, fill = as.factor(RELATIVEPOSITION))) +
  geom_bar(position = "dodge") +
  facet_grid(.~BUILDINGID, labeller = labeller(BUILDINGID = c(`0` = "Building 1", `1` = "Building 2", '2' = "Building 3"))) +
  scale_fill_discrete(name = "Position", labels = c("In room", "On corridor")) +
  labs(title = "Amount of observations", subtitle = "per building, per floor") +
  bbc_style() +
  theme(strip.text.x = element_text(size = 15, face = "italic", hjust = 0.5))

#Check amount of observations per spaceID for a specific building
wifiData %>% 
  filter(BUILDINGID == 2) %>% 
  ggplot(aes(x = SPACEID)) +
  geom_bar(position = "dodge")

#Check signal strengths per WAP per floor per building #not useful
wifiData %>% 
  filter(BUILDINGID == 1) %>% 
  select(contains("WAP"), FLOOR) %>% 
  gather(key = "WAP", value = "RSSI", -FLOOR) %>%
  replace(. == -200, NA) %>% 
  na.omit() %>% 
  group_by(FLOOR, WAP) %>% 
  summarize(RSSI = mean(RSSI)) %>% 
  ggplot(aes(x = WAP, y = RSSI)) +
  geom_point() +
  facet_grid(. ~ FLOOR)

wifiData %>% 
  filter(BUILDINGID == 1) %>% 
  select(contains("WAP"), FLOOR) %>% 
  gather(key = "WAP", value = "RSSI", -FLOOR) %>%
  replace(. == -200, NA) %>% 
  na.omit() %>% 
  ggplot(aes(x = WAP, y = RSSI)) +
  geom_density() +
  facet_grid(. ~ FLOOR)



