#Calculate prediction metrics
library(caret)
postResample(wifiVerification$PredictionsLNG, wifiVerification$LONGITUDE)
postResample(wifiVerification$PredictionsLAT, wifiVerification$LATITUDE)

# Plot density graph for distance
wifiVerification %>% 
  ggplot(aes(x=Distance)) + 
  geom_density() +
  labs(title = "Position prediction error", 
       subtitle = "for verification data, in m")

#Analyse whether errors are related to floor
wifiVerification %>% 
  ggplot(aes(x = FLOOR, fill = DistanceFlag)) +
  geom_bar(position = "dodge")

#Analyse whether errors are related to building
wifiVerification %>% 
  ggplot(aes(x = BUILDINGID, fill = DistanceFlag)) +
  geom_bar(position = "dodge")

#Analyse whether errors are related to WAP
wifiVerification %>%
  mutate_at(vars(contains("WAP")), function(x) ifelse(x == -200, 0, 1)) %>% 
  group_by(DistanceFlag) %>% 
  summarize_at(vars(contains("WAP")), sum) %>% 
  gather(key = "WAP", value = "Frequency", -DistanceFlag) %>% 
  ggplot(aes(x = WAP, y = Frequency, fill = DistanceFlag)) +
  geom_col()

wifiVerification %>%
  mutate_at(vars(contains("WAP")), function(x) ifelse(x == -200, 0, 1)) %>% 
  group_by(DistanceFlag) %>% 
  summarize_at(vars(contains("WAP")), sum) %>% 
  rownames_to_column %>% #transpose tibble
  gather(var, value, -rowname) %>% #transpose tibble
  spread(rowname, value) %>% #transpose tibble
  tail(-1) %>% 
  mutate("Percentage_wrong" = .$'2' / .$'1') %>% 
  arrange(desc(Percentage_wrong)) %>% 
  View()
