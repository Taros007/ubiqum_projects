library(caret)

# Exploration -------------------------------------------------------------

# Target variable imbalanced
table(application_train$TARGET)

#Target variable
application_train %>% 
  ggplot(aes(x = TARGET)) +
  geom_histogram(stat = "count")

#Organization type variable
application_train %>% 
  ggplot(aes(x = FLAG_OWN_CAR)) +
  geom_histogram(stat = "count")

application_train %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

#Assess correlation with selected variables
application_train %>% 
  select(c("TARGET", "EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3", "DAYS_BIRTH")) %>% 
  corrr::correlate()
  
#Finding - chance of default higher for younger ages
application_train %>% 
  transmute(AGE_BINS = cut(YEARS_BIRTH, c(-Inf, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf)),
            TARGET = TARGET) %>% 
  group_by(AGE_BINS) %>% 
  summarize(AVG_TARGET = mean(TARGET)) %>% 
  ggplot(aes(x = AGE_BINS, y = AVG_TARGET)) +
  geom_bar(stat = "identity")
  
