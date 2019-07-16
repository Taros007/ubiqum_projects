
application_train %<>%
  mutate(YEARS_BIRTH = -(DAYS_BIRTH / 365.25))
