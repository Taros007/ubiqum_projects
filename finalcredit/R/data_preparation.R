
# Load libraries ----------------------------------------------------------
library(tidyverse)
library(magrittr)

# Load base data ----------------------------------------------------------

##Only do this once as it takes some time. Use RDS after
#application_train <- read_csv('./input/application_train.csv')
#saveRDS(application_train, './input/application_train.RDS')

application_train <- readRDS('./input/application_train.RDS')

#Convert flags to factor variables
application_train %<>% 
  mutate(
    TARGET_LABEL = as.factor(ifelse(TARGET == 1, "default", "performing")),
    NAME_CONTRACT_TYPE = as.factor(ifelse(NAME_CONTRACT_TYPE == "Cash loans", 1, 0)),
    CODE_GENDER = as.factor(CODE_GENDER),
    FLAG_OWN_CAR = as.factor(FLAG_OWN_CAR),
    FLAG_OWN_REALTY = as.factor(FLAG_OWN_REALTY),
    FLAG_CONT_MOBILE = as.factor(FLAG_CONT_MOBILE),
    FLAG_EMAIL = as.factor(FLAG_EMAIL),
    FLAG_EMP_PHONE = as.factor(FLAG_EMP_PHONE),
    FLAG_WORK_PHONE = as.factor(FLAG_WORK_PHONE),
    FLAG_PHONE = as.factor(FLAG_PHONE)
  )

#Convert all DOCUMENT variables to factors
for (i in seq(2, 21)) {
  a <- paste0("application_train$FLAG_DOCUMENT_" , i, " <- " , 
              "as.factor(application_train$FLAG_DOCUMENT_", i, ")" )
  a <- parse(text = a)
  eval(a)
 }
remove(a, i)


# Load bureau data --------------------------------------------------------

##Only do this once as it takes some time. Use RDS after

#load children data table - bureau
bureau <- read_csv('./input/bureau.csv')
#load grandchildren data table - bureau balance
bureau_balance <- read_csv('./input/bureau_balance.csv')

#saveRDS(application_train, './input/application_train.RDS')

application_train <- readRDS('./input/application_train.RDS')


