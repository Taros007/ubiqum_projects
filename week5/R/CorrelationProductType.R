## Multiple regression - week 5
## Toine - March 2019

##Script aimed at easily identifying correlations for specific product types

## Load libraries =================================
library(tidyverse)
library(magrittr)
library(corrplot)
library(cowplot)

## Import dataset =================================
existingProducts <- readr::read_csv2('./input/existingChristianProud.csv')

## Preprocessing: cleaning up ==========================
existingProducts %<>% select(-X1)
names(existingProducts) %<>% make.names(.)

## Preprocessing: alter datatypes & calculate new variables ===============
existingProducts %<>%
  mutate(
    Product_type = as.factor(Product_type),
    Depth = as.numeric(Depth),
    Age = as.factor(Age),
    Professional = as.factor(Professional),
    Review_score = (5 * X5Stars + 4 * X4Stars + 3 * X3Stars + 2 * X2Stars + X1Stars) / rowSums(select(existingProducts, X5Stars:X1Stars))
  )
existingProducts %<>% filter(Volume>0)

## Feature selection =================================
existingProducts <- select(existingProducts,-Product_ID)

## Find correlation for specific product type ==============================

Producttype="Netbook"

chart_title <- paste("Correlation of variables with Volume for producttype: ", Producttype)

existingProducts %>% filter(Product_type == Producttype) %>% 
  keep(is.numeric) %>% 
  correlate() %>% 
  focus(Volume) %>% 
  mutate(rowname = reorder(rowname, Volume)) %>%
  ggplot(aes(rowname, Volume)) +
    geom_col(fill = "#1380A1") + 
    coord_cartesian(xlim = c(-1, 1)) + 
    coord_flip() +
    labs(title = chart_title) +
      ylab("Correlation") +
    theme(axis.title.y=element_blank()) +
    geom_hline(aes(yintercept = 0), linetype="dotted")

