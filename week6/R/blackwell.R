blackwell <- read_csv('./input/20190326Blackwellproducts.csv')

blackwell %>% 
  group_by(ProductType) %>% 
  summarize(n=sum(Volume)) %>%
  ggplot(n, mapping=aes(x = reorder(ProductType, n), y = n)) + 
    geom_col(fill = "#1380A1") +
    coord_flip() +
    labs(title = "Blackwell | Sales volume per producttype") +
    xlab("Product Type") +
    ylab("Sales Volume")
