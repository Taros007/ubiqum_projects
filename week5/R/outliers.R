library(dplyr)
library(tidyr)
#library(ggplot2)
#library(ruler)

# Detect based on Z-score
isnt_out_z <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - mean(x, na.rm = na.rm)) <= thres * sd(x, na.rm = na.rm)
}

# Detect based on MAD score
isnt_out_mad <- function(x, thres = 3, na.rm = TRUE) {
  x <- scale(x, center = TRUE, scale = TRUE)
  abs(x - median(x, na.rm = na.rm)) <= thres * mad(x, na.rm = na.rm)
}

# Detect based on Tukey
isnt_out_tukey <- function(x, k = 1.5, na.rm = TRUE) {
  quar <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  iqr <- diff(quar)
  (quar[1] - k * iqr <= x) & (x <= quar[2] + k * iqr)
}


# Detect based on Mahalanobis distance
maha_dist <- . %>% select_if(is.numeric) %>%
  mahalanobis(center = colMeans(.), cov = cov(.))
isnt_out_maha <- function(tbl, isnt_out_f, ...) {
  tbl %>% maha_dist() %>% isnt_out_f(...)
}




