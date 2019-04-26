# Load libraries ----------------------------------------------------------
library(tidyverse)
library(cluster)
library(lubridate)
source('./R/BBC_style.R')

# Load preprocessed data --------------------------------------------------
powerData <- readRDS('./output/powerData.RDS')

# Get energy array --------------------------------------------------------

totalenergy <- powerData %>% 
  group_by(year, month, day, week, weekday, weekend, day, hour) %>% 
  summarise('total_energy_use' = sum(Sub_unnumbered) / 1000) %>% 
  ungroup() %>% 
  spread(hour, total_energy_use) %>% 
  na.omit()

# K-means -----------------------------------------------------------------

set.seed(20)

normalizedenergy <- scale(totalenergy[,7:30], center = T, scale = T)

#optimal clusters: all 2, sub1: 3, sub2: 4 sub3: 2 or 5, unnum: 3

clustered <- kmeans(normalizedenergy, centers = 3)  
totalenergy$cluster <- clustered$cluster

cluster_means <- aggregate(totalenergy[,7:30], list(totalenergy$cluster), mean)

cluster_means <- cluster_means %>% 
  gather(key = "hour", value = "cluster_mean", -Group.1) %>% 
  mutate(hour = as.numeric(hour),
         Group.1 = as.factor(Group.1))

plot <- cluster_means %>% 
  ggplot(aes(x = as.numeric(hour), y = cluster_mean, color = Group.1)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_colour_manual(values = c("#1380A1", "#990000", "#FAAB18","#588300")) +
  bbc_style() +
  #theme(legend.position = "none") +
  labs(title = "Daily energy profiles", 
       subtitle = "Unsubmetered")

finalise_plot(plot, "UCI (energy data)", width_pixels = 1000, height_pixels = 699, save_filepath = './graphs/daily_profiles_sub_un.jpg')

#General info
totalenergy %>% 
  group_by(cluster, weekend) %>% 
  summarize(n = n())

totalenergy %>% 
  group_by(cluster, month) %>% 
  summarize(n = n())

# Plot all daily profiles -------------------------------------------------

powerData %>% 
  group_by(date = date(DateTime), hour = hour(DateTime)) %>% 
  summarize(total_energy_use = sum(Sub_metering_1) / 1000 ) %>% 
  ungroup() %>% 
  ggplot(aes(x = hour, y = total_energy_use, group = as.factor(date))) +
  geom_line(color = "#990000", alpha = 0.03) +
  geom_line(data = cluster_means, aes(x = hour, y = cluster_mean, group = Group.1),
            color = "#1380A1",
            size = 1.5,
            linetype = "longdash") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  bbc_style() +
  #theme(axis.text.y = element_blank()) +
  labs(title = "Daily energy profiles",
       subtitle = "in kWh")

finalise_plot(plot, "UCI (energy data)", width_pixels = 1000, height_pixels = 699, save_filepath = './graphs/daily_profiles.jpg')

# Identifying optimal cluster via Silhouette ---------------------------------------------

# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(normalizedenergy, centers = k)
  ss <- silhouette(km.res$cluster, dist(normalizedenergy))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")


# Identifying optimal clusters via elbow ----------------------------------

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(normalizedenergy, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
