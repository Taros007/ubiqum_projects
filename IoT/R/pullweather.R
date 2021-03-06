library(datetime)
library(lubridate)
library(darksky)

history <- setNames(
  data.frame(stringsAsFactors=FALSE,
    matrix(
      ncol = 25, nrow = 0)), 
  c("Date",
    "hour0",
    "hour1", 
    "hour2", 
    "hour3", 
    "hour4",
    "hour5",
    "hour6",
    "hour7",
    "hour8",
    "hour9",
    "hour10",
    "hour11",
    "hour12",
    "hour13",
    "hour14",
    "hour15",
    "hour16",
    "hour17",
    "hour18",
    "hour19",
    "hour20",
    "hour21",
    "hour22",
    "hour23")
  )

for (i in 1:400){
  day = date("2010-04-28 17:24:00 CET") + days(i - 1)
  then <- get_forecast_for(48.7791,2.2887, day, add_headers=TRUE, units = "auto")
  history <- data.frame(rbind(as.matrix(history), c(as_date(then$daily$time), then$hourly$temperature)))
}

history[,1] <- as_date(history[,1])
hist <- readRDS('./projects/IoT/input/weatherinfo.RDS')
rbind(hist, history)
saveRDS(history,'./weatherinfo.RDS')
