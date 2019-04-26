#Modeling dat shit man -- heaters has a normal distribution (better for predicting)
#Import libraries and dataset
require(pacman)
p_load(gridExtra, 
       tidyverse, 
       dplyr, 
       ggplot2, 
       tibbletime, 
       GGally, 
       fpp2, 
       vars, 
       lubridate, 
       imputeTS, 
       vars, 
       ggfortify, 
       tidyr, 
       DataCombine, 
       tseries, 
       seasonal, xts,
       shiny, shinydashboard, dygraphs, shinythemes, plotly)
load("dygraph.consumption")
load("dygraph.loss")



##forecast of daily data 
last.year <- hourly
ts <- ts(daily["Global_active_power"], start = c(2006, 350,25) , end = c(2010, 330,75), frequency = 365,25)
ts2 <- na.interpolation(ts)
autoplot(ts, facet = T) #seems to be multiplicative


#### finding the best K (fourier terms)
bestfit <- list(aicc=Inf) 
for(K in seq(25)) {   
  fit <- auto.arima(train, xreg=fourier(train, K=K),     
                    seasonal=FALSE)   
  if(fit[["aicc"]] < bestfit[["aicc"]]) {     
    bestfit <- fit     
    bestK <- K   
  } 
} 
fc <- forecast(bestfit,   
               xreg=fourier(train, K=bestK, h=100)) 
plot <- autoplot(week.ts, colour = "gray") +
  autolayer(fc) + 
  theme_classic() + 
  ggtitle("Weekly losses forecast - ARIMA(5, 0, 1)")

accuracy(fc, test)[,2]
checkresiduals(fc)
a


#wekly time series
week.ts <- ts(weekly["Global_active_power"], start = c(2006, 50,35), end = c(2010, 47,25), frequency = 52,25)
autoplot(week.ts)


#monthly time series
month.ts <- ts(monthly[c("Kitchen", "Laundry_room", "Heaters", "S4")], start = c(2006, 11,52), end = c(2010, 10,82), frequency = 12)
month.ts <- na.interpolation(month.ts, option = "stine")

##create training and test set
train <- head(week.ts, 152)
test <- tail(week.ts, 54)
autoplot(train, series="Training") +
  autolayer(test, series="Test")

#run a fourier series for weekly time series
fit.fourier.week <- tslm(train ~ trend + fourier(train, K = 11))
summary(fit.fourier.week) # (-0.13 trend over time by week) + season 7 (after almost two months = - 16)


autoplot(train) + 
  autolayer(test) +
  autolayer(forecast(fitted(fit.fourier.week, week.ts), h = 53), col = "darkblue", lty = 2) + 
  ggtitle("Fourier, K = 11") + xlab("Week") + ylab("GAP") + xlim(2008, 2011) + theme_classic()
accuracy(forecast(fitted(fit.fourier.week)), test)[,2]
checkresiduals(forecast(fitted(fit.fourier.week)))
f <- (forecast(fitted(fit.fourier, test), h = 54))
f
checkresiduals(f)


#fourier for monthly time series
fit.fourier.month <- tslm(train ~ trend + fourier(train, K = 5))
summary(fit.fourier) # (-0.13 trend over time by week) + season 7 (after almost two months = - 16)

autoplot(forecast(fitted(fit.fourier.month, test, PI = T), h = 24), col = "red") + xlab(element_blank()) +
  ylab("GAP") +ggtitle("Monthly Fourier, K = 5") + theme_classic() 


#create a multiple time series object of daily weekly and monthly
complex <- msts(train, seasonal.periods = c(6, 30, 52))
fit.fourier.week.m <- tslm(train ~ trend + fourier(train, K =  c(14, 25)))

autoplot(complex) + 
  autolayer(forecast(fitted(fit.fourier.week.m, complex), h = 80), col = "darkblue", lty = 2) + ggtitle("Fourier, K = 11") + xlab("Week") + ylab("GAP") + 
  theme_classic()
accuracy(forecast(fitted(fit.fourier.week.m)), test)

checkresiduals(forecast(fitted(fit.fourier.week.m)))


#accuracy of different models
c(Mean = accuracy(meanf(train), test)[2,2],
  Naive = accuracy(naive(train), test)[2,2],
  Drift = accuracy(rwf(train, drift = T), test)[2,2],
  Seasonal.Naive = accuracy(snaive(train), test)[2,2])




#check mse for each point predicted in the future
month.ts %>% tsCV(forecastfunction = fourier, h = 53) -> e
mse <- colMeans(e^2, na.rm = T)

data.frame(h = 1:53, MSE = mse) %>% 
  ggplot(aes(x = h, y = MSE, color = h)) + geom_point() #MSE falls after 5 forecasts lags



#arima
arima.train <- auto.arima(train, seasonal = F, xreg = fourier(train, K = 5), 0, 1)
arima.train <- forecast(arima.train, xreg = fourier(train, K = 5))
arima.train2 <- auto.arima(train, lambda = 0, 0, 1)


autoplot(train) 
autoplot(arima.train, h = 40, series = "Arima")

autolayer(test, series = "test") +
  xlab("Weeks") +
  ylab("GAP") +
  ggtitle("Weekly - Arima through Fourier (3,0,3)(0,1,0)") + theme_classic()
accuracy(arima.train, test)

checkresiduals(forecast(arima.train))


###HOlt winters bitch
hw <- HoltWinters(week.ts, seasonal = "additive")
hw2 <- HoltWinters(week.ts, seasonal = "additive")

hw.pred2 <- predict(hw2, 100, prediction.interval = T)

hw.pred <- forecast(hw, 100,  prediction.interval = T)

autoplot(hw.pred, series = "Additive", PI = T)+ autolayer(week.ts)
ggtitle("Weekly - Exponential smoothing with trend and additive seasonality(0.1, -0.024, 0.52)") + 
  theme_classic() + xlim(2008, 2012) 


all <- merge(actual = as.zoo(week.ts), predicted = as.zoo(hw.pred))
all.xts <- xts(all, date_decimal(index(all)))


all2 <- merge(actual = as.zoo(week.ts), predicted = as.zoo(hw.pred2))
all.xts2 <- xts(all2, date_decimal(index(all2)))

d <- dygraph(all.xts2) %>%
  dySeries("actual", label = "Loss", color = "darkred") %>%
  dySeries(c("Lo 80", "Lo 95", "Point Forecast", "Hi 80"), label = "Predictions", color = "gray") %>%
  dyRangeSelector(height = 20)


hw.pred2 <- predict(tbatsFit, 100, prediction.interval = T)
hw.pred2 <- as.data.frame(hw.pred2) %>%   ts(start = c(2010, 47,26), end = c(2012, 42,18), frequency = 52,25) 



boxplot(week.ts ~ cycle(week.ts), col = "darkgreen") + title("Outliers and variance, Global active power - Weekly level") 

# display the 2 highest "power" frequencies (find the different seasonalities)
p <- periodogram(lm.ts)

dd = data.frame(freq=p$freq, spec=p$spec)
order = dd[order(-dd$spec),]
top2 = head(order, 2)

top2
time = 1/top2$f
time

###tbats
Acf(month.ts)
tbatsFit <- tbats(week.ts, use.parallel=TRUE, num.cores = 2) # fit tbats model
autoplot(forecast(tbatsFit, h = 100)) + autolayer(test) +
  ggtitle("Weekly, Energy Losses - Tbats") + theme_classic()# plot





#trying to predict from last month data (of the last year)
##############

subset <- as_tbl_time(daily, `date(DateTime)` )
last.month <- subset %>% filter_time('2010-10-20' ~ '2010-11-26')



lm.ts <- ts(last.month["Global_reactive_power"], start = c(2010, 291,87), end  = c(2010, 328,87), frequency = 7)
autoplot(lm.ts)
fit.loss <- lm.ts %>% stlf()
fit
#create subsets
train <- head(lm.ts, 25)
test <- tail(lm.ts, 12)
autoplot(forecast(fit, h = 7), col = "gray") + 
  theme_classic() + 
  xlab("Two weeks interval") + ggtitle("Daily consumption forecast - one week ahead")
accuracy(forecast(fit), test)[,2]
forecast(fit)
fit
autoplot(forecast(llek, h = 12)) + autolayer(test)

#creating the dygraph for the short-term forecasts
short.term <- predict(fit, 8, prediction.interval = T)
short.term <- as.data.frame(short.term) %>%   
  ts(start = c(2010, 328,87), end = c(2010, 336,87), frequency = 7)

short <- merge(actual = as.zoo(lm.ts), predicted = as.zoo(short.term))
short.xts <- xts(short, date_decimal(index(short)))

dygraph(short.xts) %>%
  dySeries("Global_active_power", label = "Loss") %>%
  dySeries(c("Lo 80", "Point Forecast", "Hi 80"), label = "Predictions", color = "gray") %>%
  dyRangeSelector(height = 20)

