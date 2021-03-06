# ARIMA FORECASTING IN R

Using the forecast library in R the Arima function is used to forecast Tesla closing stock prices and percentage change, as well as the 10 year treasury rates
```
library('forecast')
autoplot(time_series)+xlab("x label")+ylab("y label")
fit <- auto.arima(time_series)
fit <- Arima(time_series, order=c(p,d,q))

opt=opt_arima(5,2,5,time_series)
```

A function, ```opt_arima()``` is also made which performs a brute force search that covers both a double difference and an auto-regressive term from 0 to 5 and 
 moving average terms from 0 to 5 as well.  

https://en.wikipedia.org/wiki/Autoregressive_integrated_moving_average
