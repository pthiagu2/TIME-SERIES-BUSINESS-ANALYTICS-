library(forecast)
library(fpp)
library(lubridate)
library(plyr)
library(zoo)

Stock_price = read.csv("DJIA_table.csv",header = TRUE,sep = ',')
z <- read.zoo(Stock_price, format = "%m/%d/%Y")
z.m <- as.zooreg(aggregate(z, as.yearmon, mean), freq = 12)
Time_Stock = ts(z.m$Close,start = c(2008,1),freq = 12)
plot(Time_Stock)


#Implementing polynomial model:
#Linear model
t1 = seq(2008,2016,length=length(Time_Stock))
tprime = t1^12
polyStock = lm(Time_Stock ~ t1 + tprime)
Stock_trend = ts(polyStock$fit,start = c(2008,1),frequency = 12)

plot(Time_Stock,lw = 2,col = "blue",xlim=c(2008,2016))
lines(Stock_trend,lw = 2,col = 'red')
abline(v = 2013.25,lty = 3)

#Implementing Stl analysis:
Stl_Stock = stl(Time_Stock,s.window = "periodic")
Stock_trend2 = Stl_Stock$time.series[,2]
plot(Stl_Stock,col = "blue",lw = 2)
plot(forecast(Stl_Stock))
abline(v = 2013.25,lty = 3)

#Predicting graph
arima = auto.arima(Stock_trend)
HaltWintersStock_ng = HoltWinters(Stock_trend,gamma = FALSE)
HaltWintersStock = HoltWinters(Stock_trend)
Neural_Net = nnetar(Stock_trend)
arima_fit = arima(Stock_trend,order = c(1,0,0),list(order=c(2,1,0),period = 12))
linear_fit = tslm(Stock_trend ~ trend + season,lambda = 0)

arima2 = auto.arima(Stock_trend2)
HaltWintersStock_ng2 = HoltWinters(Stock_trend2,gamma = FALSE)
HaltWintersStock2 = HoltWinters(Stock_trend2)
Neural_Net2 = nnetar(Stock_trend2)
arima_fit2 = arima(Stock_trend2,order = c(1,0,0),list(order=c(2,1,0),period = 12))
linear_fit2 = tslm(Stock_trend2 ~ trend + season,lambda = 0)

arima3 = auto.arima(Time_Stock)
HaltWintersStock_ng3 = HoltWinters(Time_Stock,gamma = FALSE)
HaltWintersStock3 = HoltWinters(Time_Stock)
Neural_Net3 = nnetar(Time_Stock)
arima_fit3 = arima(Time_Stock,order = c(1,0,0),list(order=c(2,1,0),period = 12))
linear_fit3 = tslm(Time_Stock ~ trend + season,lambda = 0)

#plot the graph based on polynomial trend:
plot(forecast(arima,h=60)$mean,xlim = c(2000,2030),ylim = c(2000,30000),col = "purple",xlab = "Time",ylab="Stock Price",main = "Predictions of Polynomial Trend")
lines(forecast(Stl_Stock,h = 60)$mean,col = "blue",lw = 2)
lines(Time_Stock,lw = 3)
lines(forecast(arima_fit,h = 60)$mean, col = "yellow")
lines(Stock_trend,lw = 3,col = 'red')
lines(forecast(Neural_Net,h=60)$mean,col = "brown",lty = "longdash")
lines(forecast(linear_fit,h=60)$mean,col = "orange")
lines(predict(HaltWintersStock_ng,n.ahead = 60),lw = 2,col = "green")
lines(predict(HaltWintersStock_ng3,n.ahead = 60),lw = 2,col = "green")
legend("bottomleft",legend=c("Arima","Actual Function","Prediction-Actual Function","Modified Arima","Neural Nets",
                             "Linear Model","HaltWinters"),col=c("purple","black","blue","yellow","brown","orange","green"),lw = 2)


#Plot the graph based on Stl:
plot(forecast(arima2,h=60)$mean,xlim = c(2000,2030),ylim = c(2000,30000),col = "purple",xlab = "Time",ylab="Stock Price",main = "Predictions of Stl Trend")
lines(forecast(Stl_Stock,h = 60)$mean,col = "blue",lw = 2)
lines(Time_Stock,lw = 3)
lines(forecast(arima_fit2,h = 60)$mean, col = "yellow")
lines(Stock_trend2,lw = 3,col = "red")
lines(forecast(Neural_Net2,h=60)$mean,col = "brown",lty = "longdash")
lines(forecast(linear_fit2,h=60)$mean,col = "orange")
lines(predict(HaltWintersStock_ng2,n.ahead = 60),lw = 2,col = "green")
lines(predict(HaltWintersStock_ng3,n.ahead = 60),lw = 2,col = "green")
legend("bottomleft",legend=c("Arima","Actual Function","Prediction-Actual Function","Modified Arima","Neural Nets",
                             "Linear Model","HaltWinters"),col=c("purple","black","blue","yellow","brown","orange","green"),lw = 2)
abline(v = 2013.25,lty = 3)
#Plot the graph based on Actual Function:
plot(forecast(arima3,h=60)$mean,xlim = c(2000,2030),ylim = c(2000,30000),col = "purple",xlab = "Time",ylab="Stock Price",main = "Predictions of Actual")
lines(forecast(Time_Stock,h = 60)$mean,col = "blue",lw = 2)
lines(Time_Stock,lw = 3)
lines(forecast(arima_fit3,h = 60)$mean, col = "yellow")
lines(forecast(Neural_Net3,h=60)$mean,col = "brown",lty = "longdash")
lines(forecast(linear_fit3,h=60)$mean,col = "orange")
lines(predict(HaltWintersStock_ng3,n.ahead = 60),lw = 2,col = "green")
legend("bottomleft",legend=c("Arima","Actual Function","Prediction-Actual Function","Modified Arima","Neural Nets",
                             "Linear Model","HaltWinters"),col=c("purple","black","blue","yellow","brown","orange","green"),lw = 2)
abline(v = 2013.25,lty = 3)
