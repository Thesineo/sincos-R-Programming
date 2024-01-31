# sincos
Predecitve model for Stock Return forecast (future prediction) for top technical firms in UK listed on London Stock Exchange 
#ARIMA Model 
install.packages("quantmod")
library(quantmod) 
# Input the Stock Variables for required firms predective analysis (SGE.L,SN.L,BA.L,CCC.L,GNS.L,QQ.L,FLTR.L,RWS.L,SPT.L,SXS.L) ( This is the Dataset, that will change accordingly when you input different Stock symbols or ticker symbols  of Respective companies and you can also change the time set )
data<-getSymbols("SN.L", src = "yahoo",from=as.Date("2015-01-01"),to=as.Date("2022-12-31"),auto.assign = FALSE)
#Daily Data 
data
View(data)
#Used For forecating Monthly data
#data<-to.monthly(data)
#Used For forecasting Weekly data
#data<-to.weekly(data)
# Construct only the closing price of stocks for forecasting 
CLOSEPRICE<-data[,4]
CLOSEPRICE<-na.omit(CLOSEPRICE)
View(CLOSEPRICE)

summary(CLOSEPRICE)
sd(CLOSEPRICE)
skewness(CLOSEPRICE)
kurtosis(CLOSEPRICE)
sum(!complete.cases(CLOSEPRICE))
View(CLOSEPRICE)
#Visulise the data
chart_Series(CLOSEPRICE, col = "black")
add_SMA(n = 100, on = 1, col = "red")
add_SMA(n = 20, on = 1, col = "black")
add_RSI(n = 14, maType = "SMA")
add_BBands(n = 20, maType = "SMA", sd = 1, on = -1)
add_MACD(fast = 12, slow = 25, signal = 9, maType = "SMA", histogram = TRUE)
View(CLOSEPRICE)
# Set the training (JAN 2015- DEC 2020) and testing (JAN 2021-DEC 2022) dataset
set.seed(123)
train<-CLOSEPRICE[1:1518]
testa<-CLOSEPRICE[1519:2021]
plot(CLOSEPRICE,main="STOCK CLOSE PRICE,2015-2022",ylab="Price",xlab="Days")
lines(train,col="blue")
lines(testa,col="green")
legend("bottomright",col=c("blue","green"),lty=1,legend=c("Training","Testing"))
# ADF Test for Stationrity
library(tseries)
adf<-adf.test(train)
adf
#ACF and PACF Plots 
acf(train)
pacf(train)
diff_train<-diff(train)
diff_train<-diff_train[-1,]
adf.test(diff_train)
par(mfrow=c(1,1))
acf(diff_train)
pacf(diff_train)
library(caTools)
library(forecast)
#Fitting the ARIMA Model 
fitA<-arima(train,order=c(2,1,2))
fitA
tsdisplay(residuals(fitA),lag.max = 40)
checkresiduals(fitA)
fitb<-arima(train,order  = c(3,1,2))
fitb
BIC(fitb)
tsdisplay(residuals(fitb),lag.max = 40)
checkresiduals(fitb)
fitc<-arima(train,order = c(3,1,2))
fitc
tsdisplay(residuals(fitc),lag.max = 40)
checkresiduals(fitc)
fitd<-arima(train,order=c(2,1,2))
fitd
tsdisplay(residuals(fitd),lag.max = 40)
checkresiduals(fitd)
fite<-arima(train,order=c(2,1,2))
fite
tsdisplay(residuals(fite),lag.max = 40)
checkresiduals(fite)
#Forecasting the future values for stock returns 
# "forecasta, forecastb, forecastc, forecastd,forecaste" is the predicted values of future stock prices of respective companies 
forecasta<-forecast(fitA,h=503)
plot(forecasta)
forecasta
forecastb<-forecast(fitb,h=503)
plot(forecastb)
forecastb
forecastc<-forecast(fitc,h=503)
plot(forecastc)
forecastc
forecastd<-forecast(fitd,h=503)
plot(forecastd)
forecaste<-forecast(fite,h=503)
plot(forecaste)
# Evaluate  the best ARIMA model for forecasting on the basis of forecast accuracy measures 
accuracy(forecasta)
accuracy(forecastb)
accuracy(forecastc)
accuracy(forecasta)
accuracy(forecasta)
#Estimate the performance of the best ARIMA model with the available testing model 
accuracy(forecasta,testa)


#GARCH MODEL
# Libraries
library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(rugarch)
library(tseries)

# # Input the Stock Variables for required firms predective analysis (SGE.L,SN.L,BA.L,CCC.L,GNS.L,QQ.L,FLTR.L,RNS.L,SPT.L,SXS.L) ( This is the Dataset, that will change accordingly when you input different Stock symbols or ticker symbols  of Respective companies and you can also change the time set )
datag<-getSymbols("SN.L" ,src = "yahoo",from=as.Date("2015-01-01"),to=as.Date("2022-12-31"),auto.assign = FALSE)
#For Daily Data 
View(datag)
0
#Used For Monthly data
#datag<-to.monthly(datag)
#Used For Weekly data
#datag<-to.weekly(datag)
# Construct only the closing price of stocks for forecasting 
datag<-datag[,4]
CLOSEPRICE<-datag
CLOSEPRICET<-CLOSEPRICE[1:1518]
CLOSEPRICEF<-CLOSEPRICE[1519:2021]
chartSeries(SXS.L)
View(CLOSEPRICE)

 
# Calculate Daily returns
returnss <- CalculateReturns(CLOSEPRICE)
View(returnss)
returnss <- returnss[-1]
#Contruct the training (JAN 2015- DEC 2020) and testing (JAN 2021- DEC 2022) dataset
return<-returnss[1:1517]
test<-returnss[1518:2020]
class(test)
hist(return)
#Visuualise the data
chart.Histogram(return,
                methods = c('add.density', 'add.normal'),
                colorset = c('blue', 'green', 'red'))
chartSeries(CalculatedReturns)

# Test for different Garch models 
# 1. sGARCH model with contant mean
s <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "sGARCH"),
                distribution.model = 'norm')
m <- ugarchfit(data = return, spec = s)
coef(m)
plot(m)

9
0
# 2. GARCH with sstd
s <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "sGARCH"),
                distribution.model = 'sstd')
m <- ugarchfit(data = return, spec = s)
m
checkresiduals(m)
s <-m@fit$fitted.values
kurtosis(s)
plot(m)
9
0
# GARCH with General Error Distrubution 
s <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "sGARCH"),
                distribution.model = 'ged')
m <- ugarchfit(data = return, spec = s)
m
plot(m)
9
0
# 3. GJR-GARCH
s <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m <- ugarchfit(data = return, spec = s)
coef(m)
checkresiduals(m)
s <-m@fit$fitted.values
kurtosis(s)
plot(m)
9
0

#4. GJR-GARCH in mean
s <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "sGARCH"),
                distribution.model = 'ged')
m <- ugarchfit(data = return, spec = s)
m
plot(m)
9
0
# Simulation with the best GARCH model  evaluted on the bassis of providing the lowest Akakie 
s <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m <- ugarchfit(data = return, spec = s)
m
plot(m)
9
0
s
sfinal <- s
setfixed(sfinal)<-as.list(coef(m))
# Forecast the futre returns of the stocks by the GJR-GARCH model( Values for forecasting changes according to the type of dataset choosed) 
f <- ugarchforecast(m,
                    n.ahead =503)

f
plot(f)
4
0
par(mfrow = c(1,1))
#Showing the volatality of the stock 
plot(sigma(f))
predictedvalues<- ugarchpath(spec = sfinal,
                  m.sim = 1,
                  n.sim = 503,
                  rseed = 123)
plot.zoo(fitted(predictedvalues))
View(fitted(sim))
class(test)
sigma(sim)
plot.zoo(sigma(predictedvalues))
tail(CLOSEPRICET)
# predicted values of stock return (Closing Price) of the firms on each day
#Substitute the last value of Stock close price from the trainiing set in the p variable to predict the future values from the GARCH model
p <- 1510*apply(fitted(predictedvalues),1, 'cumsum') +1510
# p is the predicted values of future stock prices of respective companies 
p
View(p)
plot(p)
table(p)
class(p)

ForecastedValues 
plot(p, type = "l", lwd = 1, main = "Forecasted Values of SN.L by GJR-GARCH(1,1)")
plot(p,closepricef, type = "l",lwd = 1)
length(p)
length(closepricef)
# Evaluate the Performance of GJR-Garch Model ,
View(p)
p<-as.numeric(p)
closepricef<-as.numeric(CLOSEPRICEF$SN.L.Close)
accuracy(p,closepricef)
p-closepricef

rmse<-sqrt(mean((p)^2))
rmse
abs<-abs((closepricef-p)/closepricef)*100
 
mean(abs)
forecastedp<-ts(p,start=1)
closepricets<-ts(CLOSEPRICEF$GNS.L.Close,start=1)
accuracy(forecastedp)
accuracy(forescastdp)

# Evaluate the supreme model among ARIMA and GARCH model for being a better fit for the data depending on required time horizon
#ARIMA model 
accuracy(forecasta,testa)
#GARCH model 
accuracy(forecastedp,closepricets)
   
