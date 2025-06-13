#INDIA

#Loading the libraries 

library(forecast)
library(vars)
library(ggplot2)
library(tseries)
library(lmtest)
library(ggplot2)
library(vars)
library(astsa)
library(plm)


#India dataset

india_current = scan()
india = ts(india_current, start = 1961)
india_share = scan()
india1 =ts(india_share, start = 1961)

#plotting the data and modeling
autoplot(india, ylab = "GDP(Current US$)")
tsdisplay(india)
india.ARIMA = auto.arima(india, trace = T, stepwise = F, approximation = F)


#Final Model
india.Arima = Arima(india, order=c(5,2,0)) 
summary(india.Arima)

#####################################

#holt modEl
india.hlt=holt(india,h=5)
summary(india.hlt)
autoplot(india.hlt)

#####################################
#Diagonastics
checkresiduals(india.ARIMA)
#H0 : Series of residual has no autocorrelation 
checkresiduals(india.Arima)

######################################
#normality of the residual
shapiro.test(india.Arima$residuals)
#H0; data is normal
jarque.bera.test(india.Arima$residuals)
jarque.bera.test(india.ARIMA$residuals)
shapiro.test(india.hlt$residuals)
#############################################

#hetroscedasticity of sq. residuals
acf(india.ARIMA$residuals^2)
pacf(india.ARIMA$residuals^2)

################################################

#forecasting
arimafore = forecast(india.Arima, h=5)
arimafore
autoplot(arimafore)
summary(india.Arima)

##########################################################################
#intervention analysis
india2 = scan()
india2 = ts(india2, start=1960)
pre_lpg=window(india2, start= 1961, end=1990)
post_lpg=window(india2, start= 1991, end=2001)
overall=window(india2, start=1961, end = 2001)
india.ARIMA = auto.arima(pre_lpg, trace = T, stepwise = F, approximation = F)
tsdisplay(pre_lpg)
ARIMA_pre=Arima(pre_lpg, order=c(2,1,0),include.drift = T)
hlt_pre= holt(pre_lpg,h=10)
autoplot(hlt_pre)
summary(hlt_pre)
forecast_pre= forecast(ARIMA_pre, h=10)
forecast_pre= forecast(india.ARIMA, h=10)
forecast_pre
autoplot(forecast_pre)
autoplot(overall)+
  forecast::autolayer(forecast_pre$mean,series = " Pre Globalization ", size = 1.1)+
  xlab("Year")+ ylab("(% of total merchandise imports)")+
  ggtitle("Low & Middle Income(Middle East & North Africa)")

##################################################################  
#VAR MODEL
india.dataframe = as.data.frame(dataset)
stnry=diffM(india.dataframe)
apply(india.dataframe[1:4],2,adf.test)
grangertest(agriculture~manufacturing, data = dataframe, order =1)
stnry=diffM(india.dataframe[,c(1:4)], d=3)
plot.ts(stnry)
vars.a <-vars::VAR(stnry, lag.max = 10, ic="AIC", type="none")
summary(vars.a)

#TESTING AUTOCORRELATION

serial.test(vars.a)

#GRANGER CAUSALITY

causality(vars.a, cause = c("manufacturing"))

#FoReCaSt

fcast= predict(vars.a, n.ahead = 5)
fcast

#Lagged Correlation
astsa::lag2.plot(dataframe$agriculture, dataframe$gdp,2)

#Panel Data

pdata = pdata.frame(dataset, index=c("country", "year"))
pooled1 = plm(gdp ~ old+young, data=pdata, model="pooling")
summary(pooled1)
femethod1 = plm(gdp ~ old+young, data=pdata, model="within")
summary(femethod1)

#structural break
model1 = Fstats(india~1, from= 0.01)
sctest(model1)
#H0 There is no structural break in a series. here there are structural change
strucchange::breakpoints(india~1)
#2003,2006,2009,2013,2016,2020

#Bai perron test.
bp.india = breakpoints(india~1, h=3)
summary(bp.india)
coefficients(bp.india)
