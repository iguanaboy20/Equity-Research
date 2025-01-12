## ESTIMATING BETA USING CAPM MODEL
install.packages("quantmod")
library(quantmod)
library(readxl)

##DAILY
#Getting data for NIFTY50 and RIIL.NS
NSE <- getSymbols.yahoo("^NSEI", from = "2020-11-02", to = "2023-10-26", verbose = F, auto.assign = F)
NSE <- na.omit(NSE)

RIIL <- getSymbols.yahoo("RIIL.NS",from = "2020-11-02",to = "2023-10-26",verbose ="False",auto.assign="False")
RIIL <- na.omit(RIIL)

SEQUENT <- getSymbols.yahoo("SEQUENT.NS",from = "2020-11-02",to = "2023-10-26",verbose ="False",auto.assign="False")
SEQUENT <- na.omit(SEQUENT)

T_Bill_D <- read_excel("T_Bills.xlsx")
T_Bill_D <- as.xts(T_Bill_D)
head(T_Bill_D)

#Getting the closing prices
Close <- cbind(NSE$NSEI.Close,RIIL$RIIL.NS.Close,SEQUENT$SEQUENT.NS.Close)
Close <- na.omit(Close)
head(Close,5)

#Getting the returns

Excess_Return_NSE <-((dailyReturn(NSE$NSEI.Close)[-1, ] - T_Bill_D[-1,]))
head(Excess_Return_NSE)

rRIIL <-dailyReturn(RIIL$RIIL.NS.Close)[-1, ]
rSEQUENT <-dailyReturn(SEQUENT$SEQUENT.NS.Close)[-1, ]

returns1 <- cbind(Excess_Return_NSE,rRIIL)
names(returns1) <- c("EXCESS_NSE","RIIL")
head(returns1)

returns2 <- cbind(Excess_Return_NSE,rSEQUENT)
names(returns2) <- c("EXCESS_NSE","SEQUENT")
head(returns2)

#Running the regression

reg_RIIL <- lm(returns1$RIIL~returns1$EXCESS_NSE)
summary(reg_RIIL)

reg_SEQUENT <- lm(returns2$SEQUENT~returns2$EXCESS_NSE)
summary(reg_SEQUENT)

## ESTIMATING AR AND MA COEFFICIENTS USING ARIMA
install.packages("tseries")
install.packages("ggplot2")
install.packages("forecast")
library(tseries)
library(ggplot2)
library(forecast)

#CALCULATING THE RETURNS
returns_riil <- as.xts(tail(data.frame(RIIL$RIIL.NS.Close),-1)/head(data.frame(RIIL$RIIL.NS.Close),-1)-1, frequency = 365)
returns_riil<- na.omit(returns_riil)
returns_sequent <- as.xts(tail(data.frame(SEQUENT$SEQUENT.NS.Close),-1)/head(data.frame(SEQUENT$SEQUENT.NS.Close),-1)-1, frequency = 365)
returns_sequent <- na.omit(returns_sequent)

#PLOTTING THE CLOSING PRICES AND RETURNS
plot(RIIL$RIIL.NS.Close)
plot(returns_riil)

plot(SEQUENT$SEQUENT.NS.Close)
plot(returns_sequent)

adf.test(returns_riil,alternative = "stationary")
adf.test(returns_sequent,alternative = "stationary")

#ACF and PACF plots for getting order of AR and MA terms
plot(acf(returns_riil,lag.max = 10)) #for AR
plot(pacf(returns_riil,lag.max = 10))#for MA

auto.arima(returns_riil)

arima_final1 <- arima(returns_riil,order = c(0,0,0))
arima_final1

predicted <- predict(arima_final1, n.ahead=10)
predicted

tsdiag(arima_final1)

plot(acf(returns_sequent,lag.max = 10)) #for AR
plot(pacf(returns_sequent,lag.max = 10))#for MA

auto.arima(returns_sequent)

arima_final2 <- arima(returns_sequent,order = c(0,0,0))
arima_final2

predicted <- predict(arima_final2, n.ahead=10)
predicted

tsdiag(arima_final2)

##GARCH AND EGARCH MODELS
install.packages("rugarch")
install.packages("rmgarch")
library(rugarch)
library(rmgarch)

RIIL <- getSymbols.yahoo("RIIL.NS",from = "2020-11-02",to = "2023-10-26",verbose ="False",auto.assign="False")
RIIL <- na.omit(RIIL)

SEQUENT <- getSymbols.yahoo("SEQUENT.NS",from = "2020-11-02",to = "2023-10-26",verbose ="False",auto.assign="False")
SEQUENT <- na.omit(SEQUENT)

r_RIIL <-dailyReturn(RIIL$RIIL.NS.Close)[-1, ]
r_SEQUENT <-dailyReturn(SEQUENT$SEQUENT.NS.Close)[-1, ]

#Implementing univariate GARCH
ug_spec = ugarchspec()
ug_spec

#Implementing EGARCH
eg_spec = ugarchspec(variance.model = list(model="eGARCH"))
eg_spec

#Estimating the models
ugfit1 = ugarchfit(spec = ug_spec, data = r_RIIL) 
ugfit1

ugfit2 = ugarchfit(spec = ug_spec, data = r_SEQUENT) 
ugfit2

#Forecasting
ugforecast1 = ugarchforecast(ugfit1, n.ahead=10) 
ugforecast1

ugforecast2 = ugarchforecast(ugfit2, n.ahead=10) 
ugforecast2
