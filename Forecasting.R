#################################################
# GDP Univariate Forecasting - Otto Dahlin
#################################################

library(writexl)
library(ggplot2)
library(dplyr)
library(AER)
library(lmtest)
library(tseries)
library(urca)
library(dynlm)
library(sandwich)
library(readxl)
library(xts)
library(vars)
library(zoo)
library(timeSeries)
library(quantmod)
library(mFilter)
library(seasonal)
library(lubridate)
library(dplyr)
library(forecast)

########################################################################################

# Data name: GDP (Expenditure Approach - användningssidan), 
# seasonally adjusted, change in volume in %, by type of use: GDP at market prices (Standard)

#######################################################################################

GDP_data <- read_excel("GDPSeasonVolumeTotalUses.xlsx")
GDP_data

str(GDP_data)
head(GDP_data)

GDP<- GDP_data[,"GDP"]
class(GDP)

#Converting into a time series
GDP.ts <- ts(GDP, frequency = 4, start=c(1981,1), end=c(2020,2))
GDP.ts
is.ts(GDP.ts)
class(GDP.ts)

# plotting the time series and labeling x/y axis.
ts.plot(GDP.ts, col="black", ylab=" Volume Change in GDP (%)", xlab="Time", main="1981:Q1 - 2020:Q2; Change in Volume - Seasonally Adjusted GDP")
grid(col="lightgray", lty = "dotted", lwd= par("lwd"), equilogs=TRUE)
legend("topleft", "Volume Change in GDP", lty=1, bg="lightgray", cex=0.60)

#######################################################################
# QUESTION 1
#######################################################################

#########################################
# Following the Box-Jenkin Approach.
#########################################

# Starting off with potential transformations. Using KPSS-stationarity test.


####################################################################
## 
##      KPSS-testet (Kwiatkowski-Phillips-Schmidt-Shin)
##
####################################################################

# Hypothesis testing. The Null-hypothesis tests for stationarity.

## H0: Stationary
## HA: Non-stationary

# If the test-static is smaller than the chosen critical value
# then this is an indication that the time series is stationary.
# For the time series to be classified as stationary, the null-hypothesis
# should not be rejected.

ur.kpss(GDP.ts, type="tau")@teststat
# Test-static: 0.09304205
ur.kpss(GDP.ts, type ="tau")@cval
# Using a= 0.05 that is 5% significance level and 0.146 as critical value.
# The test-static can also be rejected on any chosen significance level.
# Since 0.09304205 < 0.146 we cannot reject the null-hypothesis of stationary.


# That is, the time series is stationary and we do not need to transform the series
# to make it stationary.

# To conclude, the integration order "I" in "ARIMA" will be set to 0 i.e. I(0)
# due to no first-differencing.
##################################################################################


########################################################################
# Model specification - Determining the AR(p) and MA(q) components.
#######################################################################

# Using the "Acf" and "Pacf" functions instead of the "acf" and "pacf" functions in R.
# Using the "Acf" function of from the package "forecast" it will not show that lag 0.

Acf(GDP.ts, main= "ACF of Full GDP Series")
# There are two significant spikes implying a q=2


Pacf(GDP.ts, main ="PACF of Full GDP Series")
# Even in the PACF one can determine only one (1) Auto-regressive component
# p=1. As can be viewed from the plot, only one (1) significant spike outside the confidence band.'

# One could also extend the nr of lags with "lag.max" command. I've done it
# but have come to the same conclusion when altering the nr of lags with "lag.max" in the ACF/PACF command function.

# Mode: ARMA(1,2) with p=1 and q=2 according to above specification and reasoning.

# Merge into 1 pic.
par(mfrow=c(1,2))
Acf(GDP.ts, main= "ACF of Full GDP Series")
Pacf(GDP.ts, main ="PACF of Full GDP Series")


# Based on the determining the ARMA(p,q) specification of ACF/PACF it is the following:
# Modelspecification: ARMA(1,2) with I=0

# as extra, using the auto.arima function, the best chosen model seems to be ARMA(0,3) or ARIMA(0,0,3) using AIC as informationcriterion


# *** EXTRA
auto.arima(GDP.ts, trace=TRUE, ic="aic") 
# As extra, auto.arima suggests on the 1981Q1 - 2018Q2 series that
# Best model is ARMA(0,3).
#######################################################################


#######################################################################

#######################################################################
# QUESTION 2
#######################################################################


# Changing the "END" Period for all above ARMA specifications to
# 2011:Q4 as the ending date above. Model specifications
# will be estimated from 1981:Q1 to 2011:Q4 as specified in the assignment.

# Converting to time series objects.
ARMA11.ts <- ts(GDP, frequency = 4, start=c(1981,1), end=c(2011,4))
ARMA21.ts <- ts(GDP, frequency = 4, start=c(1981,1), end=c(2011,4))
ARMA12.ts <- ts(GDP, frequency = 4, start=c(1981,1), end=c(2011,4))
ARMA22.ts <- ts(GDP, frequency = 4, start=c(1981,1), end=c(2011,4))


# Setting the arima specifications for alla above.

# ARMA(1,1)
GDP.ARMA11 <- arima(ARMA11.ts, order = c(1,0,1))
GDP.ARMA11

# ARMA(2,1)
GDP.ARMA21 <- arima(ARMA21.ts, order = c(2,0,1))
GDP.ARMA21

#ARMA(1,2)
GDP.ARMA12 <- arima(ARMA12.ts, order = c(1,0,2))
GDP.ARMA12

#ARMA(2,2)
GDP.ARMA22 <- arima(ARMA22.ts, order = c(2,0,2))
GDP.ARMA22

####################################################
# Forecsating; One- and two-step ahead predictions.
# with above model specifications.

# We will be forecasting 2012;Q1 and 2012:Q2 using
# the above ARMA model specifications.


####################################################
# ARMA(1,1)
####################################################
# ARMA (1,1) One-step ahead i.e. 2012:Q1
forecast(GDP.ARMA11, h = 1)
# ARMA(1,1) Two-steap ahead i.e. 2012:Q2
forecast(GDP.ARMA11, h = 2)

# Plotting forecasts
plot(forecast(GDP.ARMA11, h=1))
plot(forecast(GDP.ARMA11, h=2))

####################################################
# ARMA(2,1)
####################################################


# ARMA (2,1) One-step ahead i.e. 2012:Q1
forecast(GDP.ARMA21, h = 1)
# ARMA(2,1) Two-steap ahead i.e. 2012:Q2
forecast(GDP.ARMA21, h = 2)

plot(forecast(GDP.ARMA21, h=1))
plot(forecast(GDP.ARMA21, h=2))

####################################################
# ARMA(1,2)
####################################################


# ARMA (2,1) One-step ahead i.e. 2012:Q1
forecast(GDP.ARMA12, h = 1)
# ARMA(2,1) Two-steap ahead i.e. 2012:Q2
forecast(GDP.ARMA12, h = 2)

plot(forecast(GDP.ARMA12, h=1))
plot(forecast(GDP.ARMA12, h=2))

####################################################
# ARMA(2,2)
####################################################
# ARMA (2,1) One-step ahead i.e. 2012:Q1
forecast(GDP.ARMA22, h = 1)
# ARMA(2,1) Two-steap ahead i.e. 2012:Q2
forecast(GDP.ARMA22, h = 2)

plot(forecast(GDP.ARMA22, h=1))
plot(forecast(GDP.ARMA22, h=2))



#################################################################
#  Own preference of chosen ARMA(p,q) of series
# between time period 1981:Q1 - 2011:Q4
#################################################################

#Converting into a shorter time series, with period 1981:Q1 - 2011:Q4
# as the ARMA (p,q) will be determined on the following window 1981:Q1 - 2011:Q4
GDP.ts.short <- ts(GDP, frequency = 4, start=c(1981,1), end=c(2015,2))
GDP.ts.short
is.ts(GDP.ts.short)
class(GDP.ts.short)

ur.kpss(GDP.ts.short, type="tau")@teststat
# Test-static: 0.06876472
ur.kpss(GDP.ts.short, type ="tau")@cval
# The short series is stationary.

# plotting the time series and labeling x/y axis.
ts.plot(GDP.ts.short, col="black", ylab=" Volume Change in GDP (%)", xlab="Time", main="1981Q1-2011Q4; Change in Volume - Seasonally Adjusted GDP")
grid(col="lightgray", lty = "dotted", lwd= par("lwd"), equilogs=TRUE)
legend("topleft", "Volume Change in GDP", lty=1, bg="lightgray", cex=0.60)


###############################################################
################################################################

########### Question 2A

################################################################
################################################################

# RECURSIVE FORECASTS: ARMA(1,1) + evaluation period 

# **********   2-step ahead forecasts with H=2

# ARMA(1,1)
GDP.ARMA11
# ska göra 14 st prognoser från 2012Q1 till 2015Q2.
pseudo.prognos.ARMA11 <- ts(matrix(NA, nrow = 14, ncol = 1),start = c(2012, 1), end=c(2015,2), frequency = 4)
pseudo.prognos.ARMA11

for (i in 1:14){
  EndDate <- 2012.00 + (i - 1) / 4
  Data <- window(GDP.ts, end = EndDate)
  Result <- arima(Data, order = c(1, 0, 1))
  pseudo.prognos.ARMA11[i] <- forecast(Result, h = 2)$mean
}

# Nedan är pseuto-out-of sample forecasts från 2012Q1 till 2015Q2.
pseudo.prognos.ARMA11 
ts.plot(pseudo.prognos.ARMA11)

utfall.från2012Q1 <- window(GDP.ts, start = c(2012,1), end =c(2015,2))
utfall.från2012Q1
window(cbind(utfall.från2012Q1,pseudo.prognos.ARMA11), end=c(2015,2))


#plottar
ts.plot(utfall.från2012Q1, main="Utfallsserie från 2012Q1")
ts.plot(pseudo.prognos.ARMA11, main="Pseudo-out-of sample forecasts från 2012Q1")
## Studera de kommande prognoserna med utfallsdata


## Samkör Utfall från 2012Q1 och Prognostiserade pseudo ARMA11
ts.plot(utfall.från2012Q1, pseudo.prognos.ARMA11, 
        main ="2012Q1-2015Q2 - Utfallsdata och ARMA(1,1)",
        gpars=list(xlab = "Time",col =c("black","red")))
legend("topleft", legend = c("Utfall från 2012Q1", "Pseudo ARMA11"), col = c("black", "red"), lty = 1, cex=0.6)




## Samkör hela GDP  serien från 1981Q1 till 2015Q2 samt Pseudo ARMA(1,1)
# "GDP.ts.short" - GDP serie från 1981Q1 till 2015Q2 hela dataserien inkl evaluation period.
# Pseudo ARMA(1,1)

## Utfall.hela = hela serien från 2005 till 2018 i fallet för SARIMA(1,1,0)(0,1,1)[12]
GDP.ts.short <- ts(GDP, frequency = 4, start=c(1981,1), end=c(2015,2))
ts.plot(GDP.ts.short)

window(cbind(GDP.ts.short,pseudo.prognos.ARMA11))
# Nu ska vi jämföra hur nära prognoserna sammanfaller med det sanna utfallsdatan.

ts.plot(GDP.ts.short , pseudo.prognos.ARMA11, 
        main ="GDP Actual Data och ARMA(1,1), Two-step Ahead", 
        gpars=list(xlab = "Time", ylab="Volume Change in GDP (%)",col =c("black","red")))
legend("topleft", legend = c("GDP", "ARMA(1,1)"), 
       col = c("black", "red"), lwd =1, lty = 1,  cex=0.6)





################################################################
################################################################


# RECURSIVE FORECASTS: ARMA(2,1) + evaluation period

# ARMA(2,1)
GDP.ARMA21
# ska göra 14 st prognoser från 2012Q1 till 2015Q2.
pseudo.prognos.ARMA21 <- ts(matrix(NA, nrow = 14, ncol = 1),start = c(2012, 1), end=c(2015,2), frequency = 4)
pseudo.prognos.ARMA21

for (i in 1:14){
  EndDate <- 2012.00 + (i - 1) / 4
  Data <- window(GDP.ts, end = EndDate)
  Result <- arima(Data, order = c(2, 0, 1))
  pseudo.prognos.ARMA21[i] <- forecast(Result, h = 2)$mean
}

# Nedan är pseuto-out-of sample forecasts från 2012Q1 till 2015Q2.
pseudo.prognos.ARMA21 
ts.plot(pseudo.prognos.ARMA21)

utfall.från2012Q1 <- window(GDP.ts, start = c(2012,1), end =c(2015,2))
utfall.från2012Q1
window(cbind(utfall.från2012Q1,pseudo.prognos.ARMA21), end=c(2015,2))


#plottar
ts.plot(utfall.från2012Q1, main="Utfallsserie från 2012Q1")
ts.plot(pseudo.prognos.ARMA21, main="Pseudo-out-of sample forecasts från 2012Q1")
## Studera de kommande prognoserna med utfallsdata


## Samkör Utfall från 2012Q1 och Prognostiserade pseudo ARMA11
ts.plot(utfall.från2012Q1, pseudo.prognos.ARMA21, 
        main ="2012Q1-2015Q2 - Utfallsdata och ARMA(2,1)",
        gpars=list(xlab = "Time",col =c("black","purple")))
legend("topleft", legend = c("Utfall från 2012Q1", "Pseudo ARMA11"), col = c("black", "purple"), lty = 1, cex=0.6)




## Samkör hela GDP  serien från 1981Q1 till 2015Q2 samt Pseudo ARMA(1,1)
# "GDP.ts.short" - GDP serie från 1981Q1 till 2015Q2 hela dataserien inkl evaluation period.
# Pseudo ARMA(1,1)

## Utfall.hela = hela serien från 2005 till 2018 i fallet för SARIMA(1,1,0)(0,1,1)[12]
GDP.ts.short <- ts(GDP, frequency = 4, start=c(1981,1), end=c(2015,2))
ts.plot(GDP.ts.short)

window(cbind(GDP.ts.short,pseudo.prognos.ARMA21))
# Nu ska vi jämföra hur nära prognoserna sammanfaller med det sanna utfallsdatan.

ts.plot(GDP.ts.short , pseudo.prognos.ARMA21, 
        main ="GDP Actual Data och ARMA(2,1), Two-step Ahead", 
        gpars=list(xlab = "Time", ylab="Volume Change in GDP (%)",col =c("black","purple")))
legend("topleft", legend = c("GDP", "ARMA(2,1)"), 
       col = c("black", "purple"), lwd =1, lty = 1,  cex=0.6)

#################################################################################
#################################################################################


# RECURSIVE FORECASTS: ARMA(1,2) + evaluation period
# TWO-step Ahead

# ARMA(1,2)
GDP.ARMA12
# ska göra 14 st prognoser från 2012Q1 till 2015Q2.
pseudo.prognos.ARMA12 <- ts(matrix(NA, nrow = 14, ncol = 1),start = c(2012, 1), end=c(2015,2), frequency = 4)
pseudo.prognos.ARMA12

for (i in 1:14){
  EndDate <- 2012.00 + (i - 1) / 4
  Data <- window(GDP.ts, end = EndDate)
  Result <- arima(Data, order = c(1, 0, 2))
  pseudo.prognos.ARMA12[i] <- forecast(Result, h = 2)$mean
}

# Nedan är pseuto-out-of sample forecasts från 2012Q1 till 2015Q2.
pseudo.prognos.ARMA12 
ts.plot(pseudo.prognos.ARMA12)

utfall.från2012Q1 <- window(GDP.ts, start = c(2012,1), end =c(2015,2))
utfall.från2012Q1
window(cbind(utfall.från2012Q1,pseudo.prognos.ARMA12), end=c(2015,2))


#plottar
ts.plot(utfall.från2012Q1, main="Utfallsserie från 2012Q1")
ts.plot(pseudo.prognos.ARMA12, main="Pseudo-out-of sample forecasts från 2012Q1")
## Studera de kommande prognoserna med utfallsdata


## Samkör Utfall från 2012Q1 och Prognostiserade pseudo ARMA11
ts.plot(utfall.från2012Q1, pseudo.prognos.ARMA12, 
        main ="2012Q1-2015Q2 - Utfallsdata och ARMA(1,2)",
        gpars=list(xlab = "Time",col =c("black","green")))
legend("topleft", legend = c("Utfall från 2012Q1", "Pseudo ARMA12"), col = c("black", "green"), lty = 1, cex=0.6)




## Samkör hela GDP  serien från 1981Q1 till 2015Q2 samt Pseudo ARMA(1,1)
# "GDP.ts.short" - GDP serie från 1981Q1 till 2015Q2 hela dataserien inkl evaluation period.
# Pseudo ARMA(1,2)

## Utfall.hela = hela serien från 2005 till 2018 i fallet för SARIMA(1,1,0)(0,1,1)[12]
GDP.ts.short <- ts(GDP, frequency = 4, start=c(1981,1), end=c(2015,2))
ts.plot(GDP.ts.short)

window(cbind(GDP.ts.short,pseudo.prognos.ARMA12))
# Nu ska vi jämföra hur nära prognoserna sammanfaller med det sanna utfallsdatan.

ts.plot(GDP.ts.short , pseudo.prognos.ARMA12, 
        main ="GDP Actual Data och ARMA(1,2), Two-step Ahead", 
        gpars=list(xlab = "Time", ylab="Volume Change in GDP (%)",col =c("black","orange")))
legend("topleft", legend = c("GDP", "ARMA(1,2)"), 
       col = c("black", "orange"), lwd =1, lty = 1,  cex=0.6)

#################################################################################################
#################################################################################################



# RECURSIVE FORECASTS: ARMA(2,2) + evaluation period
# Two-step ahead

# ARMA(2,2)
GDP.ARMA22
# ska göra 14 st prognoser från 2012Q1 till 2015Q2.
pseudo.prognos.ARMA22 <- ts(matrix(NA, nrow = 14, ncol = 1),start = c(2012, 1), end=c(2015,2), frequency = 4)
pseudo.prognos.ARMA22

for (i in 1:14){
  EndDate <- 2012.00 + (i - 1) / 4
  Data <- window(GDP.ts, end = EndDate)
  Result <- arima(Data, order = c(2, 0, 2))
  pseudo.prognos.ARMA22[i] <- forecast(Result, h = 2)$mean
}

# Nedan är pseuto-out-of sample forecasts från 2012Q1 till 2015Q2.
pseudo.prognos.ARMA22 
ts.plot(pseudo.prognos.ARMA22)

utfall.från2012Q1 <- window(GDP.ts, start = c(2012,1), end =c(2015,2))
utfall.från2012Q1
window(cbind(utfall.från2012Q1,pseudo.prognos.ARMA22), end=c(2015,2))


#plottar
ts.plot(utfall.från2012Q1, main="Utfallsserie från 2012Q1")
ts.plot(pseudo.prognos.ARMA22, main="Pseudo-out-of sample forecasts från 2012Q1")
## Studera de kommande prognoserna med utfallsdata


## Samkör Utfall från 2012Q1 och Prognostiserade pseudo ARMA11
ts.plot(utfall.från2012Q1, pseudo.prognos.ARMA22, 
        main ="2012Q1-2015Q2 - Utfallsdata och ARMA(2,2)",
        gpars=list(xlab = "Time",col =c("black","blue")))
legend("topleft", legend = c("Utfall från 2012Q1", "Pseudo ARMA22"), col = c("black", "blue"), lty = 1, cex=0.6)




## Samkör hela GDP  serien från 1981Q1 till 2015Q2 samt Pseudo ARMA(1,1)
# "GDP.ts.short" - GDP serie från 1981Q1 till 2015Q2 hela dataserien inkl evaluation period.
# Pseudo ARMA(2,2)

## Utfall.hela = hela serien från 2005 till 2018 i fallet 
GDP.ts.short <- ts(GDP, frequency = 4, start=c(1981,1), end=c(2015,2))
ts.plot(GDP.ts.short)

window(cbind(GDP.ts.short,pseudo.prognos.ARMA22))
# Nu ska vi jämföra hur nära prognoserna sammanfaller med det sanna utfallsdatan.

ts.plot(GDP.ts.short , pseudo.prognos.ARMA22, 
        main ="GDP Actual Data och ARMA(2,2), Two-step Ahead", 
        gpars=list(xlab = "Time", ylab="Volume Change in GDP (%)",col =c("black","blue")))
legend("topleft", legend = c("GDP", "ARMA(2,2)"), 
       col = c("black", "blue"), lwd =1, lty = 1,  cex=0.6)

####################################################################################
####################################################################################


###################################################################
# PROGNOSUTVÄRDERING
#
# där Pseudu.prognosARMA11/12/21/22 är prognoserna
# Medan "utfall.från2012Q1" är den faktiska dataserien dvs utfallsdatan.
################################################################### 

# FORECAST ERRORS TWO-STEP AHEAD, h=2

#### ARMA(1,1)

## utfall.från2012Q1 - pseudo.prognos.ARMA11
FE.Prognos.ARMA11<- utfall.från2012Q1 - pseudo.prognos.ARMA11
FE.Prognos.ARMA11

#### ARMA(2,1)

## utfall.från2012Q1 - pseudo.prognos.ARMA11
FE.Prognos.ARMA21<- utfall.från2012Q1 - pseudo.prognos.ARMA21
FE.Prognos.ARMA21

#### ARMA(2,1)

## utfall.från2012Q1 - pseudo.prognos.ARMA11
FE.Prognos.ARMA12<- utfall.från2012Q1 - pseudo.prognos.ARMA12
FE.Prognos.ARMA12

#### ARMA(2,2)

## utfall.från2012Q1 - pseudo.prognos.ARMA11
FE.Prognos.ARMA22<- utfall.från2012Q1 - pseudo.prognos.ARMA22
FE.Prognos.ARMA22

###########################################################################


# Formula below for RMSE
sqrt(mean((data$actual - data$predicted)^2))

# TWO-STEP AHEAD....

###########################
# RMSE,  MAE and BIAS
##########################
# FE.Prognos.ARMA11 = actual - predicted (FORECAST ERROR)
# RMSE calculated below manually for ARMA(1,1)-model

# ARMA(1,1)
sqrt(mean((FE.Prognos.ARMA11)^2))
# RMSE 0.37564
mean(abs(FE.Prognos.ARMA11))
# MAE: 0.3236743

# BIAS ARMA(1,1) when h=2
sum(FE.Prognos.ARMA11)/14
# BIAS: 0.05925002

# ARMA(2,1)
sqrt(mean((FE.Prognos.ARMA21)^2))
# RMSE 0.3933664
mean(abs(FE.Prognos.ARMA21))
# MAE: 0.3295442

# BIAS ARMA(2,1) when h=2
sum(FE.Prognos.ARMA21)/14
# BIAS: 0.05760899


# ARMA(1,2)
sqrt(mean((FE.Prognos.ARMA12)^2))
# RMSE 0.4675408
mean(abs(FE.Prognos.ARMA12))
# MAE: 0.3610935

# BIAS ARMA(1,2) when h=2
sum(FE.Prognos.ARMA12)/14
# BIAS: 0.03011329



# ARMA(2,2)
sqrt(mean((FE.Prognos.ARMA22)^2))
# RMSE 0.4739332
mean(abs(FE.Prognos.ARMA22))
#MAE : 0.3723855

# BIAS ARMA(2,2) when h=2
sum(FE.Prognos.ARMA22)/14
# BIAS: 0.03011329


####################################################################################

# ONE- Step Ahead FOrecasts

# *********** Forecast horizon when h=1 


# Question 1A

# RECURSIVE FORECASTS: ARMA(1,1) + evaluation period 


# ARMA(1,1)
GDP.ARMA11
# ska göra 14 st prognoser från 2012Q1 till 2015Q2.
pseudo.prognos.ARMA11h1 <- ts(matrix(NA, nrow = 14, ncol = 1),start = c(2012, 1), end=c(2015,2), frequency = 4)
pseudo.prognos.ARMA11h1

for (i in 1:14){
  EndDate <- 2012.00 + (i - 1) / 4
  Data <- window(GDP.ts, end = EndDate)
  Result <- arima(Data, order = c(1, 0, 1))
  pseudo.prognos.ARMA11h1[i] <- forecast(Result, h = 1)$mean
}

# Nedan är pseuto-out-of sample forecasts från 2012Q1 till 2015Q2.
pseudo.prognos.ARMA11h1
ts.plot(pseudo.prognos.ARMA11h1)

utfall.från2012Q1h1 <- window(GDP.ts, start = c(2012,1), end =c(2015,2))
utfall.från2012Q1h1
window(cbind(utfall.från2012Q1h1,pseudo.prognos.ARMA11h1), end=c(2015,2))


#plottar
ts.plot(utfall.från2012Q1h1, main="Utfallsserie från 2012Q1")
ts.plot(pseudo.prognos.ARMA11h1, main="Pseudo-out-of sample forecasts från 2012Q1")
## Studera de kommande prognoserna med utfallsdata


## Samkör Utfall från 2012Q1 och Prognostiserade pseudo ARMA11
ts.plot(utfall.från2012Q1h1, pseudo.prognos.ARMA11h1, 
        main ="2012Q1-2015Q2 - Utfallsdata och ARMA(1,1), One-step ahead",
        gpars=list(xlab = "Time",col =c("black","red")))
legend("topleft", legend = c("Utfall från 2012Q1", "Pseudo ARMA11"), col = c("black", "red"), lty = 1, cex=0.6)



## Samkör hela GDP  serien från 1981Q1 till 2015Q2 samt Pseudo ARMA(1,1)
# "GDP.ts.short" - GDP serie från 1981Q1 till 2015Q2 hela dataserien inkl evaluation period.
# Pseudo ARMA(1,1)

## Utfall.hela = hela serien från 2005 till 2018 i fallet för SARIMA(1,1,0)(0,1,1)[12]
GDP.ts.shorth1 <- ts(GDP, frequency = 4, start=c(1981,1), end=c(2015,2))
ts.plot(GDP.ts.shorth1)

window(cbind(GDP.ts.shorth1,pseudo.prognos.ARMA11h1))
# Nu ska vi jämföra hur nära prognoserna sammanfaller med det sanna utfallsdatan.

ts.plot(GDP.ts.shorth1 , pseudo.prognos.ARMA11h1, 
        main ="GDP Actual Data och ARMA(1,1), One-step ahead", 
        gpars=list(xlab = "Time", ylab="Volume Change in GDP (%)",col =c("black","red")))
legend("topleft", legend = c("GDP", "ARMA(1,1)"), 
       col = c("black", "red"), lwd =1, lty = 1,  cex=0.6)





################################################################
################################################################


# RECURSIVE FORECASTS: ARMA(2,1) + evaluation period
# H=1

# ARMA(2,1)
GDP.ARMA21
# ska göra 14 st prognoser från 2012Q1 till 2015Q2.
pseudo.prognos.ARMA21h1 <- ts(matrix(NA, nrow = 14, ncol = 1),start = c(2012, 1), end=c(2015,2), frequency = 4)
pseudo.prognos.ARMA21h1

for (i in 1:14){
  EndDate <- 2012.00 + (i - 1) / 4
  Data <- window(GDP.ts, end = EndDate)
  Result <- arima(Data, order = c(2, 0, 1))
  pseudo.prognos.ARMA21h1[i] <- forecast(Result, h = 1)$mean
}

# Nedan är pseuto-out-of sample forecasts från 2012Q1 till 2015Q2.
pseudo.prognos.ARMA21h1 
ts.plot(pseudo.prognos.ARMA21h1)

utfall.från2012Q1h1 <- window(GDP.ts, start = c(2012,1), end =c(2015,2))
utfall.från2012Q1h1
window(cbind(utfall.från2012Q1h1,pseudo.prognos.ARMA21h1), end=c(2015,2))


#plottar
ts.plot(utfall.från2012Q1h1, main="Utfallsserie från 2012Q1")
ts.plot(pseudo.prognos.ARMA21h1, main="Pseudo-out-of sample forecasts från 2012Q1")
## Studera de kommande prognoserna med utfallsdata


## Samkör Utfall från 2012Q1 och Prognostiserade pseudo ARMA11
ts.plot(utfall.från2012Q1h1, pseudo.prognos.ARMA21h1, 
        main ="2012Q1-2015Q2 - Utfallsdata och ARMA(1,1), One-step ahead",
        gpars=list(xlab = "Time",col =c("black","purple")))
legend("topleft", legend = c("Utfall från 2012Q1", "Pseudo ARMA11"), col = c("black", "purple"), lty = 1, cex=0.6)




## Samkör hela GDP  serien från 1981Q1 till 2015Q2 samt Pseudo ARMA(1,1)
# "GDP.ts.short" - GDP serie från 1981Q1 till 2015Q2 hela dataserien inkl evaluation period.
# Pseudo ARMA(1,1)

## Utfall.hela = hela serien från 2005 till 2018 i fallet för SARIMA(1,1,0)(0,1,1)[12]
GDP.ts.shorth1 <- ts(GDP, frequency = 4, start=c(1981,1), end=c(2015,2))
ts.plot(GDP.ts.shorth1)

window(cbind(GDP.ts.shorth1,pseudo.prognos.ARMA21h1))
# Nu ska vi jämföra hur nära prognoserna sammanfaller med det sanna utfallsdatan.

ts.plot(GDP.ts.shorth1 , pseudo.prognos.ARMA21h1, 
        main ="GDP Actual Data och ARMA(2,1), One-step Ahead", 
        gpars=list(xlab = "Time", ylab="Volume Change in GDP (%)",col =c("black","purple")))
legend("topleft", legend = c("GDP", "ARMA(2,1)"), 
       col = c("black", "purple"), lwd =1, lty = 1,  cex=0.6)

#################################################################################
#################################################################################

# RECURSIVE FORECASTS: ARMA(1,2) + evaluation period

# ARMA(1,2)
GDP.ARMA12
# ska göra 14 st prognoser från 2012Q1 till 2015Q2.
pseudo.prognos.ARMA12h1 <- ts(matrix(NA, nrow = 14, ncol = 1),start = c(2012, 1), end=c(2015,2), frequency = 4)
pseudo.prognos.ARMA12h1

for (i in 1:14){
  EndDate <- 2012.00 + (i - 1) / 4
  Data <- window(GDP.ts, end = EndDate)
  Result <- arima(Data, order = c(1, 0, 2))
  pseudo.prognos.ARMA12h1[i] <- forecast(Result, h = 1)$mean
}

# Nedan är pseuto-out-of sample forecasts från 2012Q1 till 2015Q2.
pseudo.prognos.ARMA12h1 
ts.plot(pseudo.prognos.ARMA12h1)

utfall.från2012Q1h1 <- window(GDP.ts, start = c(2012,1), end =c(2015,2))
utfall.från2012Q1h1
window(cbind(utfall.från2012Q1h1,pseudo.prognos.ARMA12h1), end=c(2015,2))


#plottar
ts.plot(utfall.från2012Q1h1, main="Utfallsserie från 2012Q1")
ts.plot(pseudo.prognos.ARMA12h1, main="Pseudo-out-of sample forecasts från 2012Q1")
## Studera de kommande prognoserna med utfallsdata


## Samkör Utfall från 2012Q1 och Prognostiserade pseudo ARMA11
ts.plot(utfall.från2012Q1h1, pseudo.prognos.ARMA12h1, 
        main ="2012Q1-2015Q2 - Utfallsdata och ARMA(1,2), One-step ahead",
        gpars=list(xlab = "Time",col =c("black","green")))
legend("topleft", legend = c("Utfall från 2012Q1", "Pseudo ARMA12"), col = c("black", "green"), lty = 1, cex=0.6)




## Samkör hela GDP  serien från 1981Q1 till 2015Q2 samt Pseudo ARMA(1,1)
# "GDP.ts.short" - GDP serie från 1981Q1 till 2015Q2 hela dataserien inkl evaluation period.
# Pseudo ARMA(1,2)

## Utfall.hela = hela serien från 2005 till 2018 i fallet för SARIMA(1,1,0)(0,1,1)[12]
GDP.ts.shorth1 <- ts(GDP, frequency = 4, start=c(1981,1), end=c(2015,2))
ts.plot(GDP.ts.shorth1)

window(cbind(GDP.ts.shorth1,pseudo.prognos.ARMA12h1))
# Nu ska vi jämföra hur nära prognoserna sammanfaller med det sanna utfallsdatan.

ts.plot(GDP.ts.shorth1 , pseudo.prognos.ARMA12h1, 
        main ="GDP Actual Data och ARMA(1,2), One-step ahead", 
        gpars=list(xlab = "Time", ylab="Volume Change in GDP (%)",col =c("black","green")))
legend("topleft", legend = c("GDP", "ARMA(1,2)"), 
       col = c("black", "green"), lwd =1, lty = 1,  cex=0.6)

#################################################################################################
#################################################################################################


# RECURSIVE FORECASTS: ARMA(2,2) + evaluation period

# ARMA(2,2)
GDP.ARMA22
# ska göra 14 st prognoser från 2012Q1 till 2015Q2.
pseudo.prognos.ARMA22h1 <- ts(matrix(NA, nrow = 14, ncol = 1),start = c(2012, 1), end=c(2015,2), frequency = 4)
pseudo.prognos.ARMA22h1

for (i in 1:14){
  EndDate <- 2012.00 + (i - 1) / 4
  Data <- window(GDP.ts, end = EndDate)
  Result <- arima(Data, order = c(2, 0, 2))
  pseudo.prognos.ARMA22h1[i] <- forecast(Result, h = 1)$mean
}

# Nedan är pseuto-out-of sample forecasts från 2012Q1 till 2015Q2.
pseudo.prognos.ARMA22h1 
ts.plot(pseudo.prognos.ARMA22h1)

utfall.från2012Q1h1 <- window(GDP.ts, start = c(2012,1), end =c(2015,2))
utfall.från2012Q1h1
window(cbind(utfall.från2012Q1h1,pseudo.prognos.ARMA22h1), end=c(2015,2))


#plottar
ts.plot(utfall.från2012Q1h1, main="Utfallsserie från 2012Q1")
ts.plot(pseudo.prognos.ARMA22h1, main="Pseudo-out-of sample forecasts från 2012Q1")
## Studera de kommande prognoserna med utfallsdata


## Samkör Utfall från 2012Q1 och Prognostiserade pseudo ARMA11
ts.plot(utfall.från2012Q1h1, pseudo.prognos.ARMA22h1, 
        main ="2012Q1-2015Q2 - Utfallsdata och ARMA(2,2), One-step Ahead",
        gpars=list(xlab = "Time",col =c("black","blue")))
legend("topleft", legend = c("Utfall från 2012Q1", "Pseudo ARMA22"), col = c("black", "blue"), lty = 1, cex=0.6)




## Samkör hela GDP  serien från 1981Q1 till 2015Q2 samt Pseudo ARMA(1,1)
# "GDP.ts.short" - GDP serie från 1981Q1 till 2015Q2 hela dataserien inkl evaluation period.
# Pseudo ARMA(2,2)

## Utfall.hela = hela serien från 2005 till 2018 i fallet 
GDP.ts.shorth1 <- ts(GDP, frequency = 4, start=c(1981,1), end=c(2015,2))
ts.plot(GDP.ts.shorth1)

window(cbind(GDP.ts.shorth1,pseudo.prognos.ARMA22h1))
# Nu ska vi jämföra hur nära prognoserna sammanfaller med det sanna utfallsdatan.

ts.plot(GDP.ts.shorth1 , pseudo.prognos.ARMA22h1, 
        main ="GDP Actual Data och ARMA(2,2), One-step Ahead", 
        gpars=list(xlab = "Time", ylab="Volume Change in GDP (%)",col =c("black","blue")))
legend("topleft", legend = c("GDP", "ARMA(2,2)"), 
       col = c("black", "blue"), lwd =1, lty = 1,  cex=0.6)

####################################################################################
####################################################################################


###################################################################
# PROGNOSUTVÄRDERING/ Foreast Evaluation (For One-step Ahead Forecasts)
#
# där Pseudu.prognosARMA11/12/21/22 är prognoserna
# Medan "utfall.från2012Q1" är den faktiska dataserien dvs utfallsdatan.
################################################################### 

#### ARMA(1,1)

## utfall.från2012Q1 - pseudo.prognos.ARMA11
FE.Prognos.ARMA11h1<- utfall.från2012Q1h1 - pseudo.prognos.ARMA11h1
FE.Prognos.ARMA11h1

#### ARMA(2,1)

## utfall.från2012Q1 - pseudo.prognos.ARMA11
FE.Prognos.ARMA21h1<- utfall.från2012Q1h1 - pseudo.prognos.ARMA21h1
FE.Prognos.ARMA21h1

#### ARMA(2,1)

## utfall.från2012Q1 - pseudo.prognos.ARMA11
FE.Prognos.ARMA12h1<- utfall.från2012Q1h1 - pseudo.prognos.ARMA12h1
FE.Prognos.ARMA12h1

#### ARMA(2,2)

## utfall.från2012Q1 - pseudo.prognos.ARMA11
FE.Prognos.ARMA22h1<- utfall.från2012Q1h1 - pseudo.prognos.ARMA22h1
FE.Prognos.ARMA22h1

###########################################################################


# Formula below for RMSE
sqrt(mean((data$actual - data$predicted)^2))

######################################################
# RMSE & MAE when H=1 One-step ahead forecasts conducted 
# And Bias computed as well.
#####################################################
# FE.Prognos.ARMA11 = actual - predicted (FORECAST ERROR)
# RMSE calculated below manually for ARMA(1,1)-model

# ARMA(1,1)
sqrt(mean((FE.Prognos.ARMA11h1)^2))
# RMSE 0.37564
mean(abs(FE.Prognos.ARMA11h1))
# MAE: 0.3236743


# ARMA(2,1)
sqrt(mean((FE.Prognos.ARMA21h1)^2))
# RMSE 0.3933664
mean(abs(FE.Prognos.ARMA21h1))
# MAE: 0.3295442

# ARMA(1,2) denna modell skiljer sig från när h=2
sqrt(mean((FE.Prognos.ARMA12h1)^2))
# RMSE 0.4675408
mean(abs(FE.Prognos.ARMA12h1))
# MAE: 0.4675408


# ARMA(2,2)
sqrt(mean((FE.Prognos.ARMA22h1)^2))
# RMSE 0.4739332
mean(abs(FE.Prognos.ARMA22))
#MAE : 0.3723855


# BIAS of the 4 models when h=1

# BIAS ARMA(1,1) when h=1
sum(FE.Prognos.ARMA11h1)/14
# OR Both work simply is the mean
mean(FE.Prognos.ARMA11h1)
# BIAS: 0.05925002

# BIAS ARMA(2,1) when h=1
sum(FE.Prognos.ARMA21h1)/14
# BIAS: 0.05760899

# BIAS ARMA(1,2) when h=1
sum(FE.Prognos.ARMA12h1)/14
mean(FE.Prognos.ARMA12h1)
# BIAS: 0.0424759

# BIAS ARMA(2,2) when h=1
sum(FE.Prognos.ARMA22h1)/14
# BIAS: 0.03011329

######################################################
######################################################


# QUESTION 2C) - Autoarima
# We let auto.arima select the model specication that most appropriately
# describes the DGP on the shorter time series with data that spans from 1981Q1 to 2011Q4
# as this is the time period on which the model specification is to be used for recasting
# the evaluation window/period.

# From before we have the following:
ARMA.all<- ts(GDP, frequency = 4, start=c(1981,1), end=c(2011,4))

auto.arimaselect1 <- auto.arima(ARMA.all, trace=TRUE, ic="aic") 
auto


# Chooses ARIMA(3,0,0)(1,0,0)[4] with non-zero mean on the
# time period 1981Q1- 2011Q4 to be the best description of the DGP structure
# And this above model specification is supposed to be the most appropriate one for
# forecasting the evaluation period 2012Q1 to 2015Q2. When ic = AIC

# Though when ic = bic then:
auto.arimaselect <- auto.arima(ARMA.all, trace=TRUE, ic="bic") 

auto.arimaselect <- auto.arima(ARMA.all)
auto.arimaselect
# Best model: ARIMA(1,0,1)(1,0,0)[4] with non-zero mean 

# Some forecast evaluation statistics for the auto.arima selected model.
summary(auto.arimaselect)
# RMSE: 0.8103851
# MAE: 0.574622
# BIAS:0.4675408
mean(auto.arimaselect)

######################################################
######################################################
# END
######################################################
######################################################



