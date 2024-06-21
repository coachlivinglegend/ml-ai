##################################################
### PROG8430                                    ##
### Time Series Demonstration                   ## 
##################################################
#                                               ##
##################################################
# Written by Daniel Beckley
# ID: 8846774
#
##################################################
### Assignment 2                                ##
##################################################

# Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

#Set work directory
setwd("C:/Users/Owner/Desktop/AIML/DataAnalysisMath/ass2")

options(scipen=9)

##################################################
### Remove Packages Installed                   ##
##################################################

##################################################
### Install Libraries                           ##
##################################################

#If the library is not already downloaded, download it

if(!require(tseries)){install.packages("tseries")}
library("tseries")

if(!require(TTR)){install.packages("TTR")}
library("TTR")

if(!require(smooth)){install.packages("smooth")}
library("smooth")

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

####################################################
## TIME SERIES  - Quarterly Data                  ##
####################################################

# 1. Data Transformation (2 points):

# 1.1
# Read in the Ayr data and transform it into an appropriate time series datatype.

load("A2_data_temphist.Rdata")
Ayr_ts <- ts(Ayr_temphist, start = c(1968), end = c(2003), frequency = 3)
Ayr_ts


# 2. Descriptive Time Series Data (5 points)

#2.1. Summarize the information (mean, std dev, etc.) (1 point)
stat.desc(Ayr_ts)
summary(Ayr_ts)
sd(Ayr_ts)

# 2.2 Plot the time series data. (1 point)
plot(Ayr_ts, main = "Annual Average Temperature in Ayr", xlab = "Year", ylab = "Temperature")

# 2.3. Decompose the times series data into the constituent components. Comment on each (any trends you observe, etc.) (1 point)
decomposed_Ayr_ts <- decompose(Ayr_ts, type = 'additive')
decomposed_Ayr_ts
plot(decomposed_Ayr_ts)

# 2.4. Deseasonalize the information and plot the result. (1 point)
deseasonalized_Ayr_ts <- Ayr_ts - decomposed_Ayr_ts$seasonal
plot(deseasonalized_Ayr_ts, main = "Deseasonalized Quarterly Average Temperature in Ayr", xlab = "Year", ylab = "Temperature")




# 3. Smooth the Temperature (6 points)

#3.1 Smooth the temperature chart using a moving average. Try 3 different values for the moving average and choose the one you 
#think best shows the trend (if any). (2 points)

Ayr_ts_SMA5 <- SMA(Ayr_ts, n = 5)
plot.ts(Ayr_ts_SMA5)

Ayr_ts_SMA7 <- SMA(Ayr_ts, n = 7)
plot.ts(Ayr_ts_SMA7)

Ayr_ts_SMA10 <- SMA(Ayr_ts, n = 10)
plot.ts(Ayr_ts_SMA10)

# 3.2 Determine if the time series is stationary. (2 points)
adf.test(Ayr_ts)


# 3.3 Create an autocorrelation chart (using acf) and comment on which lags are significant. Do previous values seem to 
#influence current values? (2 points)
acf(Ayr_ts, main = "Autocorrelation of Average Temperature in Ayr")


# 4. Time Series Forecast (6 points)

#4.1 Create a simple moving average forecast of temperature in Ayr for five years beyond the data provided. 
#Graph your results along with a 75% prediction interval. (2 points)
move_avg <- sma(Ayr_ts)
move_avg
move_avg <- forecast(move_avg, h=5,level=0.75)   #h - periods to forecast; level=Prediction Level
move_avg
plot(move_avg)


# 4.2 Create an exponentially smoothing forecast of temperature in Ayr for five years beyond the data provided. 
# Graph your results along with a 75% prediction interval. (2 points)

ES_avg <- es(Ayr_ts)
ES_avg
ES_avg <- forecast(ES_avg, h=5,level=0.75)
ES_avg
plot(ES_avg)

