install.packages("neuralnet")
install.packages("nnfor")
install.packages("forecast")
install.packages("tseries")
install.packages("hydroGOF")
library(hydroGOF)
library(tseries)
library(forecast)
library(ggplot2)
install.packages("e1071", dependencies = TRUE)
library(e1071)
library(neuralnet)
library(nnfor)
install.packages("tidyverse", dependencies = TRUE)
library(tidyverse)

#read file
library(readxl)
ae_data <- read_excel("C:/Users/PRANJAL LAL/Downloads/unemployment_data.xlsx")
#View(ae_data)
ae_data <- as.data.frame(ae_data)

#preparation of dataset
#missing_values <- sum(is.na(ae_data[, 1]))
#if (missing_values > 0) {
#  print("Missing values found in the first column!")
#} else {
#  print("No missing values found in the first column!")
#}

for (i in 2:ncol(ae_data)) {
  # Check for missing values
  if (any(is.na(ae_data[, i]))) {
    # Interpolate using approx
    ae_data[, i][is.na(ae_data[, i])] <- approx(ae_data$Date[!is.na(ae_data[, i])], ae_data[, i][!is.na(ae_data[, i])], na.rm = TRUE)$y
  }
}
#View(ae_data)
date_vector <- as.data.frame(ae_data$Date)
date_vector

#prepping training and comparison data
ae_data_ts <- ts(ae_data[,3], start = 1, end = 204, frequency = 1)
#view(ae_data_ts)
#plot(ae_data_ts)
#ae_data_ts

ae_data_comp <- ts(ae_data[,3], start = 205, end = 276, frequency = 1)
#view(ae_data_comp)
#plot( ae_data_comp )

#checking for stationarity
acf(ae_data_ts)
pacf(ae_data_ts)
tseries::adf.test(ae_data_ts)
unemploymodel <- forecast::auto.arima(ae_data_ts, ic="aic", trace=TRUE)
#unemploymodel



acf(ts(unemploymodel$residuals))
pacf(ts(unemploymodel$residuals))
unemployforecast=forecast::forecast(unemploymodel,level=c(95),h=72)
#unemployforecast
Box.test(unemployforecast$resid, lag=15, type= "Ljung-Box")
plot(unemployforecast)


unemployforecast <- as.data.frame(unemployforecast)
unemployforecast <- ts(unemployforecast)
unemployforecast

#FINAL INFO
RMSE <- rmse(unemployforecast[,1],ae_data_comp); RMSE
MAE <- mae(unemployforecast[,1],ae_data_comp); MAE
ae_data_comp <- ae_data_comp[1:length(unemployforecast[,1])]
MAPE <- mean(abs(unemployforecast[,1]-ae_data_comp)/ae_data_comp);MAPE
