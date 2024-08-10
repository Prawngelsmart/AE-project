install.packages("neuralnet")
install.packages("nnfor")
install.packages("forecast")
install.packages("nnet")
install.packages("tseries")
install.packages("hydroGOF")
library(nnet)
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
options(warn = -1)
#read file
library(readxl)
ae_data <- read_excel("C:/Users/PRANJAL LAL/Downloads/unemployment_data.xlsx")
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
View(ae_data)
date_vector <- as.data.frame(ae_data$Date)

#prepping training and comparison data
ae_data_ts <- ts(ae_data[,6], start = 1, end = 204, frequency = 1)
#view(ae_data_ts)
plot(ae_data_ts)
ae_data_ts

ae_data_comp <- ts(ae_data[,6], start = 205, end = 276, frequency = 1)
#view(ae_data_comp)
plot( ae_data_comp )

nrow(ae_data_comp)
#checking for stationarity
acf(ae_data_ts)
pacf(ae_data_ts)
tseries::adf.test(ae_data_ts)
unemploymodel <- forecast::auto.arima(ae_data_ts, ic="aic", trace=TRUE)
unemploymodel



acf(ts(unemploymodel$residuals))
pacf(ts(unemploymodel$residuals))
unemployforecast=forecast::forecast(unemploymodel,level=c(95),h=72)
unemployforecast
Box.test(unemployforecast$resid, lag=15, type= "Ljung-Box")
plot(unemployforecast)

#finding residuals(In-Sample)
residuals <- resid(unemploymodel)
residuals

#Training NN
nnetar_model <- forecast::nnetar(residuals, hidden = c(10))

# Forecasting with NNetAR
nnetar_forecast <- forecast(nnetar_model, h = 72)
unemployforecast <- as.data.frame(unemployforecast)
unemployforecast <- ts(unemployforecast)
nnetar_forecast <- as.data.frame(nnetar_forecast)
nnetar_forecast <- ts(nnetar_forecast)
#combining two results
pred <- unemployforecast[,1] + nnetar_forecast;
a=data.frame(pred)
a
pred
dates_as_date <- as.Date(date_vector[205:276,], format = "%d/%m/%Y")  # Adjust format if needed

# Combining with the data
a <- cbind(dates_as_date, a)
colnames(a)[1] <- "Date"  # Set column name for dates
colnames(a)[2] <- "Pred"  # Set column name for predictions
#view(a)

plot(a)
a <-ts(a[,2], frequency = 1)
plot(a)
class(pred)
class(ae_data_comp)

#giving final info
RMSE <- rmse(pred,ae_data_comp); RMSE
MAE <- mae(pred,ae_data_comp); MAE
ae_data_comp <- ae_data_comp[1:length(pred)]
M <- abs(ae_data_comp-pred); 
MAPE <- mean(M/ae_data_comp); MAPE

