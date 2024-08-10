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
View(ae_data)
date_vector <- as.data.frame(ae_data$Date)
date_vector

#prepping training and comparison data
ae_data_ts <- ts(ae_data[,2], start = 1, end = 252, frequency = 1)
#view(ae_data_ts)
#plot(ae_data_ts)
#ae_data_ts

ae_data_comp <- ts(ae_data[,2], start = 253, end = 276, frequency = 1)
#view(ae_data_comp)
#plot( ae_data_comp )

nrow(ae_data_comp)
#checking for stationarity
acf(ae_data_ts)
pacf(ae_data_ts)
tseries::adf.test(ae_data_ts)
unemploymodel <- forecast::auto.arima(ae_data_ts, ic="aic", trace=TRUE)
unemploymodel



acf(ts(unemploymodel$residuals))
pacf(ts(unemploymodel$residuals))
unemployforecast=forecast::forecast(unemploymodel,level=c(95),h=24)
unemployforecast
Box.test(unemployforecast$resid, lag=15, type= "Ljung-Box")
plot(unemployforecast)

#finding residuals(In-Sample)
residuals <- resid(unemploymodel)
residuals

#Training NN
# Data preprocessing
t3 = residuals[1:249]
t2 = residuals[2:250]
t1 = residuals[3:251]
result = residuals[4:252]
df = data.frame(t1,t2,t3,result)
df

#scale data from 0 to 1
maxs <- apply(df, 2, max)
mins <- apply(df, 2, min)
scaled.data <- as.data.frame(scale(df,center = mins, scale = maxs - mins))
scaled.data

#training
feats <- names(scaled.data[1:3])
formula <- paste(feats, collapse=' + ')
formula <- paste('result ~', formula)
nn <- neuralnet::neuralnet(formula,scaled.data, hidden = c(4), linear.output = TRUE)
#plot(nn)


#continuing forecasting ARIMA 
pred.arima <- predict(unemploymodel, n.ahead=24)
pred.arima <- pred.arima$pred

#calculated residuals
residuals_pre <- ae_data_comp-pred.arima
residuals_pre

#ann prediction
t3 = c(tail(t2,n=1), tail(t1,n=1), tail(result,n=1), residuals_pre[1:21])
t2 = c(tail(t1,n=1), tail(result,n=1), residuals_pre[1:22])
t1 = c(tail(result,n=1), residuals_pre[1:23])
result = residuals_pre
df = data.frame(t1,t2,t3,result)
df

#scale data from 0 to 1
maxs <- apply(df, 2, max)
mins <- apply(df, 2, min)
scaled.data <-as.data.frame(scale(df, center = mins, scale = maxs - mins))

#forecasting
pr.nn <- neuralnet::compute(nn, scaled.data[, 1:3])
pr.nn
#class(pr.nn)
#pr.nn <- pr.nn$net.result
#pr.nn <- ts(pr.nn)

#forecast_values <- forecast(pr.nn, h = 5)
#forecast_values


#scaling back
pred.nn <- pr.nn$net.result*(max(df$result)-min(df$result))+min(df$result)
pred.nn

#combining two results
pred <- pred.arima + pred.nn[,1];
a=data.frame(pred)

#adding date column for convenience
# Convert character dates to actual date objects
dates_as_date <- as.Date(date_vector[253:276,], format = "%d/%m/%Y")  # Adjust format if needed

# Combining with the data
a <- cbind(dates_as_date, a)
colnames(a)[1] <- "Date"  # Set column name for dates
colnames(a)[2] <- "Pred"  # Set column name for predictions
#view(a)

#plot(a)
a <-ts(a[,2], frequency = 1)
#plot(a)
#class(pred)
#class(ae_data_comp)

#giving final info
RMSE <- rmse(pred,ae_data_comp); RMSE
MAE <- mae(pred,ae_data_comp); MAE
MAPE <- mean(abs((ae_data_comp-pred)/ae_data_comp)); MAPE

