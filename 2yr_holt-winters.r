library(hydroGOF)
library(tseries)
library(forecast)
library(ggplot2)
library(e1071)
library(neuralnet)
library(nnfor)
library(tidyverse)
library(readxl)
library(dplyr)
unemployment_data <- read_excel("C:/Users/PRANJAL LAL/Downloads/unemployment_data.xlsx")
view(unemployment_data)
#prepping data
unemployment_data <- as.data.frame(unemployment_data)
my.data <-data.frame(date=unemployment_data['Date'])
for (i in 2:ncol(unemployment_data)) {
  # Check for missing values
  if (any(is.na(unemployment_data[, i]))) {
    # Interpolate using approx
    unemployment_data[, i][is.na(unemployment_data[, i])] <- approx(unemployment_data$Date[!is.na(unemployment_data[, i])], unemployment_data[, i][!is.na(unemployment_data[, i])], na.rm = TRUE)$y
  }
}
unemployment_data$Date <- as.Date(unemployment_data$Date, "%Y-%m-%d")
dftotal <- ts(unemployment_data[,11], frequency=12, start=c(2001,1),end=c(2023,12))
dfts <- ts(unemployment_data[,11], frequency=12, start=c(2001,1),end=c(2021,12))
dfts

dfcd <- ts(unemployment_data[,11], frequency=12, start=c(2022,1),end=c(2023,12))
dfcd
components_dfts <- decompose(dfts)
plot(components_dfts)


HW1 <- HoltWinters(dfts)
# Custom HoltWinters fitting
HW2 <- HoltWinters(dfts, alpha=0.8, beta=0.2, gamma=0.8,seasonal = "additive")
#Visually evaluate the fits
plot(dfts, ylab="Unemployment_rate", xlim=c(2011,2017))
lines(HW1$fitted[,1], lty=2, col="blue")
lines(HW2$fitted[,1], lty=2, col="red")



HW2.pred <- predict(HW2, 24, prediction.interval = TRUE, level=0.95)
#Visually evaluate the prediction
plot(dftotal, ylab="Unemployment_data", xlim=c(2001,2023))
lines(HW2$fitted[,1], lty=2, col="blue")
lines(HW2.pred[,1], col="red")


HW3 <- HoltWinters(dfts, )
HW3.pred <- predict(HW3, 24, prediction.interval = TRUE, level=0.95)
plot(dftotal, ylab="candy production", xlim=c(2001,2023))
lines(HW3$fitted[,1], lty=2, col="blue")
lines(HW3.pred[,1], col="red")
lines(HW3.pred[,2], lty=2, col="orange")
lines(HW3.pred[,3], lty=2, col="orange")

HW1_for <- forecast(HW1, h=24, level=c(80,95))
#visualize our predictions:
plot(HW1_for)
lines(HW1_for$fitted, lty=2, col="purple")


acf(HW1_for$residuals, lag.max=20, na.action=na.pass)
Box.test(HW1_for$residuals, lag=20, type="Ljung-Box")
hist(HW1_for$residuals)

#giving final info
RMSE <- rmse(HW2.pred[,1],dfcd); RMSE
MAE <- mae(HW2.pred[,1],dfcd); MAE
MAPE <- mean(abs((dfcd-HW3.pred[,1])/dfcd)); MAPE

