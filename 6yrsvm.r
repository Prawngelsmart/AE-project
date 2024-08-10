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
library(readxl)
#Reading the data
unemployment_data <- read_excel("C:/Users/PRANJAL LAL/Downloads/unemployment_data.xlsx")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
#prepping the data 
unemployment_data <- as.data.frame(unemployment_data)
my.data <-data.frame(date=unemployment_data['Date'])
for (i in 2:ncol(unemployment_data)) {
  # Check for missing values
  if (any(is.na(unemployment_data[, i]))) {
    # Interpolate using approx
    unemployment_data[, i][is.na(unemployment_data[, i])] <- approx(unemployment_data$Date[!is.na(unemployment_data[, i])], unemployment_data[, i][!is.na(unemployment_data[, i])], na.rm = TRUE)$y
  }
}
#View(unemployment_data)
#preparing training and testing data
#training_data <- scale(unemployment_data[,2])
#testing_data <- scale(unemployment_data[,2])
training_data <- ts(unemployment_data[,2], start = 1, end = 204, frequency = 1)

testing_data <- ts(unemployment_data[,2], start = 204, end = 276, frequency = 1)




#creating svm model
testing_data
t3=training_data[1:201]
t2=training_data[2:202]
t1= training_data[3:203]
result= training_data[4:204]
df=data.frame(t1,t2,t3,result)
model <- svm(result~t1+t2+t3,  # target variable on the left, features on the right
             data = df,
             kernel = "linear",  # Choose kernel type (e.g., linear, radial)
             cost = 10^6
             )  # Cost parameter (C)
training_data<-as.data.frame(training_data)
testing_data<- as.data.frame(testing_data)
nrow(testing_data)

#finding predictions
predictions <- predict(model, testing_data,n.ahead=72)
nrow(predictions)
plot(predictions)
pred <- ts(predictions)

# Calculate evaluation metrics
testing_data <- ts(testing_data)
predictions <- predictions[1:72]
RMSE <- rmse(predictions,testing_data); RMSE
MAE <- mae(predictions,testing_data); MAE
MAPE <- mean(abs((testing_data-predictions)/testing_data)); MAPE


error=testing_data[,1] - predictions
error

cat("RMSE:", RMSE, "\n")
cat("MAE:", MAE, "\n")
cat("MAPE:", MAPE, "% \n")
print(RMSE)

