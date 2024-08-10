# Load necessary libraries
library(forecast)
library(readxl)

# Load data from Excel file
excel_data <- read_excel("C:/Users/PRANJAL LAL/Downloads/ae data.xlsx")

# Assuming your Excel data has columns like "Timestamp", "Country1", "Country2", ...
# where each column represents the index of a country over time

# Replace "Country1", "Country2", ... with the actual column names representing each country's index

# Select the index data for a specific country (e.g., "Country1")
country_data <- excel_data$`MALSRATE Index  (R4)(MALAYSIA)`  # Replace "Country1" with the actual column name

# Perform data cleaning and imputation for missing values
country_data_cleaned <- na.omit(country_data)  # Remove rows with missing values
time_series <- ts(country_data_cleaned)

# Find the last available year in the dataset
last_year <- time(time_series)[length(time(time_series))]

# Define the training period from 0 years until x-5 years
training_period <- last_year - 5

training_period
# Subset the data for the training period
training_data <- window(time_series, end = training_period)

model <- nnetar(training_data)

# Forecast future values for the specific country from x-5 years until x years
forecast_values <- forecast(model, h = 5)  # Forecast for 5 periods (years)

# Calculate MAE, RMSE, and MAPE
actual_values <- window(time_series, start = last_year - 4)  # Actual values for the forecasted period
mae_value <- mean(abs(forecast_values$mean - actual_values))
rmse_value <- sqrt(mean((forecast_values$mean - actual_values)^2))
mape_value <- mean(abs((forecast_values$mean - actual_values) / actual_values)) * 100

# Set the plot margins and size
par(mar = c(5, 4, 4, 2) + 0.1, mfrow = c(1, 1))

# Plot the original time series data
plot(time_series, type = "l", col = "blue", lwd = 2, xlab = "Time", ylab = "Index", main = "Index Forecast for Country1")
lines(forecast_values$mean, col = "red", lwd = 2)
legend("topright", legend = c("Actual Data", "Forecasted Data"), col = c("blue", "red"), lty = 1, lwd = 2)

# Print MAE, RMSE, and MAPE values
cat("MAE:", round(mae_value, 2), "\n")
cat("RMSE:", round(rmse_value, 2), "\n")
cat("MAPE:", round(mape_value, 2), "%\n")

