library(forecast)
library(ggplot2)
library(zoo)

# Create sample dataset
dates <- seq.Date(from = as.Date("2014-07-01"), to = as.Date("2020-02-29"), by = "week")
set.seed(123)
count <- rnorm(length(dates), mean = 100, sd = 20)
data <- data.frame(date = dates, count = count)

# Convert to time series object
ts_data <- ts(data$count, frequency = 52, start = c(2014, 27))

# Split data into training and test
train_end <- which(data$date == as.Date("2019-11-30"))
train_data <- window(ts_data, end = c(2019, train_end %% 52))
test_data <- window(ts_data, start = c(2019, (train_end %% 52) + 1))

# Fit SARIMA model
model <- auto.arima(train_data, seasonal = TRUE)

# Print model summary
summary(model)

# Forecast for the winter period (13 weeks)
forecast_result <- forecast(model, h = 13)

# Plot the forecast
plot(forecast_result)
lines(test_data, col = "red")

# Calculate excess cases
observed <- as.vector(test_data)
predicted <- as.vector(forecast_result$mean)
upper_pi <- as.vector(forecast_result$upper[,2])  # 95% prediction interval

# Ensure observed and upper_pi have the same length
excess_cases <- pmax(observed[1:length(upper_pi)] - upper_pi, 0)
total_excess <- sum(excess_cases)

cat("Estimated total excess cases:", round(total_excess, 2), "\n")

# Visualize with LOESS smoothing
all_data <- c(as.vector(train_data), as.vector(test_data))
all_dates <- dates  # Use all dates

loess_fit <- loess(all_data ~ as.numeric(all_dates), span = 0.2)
smoothed <- predict(loess_fit)

ggplot() +
  geom_line(aes(x = all_dates, y = all_data), color = "blue") +
  geom_line(aes(x = all_dates, y = smoothed), color = "red") +
  labs(title = "Observed Data with LOESS Smoothing",
       x = "Date", y = "Count") +
  theme_minimal()
ggsave("ElmoreCopy.png", width=8, height=5)

# Sensitivity analysis (example for different time periods)
winter_data <- subset(data, format(date, "%m") %in% c("12", "01", "02"))
winter_ts <- ts(winter_data$count, frequency = 52)

winter_model <- auto.arima(winter_ts, seasonal = TRUE)
summary(winter_model)