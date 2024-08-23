# Possible documentation
# https://towardsdatascience.com/time-series-analysis-with-auto-arima-in-r-2b220b20e8ab

library(MMWRweek)
library(forecast)
library(ggplot2)
library(zoo)
library(xts)
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

# Dog data set
data = read.csv("~/animal_mortality/RawData/dogs.csv", sep=";")
data$WeekDate = as.Date((data$date), format="%d.%m.%Y")
data$Count = rowMeans(data[,c("prevalence_since_2019", "prop_unwell")], na.rm=T)
data = data[MMWRweek(data$WeekDate)$MMWRweek != 53,]
head(data)

ts_data = as.ts(xts(data$Count, order.by = as.Date(data$date)))
ts_data = as.ts(xts(data$Count, order.by = as.Date(data$WeekDate)))
xts_data = xts(data$Count, order.by = as.Date(data$WeekDate))
plot(xts_data)
train_end <- which(data$WeekDate == as.Date("2019-12-01"))
train_data <- xts_data[1:train_end]
test_data <- xts_data[(train_end + 1):length(ts_data)]
length(ts_data)
length(train_data)
length(test_data)


# Fit SARIMA model
tsdisplay(train_data)
model <- auto.arima(train_data, seasonal = TRUE)
# Coefficients:
#          ar1     ar2      ma1    mean
#       0.7072  0.1770  -0.3600  8.9617
# s.e.  0.1596  0.1285   0.1485  0.2312

# sigma^2 = 0.3031:  log likelihood = -124.2
# AIC=258.4   AICc=258.81   BIC=273.55

acf2(train_data)
themodel = arima(train_data, order = c(2,0,1), seasonal = list(order = c(0,1,1), period = 52))
# Forecast for the next year (52 weeks)
prediction_length = 52
test_hat = predict(themodel, n.ahead=prediction_length)

# Print model summary
summary(themodel)
checkresiduals(themodel)
autoplot(themodel)

# Calculate excess cases
observed <- as.vector(test_data)
predicted <- as.vector(forecast_result$mean)
upper_pi <- as.vector(forecast_result$upper[,2])  # 95% prediction interval
xts_predicted = xts(test_hat$pred, order.by=time(test_data)[1:prediction_length])
xts_upper = xts(test_hat$pred + test_hat$se * 2, order.by=time(test_data)[1:prediction_length])

# Ensure observed and upper_pi have the same length
excess_cases <- pmax(observed[1:length(upper_pi)] - upper_pi, 0)
total_excess <- sum(excess_cases)

cat("Estimated total excess cases:", round(total_excess, 2), "\n")

# Visualize with LOESS smoothing
all_data <- c(as.vector(train_data), as.vector(test_data))
all_dates <- c(time(train_data), time(test_data))  # Use all dates
loess_fit <- loess(all_data ~ as.numeric(all_dates), span = 0.2)
smoothed <- predict(loess_fit)

ggplot() +
  geom_line(aes(x = all_dates, y = all_data, color = "observed")) +
  geom_line(aes(x = all_dates, y = smoothed, color = "smooth")) +
  geom_line(aes(x = time(test_data[1:prediction_length]), y = test_hat$pred, color = "estimate")) +
  geom_line(aes(x = time(test_data[1:prediction_length]), y = xts_upper, color = "95th conf.int.")) +
  scale_color_manual(name = "Y series", values = c("smooth" = "red", "observed" = "black", "estimate" = "darkblue", "95th conf.int." = "darkgreen")) + 
  labs(title = "Observed Data with LOESS Smoothing",
       x = "Date", y = "Count") +
  theme_minimal()
ggsave("ElmoreCopy.png", width=8, height=5)

# Sensitivity analysis (example for different time periods)
winter_data <- subset(data, format(date, "%m") %in% c("12", "01", "02"))
winter_ts <- ts(winter_data$count, frequency = 52)

winter_model <- auto.arima(winter_ts, seasonal = TRUE)
summary(winter_model)