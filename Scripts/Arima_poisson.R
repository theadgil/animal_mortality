######################################
# SARIMA R-SCRIPT
######################################

# Load necessary libraries
library(MMWRweek)
library(forecast)
library(ggplot2)
library(patchwork)
library(ggpubr)
library(zoo)
library(xts)
library(stringr) # for line breaks in legends
library(boot)

######################################
# Define functions
######################################

# define 'find_best_model()' function
find_best_model <- function(train_data, period = 52, seasonal = TRUE) {
  
  ### Fit Initial ARIMA Model with auto.arima()
  initial_model <- auto.arima(train_data, seasonal = seasonal, max.p = 2, max.q = 2, max.d = 1)
  
  # Extract non-seasonal parameters
  p <- initial_model$arma[1]  # AR order
  d <- initial_model$arma[6]  # Differencing order
  q <- initial_model$arma[2]  # MA order
  
  # Print non-seasonal parameters
  print(paste("Non-seasonal parameters: p =", p, ", d =", d, ", q =", q))
  
  ### Grid Search for Seasonal Parameters
  # Function to evaluate different seasonal ARIMA models
  evaluate_seasonal_arima_model <- function(train_data, P, D, Q, period) {
    model <- tryCatch({
      arima(train_data, order = c(p, d, q), seasonal = list(order = c(P, D, Q), period = period))
    }, error = function(e) {
      return(NULL)
    })
    
    if (!is.null(model)) {
      return(AIC(model))  # Or use BIC(model)
    } else {
      return(Inf)  # Return a high value for invalid models
    }
  }
  
  # Define a grid of seasonal parameters
  P_values <- 0:1
  D_values <- 0:1
  Q_values <- 0:1
  
  # Perform the grid search for seasonal parameters
  best_aic <- Inf
  best_model <- NULL
  best_params <- NULL
  
  for (P in P_values) {
    for (D in D_values) {
      for (Q in Q_values) {
        current_aic <- evaluate_seasonal_arima_model(train_data, P, D, Q, period)
        if (current_aic < best_aic) {
          best_aic <- current_aic
          best_params <- list(P = P, D = D, Q = Q)
          best_model <- arima(train_data, order = c(p, d, q), seasonal = list(order = c(P, D, Q), period = period))
        }
      }
    }
  }
  # Print seasonal parameters
  print(paste("Seasonal parameters: P =", best_params$P, ", D =", best_params$D, ", Q =", best_params$Q))
  
  # Return the best model and its parameters
  return(list(model = best_model, parameters = list(p = p, d = d, q = q, P = best_params$P, D = best_params$D, Q = best_params$Q, period = period)))
}

##############

# define 'fit_arima_model()' function to use the best parameters found by the find_best_model() function:

fit_arima_model <- function(train_data, best_params, xreg = NULL) {
  # Extract best parameters from the input list
  p <- best_params$p
  d <- best_params$d
  q <- best_params$q
  P <- best_params$P
  D <- best_params$D
  Q <- best_params$Q
  period <- best_params$period
  
  # Fit the ARIMA model with or without external regressors
  if (is.null(xreg)) {
    arima_model <- arima(
      train_data,
      order = c(p, d, q),
      seasonal = list(order = c(P, D, Q), period = period)
    )
  } else {
    arima_model <- arima(
      train_data,
      order = c(p, d, q),
      seasonal = list(order = c(P, D, Q), period = period),
      xreg = xreg  # Include external regressors if provided
    )
  }
  
  return(arima_model)
}

############

# Define 'insamplePI()' function to calculate in-sample prediction intervals
insamplePI <- function(model, confidence_level) {
  alpha <- 1 - confidence_level/100
  z.score <- qnorm(1 - alpha / 2)  
  pi_train <- data.frame(
    lower_bound = as.numeric(fitted(model)) - (z.score * sqrt(model$sigma2)), # RMSE of the model
    upper_bound = as.numeric(fitted(model)) + (z.score * sqrt(model$sigma2)) # RMSE of the model
  )
  return(pi_train)
}

######################################

# Load external data sets
# set path as needed

dog <- read.csv("RawData/dogs.csv", sep=";")
NCP <- read.csv("OutFiles/NCP_by_week.csv", sep=";")
inf <- read.csv("RawData/influenza_formatted.csv", sep=";")
Elm2 <- read.csv("RawData/ElmoreFig2B_data.csv", sep=";")

######################################

# format dog data
dog$WeekDate <- as.Date(dog$date, format="%d.%m.%Y")
dog$Count <- rowMeans(dog[,c("prevalence_since_2019", "prop_unwell")], na.rm=TRUE)

# format NCP data
NCP$WeekDate <- as.Date(NCP$WeekDate)
NCP$Count <- as.numeric(NCP$Count)

# format inf data
# Extract year and week from 'inf$Week' and return the MMWRweekDate
inf$WeekDate <- MMWRweek2Date(as.numeric(substr(inf$Week, 1, 4)), as.numeric(substr(inf$Week, 5, 6)))
inf$Count <- inf$Total.ILI #/ inf$Total.Patients #X.Weighted.ILI    #Total.ILI

# Interpolate missing values using linear interpolation
inf$Count <- na.approx(inf$Count) 

# format Elm data
# Elm$WeekDate <- MMWRweek2Date(Elm$Year, Elm$Week)
# Elm$Count <- Elm$Outpatient_visits

# format Elm2 data
Elm2$WeekDate <- as.Date(Elm2$WeekDate, "%d.%m.%Y")

######################################

# set working data set
#data <- inf[inf$WeekDate > MMWRweek2Date(2008,39),] #Elm2 #dog, NCP, inf
data <- NCP
nz = data$Count != 0
data$logcount = rep(0.00001, length(nz))
data$logcount[nz] = log(data$Count[nz])

######################################
# Extract week 53 data separately
week_53_data <- data[MMWRweek(data$WeekDate)$MMWRweek == 53, ]

# Remove week 53 for modeling
data_model <- data[MMWRweek(data$WeekDate)$MMWRweek != 53,]

# Convert the filtered data to a time series object
xts_data <- xts(data_model$logcount, order.by = as.Date(data_model$WeekDate))

# Find the index of the last training date
train_end <- which(data_model$WeekDate == as.Date("2019-12-01") - 7)

# Split the filtered data into training and testing
train_data <- xts_data[1:train_end]
test_data <- xts_data[(train_end + 1):length(xts_data)]

# check the below if needed
#plot(xts_data)

# Plot the Autocorrelation Function (ACF) and Partial Autocorrelation Function (PACF)
#tsdisplay(train_data)

#length(xts_data)
#length(train_data)
#length(test_data)

######################################

# Find best SARIMA parameters p,d,q and P,D,Q
result <- find_best_model(train_data)
# arima(train_data, order = c(2, 0, 2), seasonal = list(order = c(1, 0, 1), period = 52))
# Extract best parameters
best_params <- result$parameters

# Fit the ARIMA model with the best parameters
themodel <- fit_arima_model(train_data, best_params) 

# Print model summary
summary(themodel)
checkresiduals(themodel)
#tsdisplay(residuals(themodel))
autoplot(themodel)

######################################

# Predicted values
prediction_length = length(test_data)
fc <- forecast(themodel, h = prediction_length, level = c(95, 99))
forecasted = predict(themodel, n.ahead=prediction_length)

# Quickly plot the forecast (time series data)

#plot(fc, main="ARIMA Forecast with Prediction Intervals", ylim=c(min(xts_data), max(xts_data)))
#lines(as.ts(xts_data), col="blue") # full data in blue 
#lines(forecasted$pred, col="red") # double check with test_hat forecasted data

# Combine train and test data
all_dates <- c(time(train_data), time(test_data), as.Date(week_53_data$WeekDate)) # All dates including week 53
all_obs <- c(as.vector(train_data), as.vector(test_data), week_53_data$Count) # Observed values including week 53
all_pred <- c(as.numeric(fitted(themodel)), as.vector(forecasted$pred))   # ! NOT including week 53 !

week_52_dates <- as.Date(week_53_data$WeekDate) - 7  

# Fill week 53 predictions using the corresponding week 52 predictions
week_53_predictions <- all_pred[which(all_dates %in% week_52_dates)] ## Extract week 52 predictions from all_pred
all_pred53 <- c(all_pred, week_53_predictions) # Combine all predictions including week 53

############
# Calculate in-sample prediction intervals

pi95_train <- insamplePI(themodel, 95) # returns df(nrow=length(train_data), ncol=2) with lower_bound in 1st column, upper_bound in 2nd col
pi99_train <- insamplePI(themodel, 99) # returns df(nrow=length(train_data), ncol=2) with lower_bound in 1st column, upper_bound in 2nd col
  
############
# Combine train and test prediction intervals
lwr95_ts <- c(pi95_train[,"lower_bound"], as.numeric(fc$lower[,1])) # 95% in first col of fc$lower
upr95_ts <- c(pi95_train[,"upper_bound"], as.numeric(fc$upper[,1])) # 95% in first col of fc$upper

lwr99_ts <- c(pi99_train[,"lower_bound"], as.numeric(fc$lower[,2])) # 99% in second col of fc$lower
upr99_ts <- c(pi99_train[,"upper_bound"], as.numeric(fc$upper[,2])) # 99% in second col of fc$upper

# Fill week 53 predictions using the corresponding week 52 predictions
lwr95 <- c(lwr95_ts, lwr95_ts[which(all_dates %in% week_52_dates)]) 
upr95 <- c(upr95_ts, upr95_ts[which(all_dates %in% week_52_dates)])

lwr99 <- c(lwr99_ts, lwr99_ts[which(all_dates %in% week_52_dates)])
upr99 <- c(upr99_ts, upr99_ts[which(all_dates %in% week_52_dates)])

# Visualize with LOESS smoothing
smoothed.obs <- predict(loess(all_obs ~ as.numeric(all_dates), span = 0.05)) # for more smoothing try 0.1 or 0.2
smoothed.pred <- predict(loess(all_pred53 ~ as.numeric(all_dates), span = 0.05))

lwr95.sm <- predict(loess(lwr95 ~ as.numeric(all_dates), span = 0.05))
upr95.sm <- predict(loess(upr95 ~ as.numeric(all_dates), span = 0.05))

lwr99.sm <- predict(loess(lwr99 ~ as.numeric(all_dates), span = 0.05))
upr99.sm <- predict(loess(upr99 ~ as.numeric(all_dates), span = 0.05))

# combine into a data frame
all_data <- data.frame(
  date = all_dates,
  observed = exp(all_obs),
  predicted = exp(all_pred53),
  smoothed.obs = exp(smoothed.obs),
  smoothed.pred = exp(smoothed.pred),
  lower_PI95 = exp(lwr95),
  upper_PI95 = exp(upr95),
  lower_PI99 = exp(lwr99),
  upper_PI99 = exp(upr99),
  lower_PI95sm = exp(lwr95.sm),
  upper_PI95sm = exp(upr95.sm),
  lower_PI99sm = exp(lwr99.sm),
  upper_PI99sm = exp(upr99.sm)
)

# Order the data frame by the date column
all_data <- all_data[order(all_data$date), ]

# add a column "category" with the assigned p-value window
all_data$category <- NA

# Loop through each row of the dataframe
for (i in 1:nrow(all_data)) {
  all_data$category[i] <- ifelse(
    (all_data$observed[i] - all_data$lower_PI95sm[i] > 0 & all_data$observed[i] - all_data$upper_PI95sm[i] < 0), "p>0.05",
    ifelse(
      (all_data$observed[i] - all_data$lower_PI99sm[i] < 0 | all_data$observed[i] - all_data$upper_PI99sm[i] > 0), "p<0.01",
      "0.01<=p<=0.05"
    )
  )
}

# remove negative values 
all_data[sapply(all_data, is.numeric)] <- lapply(all_data[sapply(all_data, is.numeric)], 
                                                          function(x) ifelse(x < 0, 0, x))


# Plot observed data with ordered dates
plot(all_data$date, all_data$observed, 
     xlab = "Date", ylab = "Count", main = "Original and Predicted Data with Week 53 Included")
lines(all_data$date, all_data$predicted, col = "red")
lines(all_data$date, all_data$smoothed.pred, col = "green")
lines(all_data$date, all_data$lower_PI95sm, col="blue", lty=2)
lines(all_data$date, all_data$upper_PI95sm, col="blue", lty=2)
lines(all_data$date, all_data$lower_PI99sm, col="grey", lty=3)
lines(all_data$date, all_data$upper_PI99sm, col="grey", lty=3)
legend("topleft", 
       legend = c("Original Data", 
                  "Predicted Data", 
                  "Smoothed Pred.", 
                  "95% pred.int.", 
                  "99% pred.int."), 
       col = c("black", 
               "red", 
               "green", 
               "blue", 
               "grey"), 
       lty = c(1,1,1,2,3), 
       cex = 0.8)


#####################################

#GRAPHING WITH GGPLOT

####################################

# Define the start and end of the forecast
forecast_start <- time(test_data)[1]
forecast_end <- max(time(test_data))

# Filter the dataframe for predicted values
predicted_data <- all_data[all_data$date >= forecast_start, ]
train_data_plot <- all_data[all_data$date < forecast_start, ]

ggplot() +
  geom_ribbon(data = predicted_data, aes(x = date, ymin = lower_PI95sm, ymax = upper_PI95sm, fill = "forecast 95% prediction interval"), alpha = 0.6) +
  
  # In-sample 95% prediction interval ribbon
  geom_ribbon(data = train_data_plot, aes(x = date, ymin = lower_PI95sm, ymax = upper_PI95sm, fill = "in-sample 95% prediction interval"), alpha = 0.6) +
  
  # Forecast 99% prediction interval ribbon
  geom_ribbon(data = predicted_data, aes(x = date, ymin = lower_PI99sm, ymax = upper_PI99sm, fill = "forecast 99% prediction interval"), alpha = 0.4) +
  
  # In-sample 99% prediction interval ribbon
  geom_ribbon(data = train_data_plot, aes(x = date, ymin = lower_PI99sm, ymax = upper_PI99sm, fill = "in-sample 99% prediction interval"), alpha = 0.4) +
  
  # Points for observed data, color by category
  geom_point(data = all_data, aes(x = date, y = observed, color = category), shape = 16) +
  
  # Line for smoothed predicted data
  geom_line(data = all_data, aes(x = date, y = smoothed.pred, color = "modeled trend line")) +
  
  # Points for predicted data
  geom_point(data = predicted_data, aes(x = date, y = predicted, color = "predicted"), shape = 2) +
  
  # Add vertical line at the start of the forecast
  geom_vline(xintercept = as.numeric(forecast_start), color = "orange", linetype = "dotted", size=1) +
  
  # Add text label for the vertical line
  annotate("text", x = forecast_start, y = 0.6*max(all_data$observed), label = "start of forecast\n1/Dec/2019", 
           hjust = -0.1, vjust = -0.3, color = "orange", angle = 90, size = 4) +
  
  # Minimal theme layout
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
        axis.line = element_line(color = "black", size = 0.5),
        axis.ticks = element_line(color = "black", size = 0.5)) +
  
  # Customize colors and fills
  scale_color_manual(name = "Series", values = c("modeled trend line" = "blue", "observed" = "black", "predicted" = "black", "p>0.05" = "darkgreen", "p<0.01" = "red", "0.01<=p<=0.05" = "orange")) +
  scale_fill_manual(
    name = "Intervals",
    values = c("in-sample 95% prediction interval" = "darkgray", 
               "in-sample 99% prediction interval" = "lightgray",
               "forecast 95% prediction interval" = "lightblue",
               "forecast 99% prediction interval" = "lightblue1"),
    labels = str_wrap(c("in-sample 95% prediction interval",
                        "in-sample 99% prediction interval",
                        "forecast 95% prediction interval",
                        "forecast 99% prediction interval"
    ), width = 25)
  ) +
  
  # Labels and theme
  labs(title = "Arima modelled time series", 
       x = "", y = "")
  
  # Ensure points and lines have separate legends
  guides(
    color = guide_legend(
      order = 1, # 'Series' legend appears first
      override.aes = list(
        linetype = c(1, NA, NA, NA),  # Line type for Smoothed, None for Observed and Predicted
        shape = c(NA, 16, 2, 16, 16,16)     # Shape for Observed and Predicted, None for Smoothed
      )
    ),
    fill = guide_legend(order = 2)  # No customization needed for fill
  )
  
  # Set x-axis limits to start exactly at forecast_start
  #scale_x_date(limits = c(forecast_start, forecast_end), 
  #     date_breaks = "8 week", date_labels = "w%U-%Y") +  # '%U' for week number
  
  

#ggsave("NCPzoom.png", width=8, height=5)
############################################

NCP.plot +
  labs(title = "Equine nocardioform placentitis (weekly cases)", x = "", y = "") +
  scale_fill_manual(
    name = "Intervals",
    values = c(#"in-sample 95% prediction interval" = "darkgray", 
               #"in-sample 99% prediction interval" = "lightgray",
               "forecast 95% prediction interval" = "lightblue",
               "forecast 99% prediction interval" = "lightblue1"),
    labels = str_wrap(c("in-sample 95% prediction interval",
                        "in-sample 99% prediction interval",
                        "forecast 95% prediction interval",
                        "forecast 99% prediction interval"
    ), width = 25)
  ) +
  scale_x_date(limits = c(forecast_start, forecast_end), 
    date_breaks = "4 week", date_labels = "w%U-%Y")   # '%U' for week number   


############################################

# combine all plots and align x-axes

dog.plot+ scale_x_date(limits = c(start(xts_data), end(xts_data))) + 
  theme(legend.position="right") +
  
Elm2.plot+theme(legend.position="none")+ #Elm2 plot without legend
  theme(legend.position="none") +
  
  plot_annotation(title = 'Arima models')+
  plot_layout(nrow=2)