# Load required libraries
library(MMWRweek)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(zoo)
library(ISOweek)

# Define functions
find_median = function(values, dates, train_before="2019-10-01") {
    idx = dates < train_before
    state_deaths = data.frame(weeks=MMWRweek(data$WeekDate)$MMWRweek, values=values, dates=dates)
    median = state_deaths[idx,] %>% group_by(weeks) %>% summarise(med = median(values, na.rm=TRUE))
    merged = merge(state_deaths, median, by="weeks")
    merged$med[order(merged$dates)]
}

find_quantile = function(values, dates, q = .95, train_before="2019-10-01", before_month=6) {
    idx = dates < train_before
    state_deaths = data.frame(weeks=MMWRweek(data$WeekDate)$MMWRweek, values=values, dates=dates)
    median = state_deaths[idx,] %>% group_by(weeks) %>% summarise(est = quantile(values, probs=c(q), na.rm=TRUE))
    merged = merge(state_deaths, median, by="weeks")
    merged$est[order(merged$dates)]
}

find_sd = function(values, dates, train_before="2019-10-01", before_month=6) {
    idx = dates < train_before
    state_deaths = data.frame(weeks=MMWRweek(data$WeekDate)$MMWRweek, values=values, dates=dates)
    std = state_deaths[idx,] %>% group_by(weeks) %>% summarise(std = sd(values, na.rm=TRUE) / sqrt(length(values)))
    merged = merge(state_deaths, std, by="weeks")
    merged$std[order(merged$dates)]
}

# Read and process data
data = read.csv("~/animal_mortality/RawData/Elmore_data.csv", sep=";")
data$Week <- sprintf("W%02d", data$Week)
data$Count = data$Outpatient_visits
data$WeekYear <- c(paste(data$Year,data$Week,"1",sep="-"))
data$WeekDate = ISOweek2date((data$WeekYear))
head(data)

# Plot Influenza cases from Elmore
theme_set(theme_bw())
ggplot(data)+
  geom_line(aes(x=WeekDate,y=Outpatient_visits))+
  scale_x_date(date_breaks = '2 year', date_labels = '%Y')+
  labs(title = 'Influenza cases from Elmore',
       x = '', y="Count per week")

# Process data for centered plot
data = data %>% mutate(ma=rollapply(log(Count, base = exp(1)),6,mean,align='center',fill=NA))

data_centered = data %>% reframe(
    ma = ma,
    Count=Count,
    logcount = log(Count),
    median = find_median(ma, WeekDate),
    std = find_sd(ma, WeekDate),
    upper = find_quantile(ma, WeekDate, q=0.975),
    lower = find_quantile(ma, WeekDate, q=0.025),
    WeekDate = WeekDate)
data_centered$centered = exp(data_centered$ma - data_centered$median) - 1
data_centered$centered_high = exp(data_centered$ma - (data_centered$lower)) - 1
data_centered$centered_low = exp(data_centered$ma - (data_centered$upper)) - 1
head(data_centered)

# Plot centered data
options(repr.plot.width=8, repr.plot.height=5)
b = ggplot(data_centered, aes(x=WeekDate, y=centered)) + 
  geom_line() +
  geom_ribbon(aes(ymin=centered_low, ymax=centered_high), alpha = 0.2) +
  labs(x = "Time", y = "Weekly cases [x over 2014-2020 median]", title="Weekly influenza cases") + 
  theme_gray(base_size = 14) 
print(b)
ggsave("excessElmore.png", width=8, height=5)

# Plot moving average compared with weekly median
options(repr.plot.width=8, repr.plot.height=5)
b = ggplot(data_centered, aes(x=WeekDate, y=exp(ma))) + 
  geom_line() +
  geom_line(aes(x=WeekDate, y=exp(median))) +
  labs(x = "Time", y = "Weekly cases [x over 2014-2020 median]", title="Weekly influenza cases") + 
  theme_gray(base_size = 14) 
print(b)