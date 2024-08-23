# Load required libraries
library(MMWRweek)
library(ggplot2)
library(zoo)
library(dplyr)
library(tidyr)
library(lubridate)

# Define functions
find_median = function(values, dates, train_before=2020) {
    idx = year(dates) < train_before
    state_deaths = data.frame(weeks=MMWRweek(NCP_week$WeekDate)$MMWRweek, values=values, dates=dates)
    median = state_deaths[idx,] %>% group_by(weeks) %>% summarise(med = median(values, na.rm=TRUE))
    merged = merge(state_deaths, median, by="weeks")
    merged$med[order(merged$dates)]
}

find_quantile = function(values, dates, q = .95, train_before=2020) {
    idx = year(dates) < train_before
    state_deaths = data.frame(weeks=MMWRweek(NCP_week$WeekDate)$MMWRweek, values=values, dates=dates)
    median = state_deaths[idx,] %>% group_by(weeks) %>% summarise(est = quantile(values, probs=c(q), na.rm=TRUE))
    merged = merge(state_deaths, median, by="weeks")
    merged$est[order(merged$dates)]
}

find_sd = function(values, dates, train_before=2020) {
    idx = year(dates) < train_before
    state_deaths = data.frame(weeks=MMWRweek(NCP_week$WeekDate)$MMWRweek, values=values, dates=dates)
    std = state_deaths[idx,] %>% group_by(weeks) %>% summarise(std = sd(values, na.rm=TRUE) / sqrt(length(values)))
    merged = merge(state_deaths, std, by="weeks")
    merged$std[order(merged$dates)]
}

# Load and process data
list_csv_files <- list.files(path = "~/animal_mortality/RawData/NCP",
                             pattern="*.csv", full.names = TRUE)
NCP = do.call(rbind, lapply(list_csv_files, function(x) read.csv(x, stringsAsFactors = FALSE)))

NCP$SubmittedDate <- as.Date(NCP$SubmittedDate,"%Y-%m-%d %H:%M:%S")

NCP$Week <- MMWRweek(NCP$SubmittedDate)[,2]
NCP$Week <- sprintf("%02d", NCP$Week)

NCP$Year <- as.integer(c(strftime(NCP$SubmittedDate, format = "%Y")))
NCP$WeekYr <- c(paste(NCP$Year,NCP$Week,sep="-"))
NCP$WeekYear <- as.character(strftime((NCP$SubmittedDate), "%Y-%U",tz="CET"))

NCP$WeekDate <- cut(as.Date(NCP$SubmittedDate), "week",start.on.monday = FALSE)

# Aggregate by week
NCP_week <- aggregate(NCP, by=list(NCP$WeekDate), FUN=length)[,1:2]
colnames(NCP_week) <- c("WeekDate","Count")
NCP_week$WeekDate <- as.Date(NCP_week$WeekDate, "%Y-%m-%d")

# Plot NCP cases per week since 2010
theme_set(theme_bw())
ggplot(NCP_week)+
  geom_line(aes(x=WeekDate,y=Count))+
  scale_x_date(date_breaks = '2 year', date_labels = '%Y')+
  labs(title = 'Equine nocardioform placentitis in Kentucky',
       x = '', y="Count per week")

# Process data for centered plot
NCP_week = NCP_week %>% mutate(ma=rollapply(log(Count, base = exp(1)),6,mean,align='center',fill=NA))

state_centered = NCP_week %>% reframe(
    ma = ma,
    Count=Count,
    logcount = log(Count),
    median = find_median(ma, WeekDate),
    std = find_sd(ma, WeekDate),
    upper = find_quantile(ma, WeekDate, q=0.975),
    lower = find_quantile(ma, WeekDate, q=0.025),
    WeekDate = WeekDate)
state_centered$centered = exp(state_centered$ma - state_centered$median) - 1
state_centered$centered_high = exp(state_centered$ma - (state_centered$lower)) - 1
state_centered$centered_low = exp(state_centered$ma - (state_centered$upper)) - 1

# Plot centered data
options(repr.plot.width=8, repr.plot.height=5)
b = ggplot(state_centered, aes(x=WeekDate, y=centered)) + 
  geom_line() +
  geom_ribbon(aes(ymin=centered_low, ymax=centered_high), alpha = 0.2) +
  labs(x = "Time", y = "Weekly Deaths [x over 2010-2020 median]", title="Weekly deaths from neocardioform placentis") + 
  theme_gray(base_size = 14) 
print(b)

# Save plot
ggsave("excessNCP.png", width=8, height=5)