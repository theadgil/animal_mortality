################################################################
################################################################
#####               R-Script: NCP_Kentucky                 #####
#####      Dorothea Gilbert & Matz A. Haugen (2023):       #####
#####                      03.04.2023				               #####
################################################################
################################################################


#install.packages("MMWRweek")
#install.packages("ggplot2")


library("dplyr")
library("tidyr")
library("lubridate")
library(MMWRweek)
library(ggplot2)
library(zoo)
# set path as needed

#setwd("~/RawData/OutFiles")



################################################################
#### Load raw data 
################################################################

# the raw data can be downloaded from the University of Kentucky's Veterinary Diagnostic Laboratory (VDL) Dashboard: Historical
# picture of nocardioform placentitis in Kentucky 

# http://vdlmaps.vdl.uky.edu:8080/informer/DashboardViewer.html?locale=en_US&embedToken=d58d8dbf-bef0-4619-b57c-9d4343260338

# Using read.csv() to read all downloaded csv-files, put in list
# bind with rbind

list_csv_files <- list.files(path = "~/animal_mortality/RawData/NCP",
                             pattern="*.csv", full.names = TRUE)
NCP = do.call(rbind, lapply(list_csv_files, function(x) read.csv(x, stringsAsFactors = FALSE)))

################################################################

# check formatting with str()


### format date

NCP$SubmittedDate <- as.Date(NCP$SubmittedDate,"%Y-%m-%d %H:%M:%S")

### write MMWR week into new column

NCP$Week <- MMWRweek(NCP$SubmittedDate)[,2]
NCP$Week <- sprintf("%02d", NCP$Week)

NCP$Year <- as.integer(c(strftime(NCP$SubmittedDate, format = "%Y")))
NCP$WeekYr <- c(paste(NCP$Year,NCP$Week,sep="-"))
NCP$WeekYear <- as.character(strftime((NCP$SubmittedDate), "%Y-%U",tz="CET"))

NCP$WeekDate <- cut(as.Date(NCP$SubmittedDate), "week",start.on.monday = FALSE)

NCP$Month <- as.character(strftime((NCP$SubmittedDate), "%m",tz="CET"))
NCP$MonthDate <- as.Date(paste(NCP$Year, NCP$Month, "01", sep="-"), "%Y-%m-%d")

################################################################

### aggregate by week

NCP_week <- aggregate(NCP, by=list(NCP$WeekDate), FUN=length)[,1:2]
colnames(NCP_week) <- c("WeekDate","Count")
NCP_week$WeekDate <- as.Date(NCP_week$WeekDate, "%Y-%m-%d")

### aggregate by month

NCP_month <- aggregate(NCP, by=list(NCP$MonthDate), FUN=length)[,1:2]
colnames(NCP_month) <- c("MonthDate","Count")


################################################################
#### Graphics 
################################################################

### histogram

hist(as.numeric(NCP$Week))

### NCP cases per week since 2010

theme_set(theme_bw())
ggplot(NCP_week)+
  geom_line(aes(x=WeekDate,y=Count))+
  scale_x_date(#limits = c(as.Date("2019-01-01"), NA),
    date_breaks = '2 year', date_labels = '%Y')+
  labs(title = 'Equine nocardioform placentitis in Kentucky',
       #subtitle ='',
       #caption = '',
       x = '', y="Count per week")

ggsave("NCP_by_week_since2010.png", width=8, height=5)

### NCP cases per month since 2010

theme_set(theme_bw())
ggplot(NCP_month)+
  geom_line(aes(x=MonthDate,y=Count))+
  scale_x_date(#limits = c(as.Date("2019-01-01"), NA),
    date_breaks = '2 year', date_labels = '%Y')+
  labs(title = 'Equine nocardioform placentitis in Kentucky',
       #subtitle ='',
       #caption = '',
       x = '', y="Count per month")

ggsave("NCP_by_month_since2010.png", width=8, height=5)

################################################################

### NCP cases per week since 2019

theme_set(theme_bw())
ggplot(NCP_week)+
  geom_line(aes(x=WeekDate,y=Count))+
  scale_x_date(limits = c(as.Date("2019-01-01"), NA),
               date_breaks = '4 weeks', date_labels = 'w%V-%y')+
  labs(title = 'Equine nocardioform placentitis in Kentucky',
       #subtitle ='',
       #caption = '',
       x = '', y="Count per week")+
  theme(axis.text.x = element_text(angle = 90,hjust = 1, vjust = 0.5),
        strip.text=element_text(size=10))

ggsave("NCP_by_week_since2019.png", width=8, height=5)

################################################################

#write.table(NCP_week, file="NCP_by_week.csv",sep=";",row.names=T)

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

options(repr.plot.width=8, repr.plot.height=5)
b = ggplot(state_centered, aes(x=WeekDate, y=centered)) + geom_line() +
 geom_ribbon(aes(ymin=centered_low, ymax=centered_high), alpha = 0.2) +
 labs(x = "Time", y = "Weekly Deaths [x over 2010-2020 median]", title="Weekly deaths from nocardioform placentitis") + 
theme_gray(base_size = 14) 

ggsave("excessNCP.png", width=8, height=5)

