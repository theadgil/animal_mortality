################################################################
################################################################
#####               R-Script: Binning data by week         #####
#####      Dorothea Gilbert & Matz A. Haugen (2023):       #####
#####                      03.04.2023				               #####
################################################################
################################################################


#install.packages("MMWRweek")
#install.packages("utils")
#install.packages("gdata")

library(utils)
library(MMWRweek)
library(gdata)

# set working directory as needed

#setwd("~/RawData")



################################################################
#### Load raw data 
################################################################

THA <- read.csv("~/Corona/Mortality Data/Covid_Thailand_daily.csv", header=TRUE, comment.char="#", sep=";")
ECU <- read.csv("~/Corona/Manuscript/RawData/Covid_Ecuador_daily.csv", header=TRUE, comment.char="#", sep=";")
GER <- read.csv("~/Corona/Manuscript/RawData/DeStatis_daily_deaths.csv", header=TRUE, comment.char="#", sep=";")
ECDC <- read.csv("~/Corona/Manuscript/RawData/historic_ECDC_covid_data[download20210713].csv", header=TRUE, comment.char="#")

# uncomment the next line for online retrieval of ECDC-Covid data
# ECDC <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")

birds <- read.csv("~/Corona/Manuscript/RawData/birds.csv", header=TRUE, comment.char="#", sep=";")
horses <- read.csv("~/Corona/Mortality Data/horses.csv", header=TRUE, comment.char="#", sep=";")




################################################################
#### Format raw data 
################################################################

### format date

#check formatting with str()

THA$date <- as.Date(THA$date,"%d.%m.%Y")
ECU$date <- as.Date(ECU$date,"%d.%m.%Y")
GER$date <- as.Date(GER$date,"%d.%m.%Y")
birds$date <- as.Date(birds$date,"%d.%m.%Y")
horses$date <- as.Date(horses$date,"%d.%m.%Y")

str(ECDC)
ECDC$dateRep <- as.Date(ECDC$dateRep,"%d/%m/%Y")

### write MMWR week into new column

THA$Week <- MMWRweek(THA$date)[,2]
ECU$Week <- MMWRweek(ECU$date)[,2]
GER$Week <- MMWRweek(GER$date)[,2]
birds$Week <- MMWRweek(birds$date)[,2]
horses$Week <- MMWRweek(horses$date)[,2]
ECDC$Week <- MMWRweek(ECDC$dateRep)[,2]



################################################################
#### subset ECDC data
################################################################

Europe <- subset(ECDC, continentExp == "Europe")
Europe$geoId <- as.factor(Europe$geoId)
#length(levels(Europe$geoId))

ECDC22 <-rbind(
  subset(ECDC, countriesAndTerritories == "Austria"),
  subset(ECDC, countriesAndTerritories == "Belgium"),
  subset(ECDC, countriesAndTerritories == "Cyprus"),
  subset(ECDC, countriesAndTerritories == "Denmark"),
  subset(ECDC, countriesAndTerritories == "Estonia"),
  subset(ECDC, countriesAndTerritories == "Finland"),
  subset(ECDC, countriesAndTerritories == "France"),
  subset(ECDC, countriesAndTerritories == "Germany"),
  subset(ECDC, countriesAndTerritories == "Greece"),
  subset(ECDC, countriesAndTerritories == "Hungary"),
  subset(ECDC, countriesAndTerritories == "Ireland"),
  subset(ECDC, countriesAndTerritories == "Israel"),
  subset(ECDC, countriesAndTerritories == "Italy"),
  subset(ECDC, countriesAndTerritories == "Luxembourg"),
  subset(ECDC, countriesAndTerritories == "Netherlands"),
  subset(ECDC, countriesAndTerritories == "Norway"),
  subset(ECDC, countriesAndTerritories == "Portugal"),
  subset(ECDC, countriesAndTerritories == "Slovenia"),
  subset(ECDC, countriesAndTerritories == "Spain"),
  subset(ECDC, countriesAndTerritories == "Sweden"),
  subset(ECDC, countriesAndTerritories == "Switzerland"),
  subset(ECDC, countriesAndTerritories == "United_Kingdom")
)

ECDC_LUX <- subset(ECDC, countriesAndTerritories == "Luxembourg")



################################################################
#### Bin data by week
################################################################

### aggregate daily deaths by week 

THA_wk <- aggregate(THA[,c("Covid_THA")], by=list(THA$Week), FUN=sum, na.rm=TRUE)
colnames(THA_wk) <- c("WEEK","Covid_THA")

ECU_wk <- aggregate(ECU[,c("Covid_ECU")], by=list(ECU$Week), FUN=sum, na.rm=TRUE)
colnames(ECU_wk) <- c("WEEK","Covid_ECU")

GER_wk <- aggregate(GER[,c("GER_total_DeStatis")], by=list(GER$Week), FUN=sum, na.rm=TRUE)
colnames(GER_wk) <- c("WEEK","total_GER")

birds_wk <- aggregate(birds[,c("tits_LUX", "tits_GER")], by=list(birds$Week), FUN=sum, na.rm=TRUE)
colnames(birds_wk) <- c("WEEK","birds_LUX", "birds_GER")

horses_wk <- aggregate(horses[,c("AHS_deaths", "AHS_cases")], by=list(horses$Week), FUN=sum, na.rm=TRUE)
colnames(horses_wk) <- c("WEEK","horses_deaths", "horses_cases")

ECDC55_wk <- aggregate(Europe[,c("deaths")], by=list(Europe$Week), FUN=sum, na.rm=TRUE)
colnames(ECDC55_wk) <- c("WEEK","Covid_ECDC55")

ECDC22_wk <- aggregate(ECDC22[,c("deaths")], by=list(ECDC22$Week), FUN=sum, na.rm=TRUE)
colnames(ECDC22_wk) <- c("WEEK","Covid_ECDC22")

ECDC_LUX_wk <- aggregate(ECDC_LUX[,c("deaths")], by=list(ECDC_LUX$Week), FUN=sum, na.rm=TRUE)
colnames(ECDC_LUX_wk) <- c("WEEK","Covid_LUX")




################################################################
#### Bind columns for weeks 10 to 25
################################################################

# make a Week and Date column

weekly <- data.frame(week = c(10:25),
                     date = seq.Date(from = as.Date('2020-03-01'), to = as.Date('2020-06-20'), by = 'weeks'))


# requires gbind package loaded

weekly <- cbindX(weekly,
    GER_wk[GER_wk$WEEK > 9 & GER_wk$WEEK < 26,],
    THA_wk[THA_wk$WEEK > 9 & THA_wk$WEEK < 26,],
    ECU_wk[ECU_wk$WEEK > 9 & ECU_wk$WEEK < 26,],
    ECDC55_wk[ECDC55_wk$WEEK > 9 & ECDC55_wk$WEEK < 26,],
    ECDC22_wk[ECDC22_wk$WEEK > 9 & ECDC22_wk$WEEK < 26,],
    ECDC_LUX_wk[ECDC_LUX_wk$WEEK > 9 & ECDC_LUX_wk$WEEK < 26,],
    birds_wk[birds_wk$WEEK > 9 & birds_wk$WEEK < 26,],
    horses_wk[horses_wk$WEEK > 9 & horses_wk$WEEK < 26,]
)

weekly <- weekly[,!names(weekly) %in% c("WEEK")]



################################################################
#### Write to CSV-file 
################################################################

#write.table(weekly, file="weekly_deaths.csv",sep=";",row.names=T)


