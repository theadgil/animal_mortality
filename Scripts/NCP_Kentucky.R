################################################################
################################################################
#####               R-Script: NCP_Kentucky                 #####
#####      Dorothea Gilbert & Matz A. Haugen (2023):       #####
#####                      03.04.2023				               #####
################################################################
################################################################


#install.packages("MMWRweek")
#install.packages("ggplot2")



library(MMWRweek)
library(ggplot2)

# set path as needed

#setwd("~/RawData/OutFiles")



################################################################
#### Load raw data 
################################################################

# the raw data can be downloaded from the University of Kentucky's Veterinary Diagnostic Laboratory (VDL) Dashboard: Historical
# picture of nocardioform placentitis in Kentucky 

# https://nam04.safelinks.protection.outlook.com/?url=http%3A%2F%2Fvdl.uky.edu%3A8080%2Finformer%2FDashboardViewer.html%3Flocale%3Den_US%26embedToken%3Dd58d8dbf-bef0-4619-b57c-9d4343260338&data=02%7C01%7Chfwiem2%40uky.edu%7Cfa8358fd86d54918098808d7a8e987b4%7C2b30530b69b64457b818481cb53d42ae%7C0%7C0%7C637163593278757986&sdata=xctfebsHFsOyY%2BttUSbOzFvXJHJUAfO9bgrfQv17oL4%3D&reserved=0


# Using read.csv() to read all downloaded csv-files, put in list
# bind with rbind

list_csv_files <- list.files(path = "~/Kentucky",
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

################################################################

### aggregate by week

NCP_week <- aggregate(NCP, by=list(NCP$WeekDate), FUN=length)[,1:2]
colnames(NCP_week) <- c("WeekDate","Count")
NCP_week$WeekDate <- as.Date(NCP_week$WeekDate, "%Y-%m-%d")


################################################################
#### Graphics 
################################################################

### histogram

hist(as.numeric(NCP$Week))


### NCP cases per week since 2010

theme_set(theme_bw())
ggplot(NCP_week)+
  geom_point(aes(x=WeekDate,y=Count))+
  scale_x_date(#limits = c(as.Date("2019-01-01"), NA),
    date_breaks = '2 year', date_labels = '%Y')+
  labs(title = 'Equine nocardioform placentitis in Kentucky',
       #subtitle ='',
       #caption = '',
       x = '', y="Count per week")

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


################################################################

#write.table(NCP_week, file="NCP_by_week.csv",sep=";",row.names=T)




