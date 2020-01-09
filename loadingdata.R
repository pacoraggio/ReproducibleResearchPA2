# Loading data
# https://www.ncdc.noaa.gov/stormevents/choosedates.jsp?statefips=-999%2CALL

rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)

names <- read.csv("./data/repdata_data_StormData.csv/repdata_data_StormData.csv", 
                  nrows = 10, header = TRUE)
names(names)

start.time <- Sys.time()
df.rawdata <- read.csv("./data/repdata_data_StormData.csv.bz2",
                    colClasses = c("character", 
                                   "character", 
                                   rep("NULL",4), 
                                   rep("character",2),
                                   rep("NULL",12),
                                   "character","numeric",
                                   rep("numeric",3),
                                   "character", 
                                   "numeric", 
                                   "character", 
                                   rep("NULL",9)))
end.time <- Sys.time()
time.taken <- end.time - start.time 
time.taken

names(df.rawdata)
tail(df.rawdata$BGN_DATE)

## convert Begin Date to Date format
a1 <- as.Date(sub(" .*", "", df.rawdata$BGN_DATE), format("%m/%d/%Y"))

## Considering events where at least one fatality, injury, prop or crop demage
## happened 

df.rawdatared <- df.rawdata[!(df.rawdata$FATALITIES == 0 &
                              df.rawdata$INJURIES == 0 &
                              df.rawdata$PROPDMG == 0 &
                              df.rawdata$CROPDMG == 0),]



## Event type
## to be placed before flood
unique(grep("Lakes",df.rawdatared$EVTYPE, ignore.case = TRUE,value = TRUE))


unique(grep("hvy",df.rawdatared$EVTYPE, ignore.case = TRUE,value = TRUE))
l <- gsub("\bhv\b", "Heavy Rain", df.rawdata$EVTYPE, ignore.case = TRUE)

unique(grep("Heavy",l, ignore.case = FALSE,value = TRUE))

unique(grep("heavy sn",df.rawdatared$EVTYPE, ignore.case = TRUE,value = TRUE))
unique(grep("surf",df.rawdatared$EVTYPE, ignore.case = TRUE,value = TRUE))
unique(grep("wind",df.rawdatared$EVTYPE, ignore.case = TRUE,value = TRUE))
unique(grep("Hur",df.rawdatared$EVTYPE, ignore.case = TRUE,value = TRUE))
unique(grep("ice s",df.rawdatared$EVTYPE, ignore.case = TRUE,value = TRUE))
unique(grep("Lake-",df.rawdatared$EVTYPE, ignore.case = TRUE,value = TRUE))
unique(grep("Lake e",df.rawdatared$EVTYPE, ignore.case = TRUE,value = TRUE))


df.rawdata[df.rawdata$EVTYPE == "Thundersnow shower",]

unique(df.rawdata$PROPDMGEXP)
df.rawdata[df.rawdata$CROPDMGEXP == "?",]


df1950 <- df.rawdata[grep("1950", df.rawdata$BGN_DATE, ignore.case = TRUE),]

sum(df1950$CROPDMG)

df.rawdatared <- df.rawdata[!(df.rawdata$FATALITIES == 0 &
                   df.rawdata$INJURIES == 0 &
                   df.rawdata$PROPDMG == 0 &
                   df.rawdata$CROPDMG == 0),]

df.pophealth <- df.rawdatared[((df.rawdatared$FATALITIES != 0) |
                             (df.rawdatared$INJURIES != 0)),]


df.economicdDMG <- df.rawdatared[df.rawdatared$PROPDMG != 0 |
                               df.rawdatared$CROPDMG != 0,] 

head(df.pophealth)
head(df.economicdDMG)

dim(df.rawdatared)[1] - dim(df.economicdDMG)[1]

unique(df.rawdatared$EVTYPE)

df.stm <- df.rawdatared[grep("TSTM", 
                    df.rawdatared$EVTYPE, 
                    ignore.case = TRUE),]

df.snow <- df.rawdatared[grep("snow", 
                             df.rawdatared$EVTYPE, 
                             ignore.case = TRUE),]


df.ice <- df.rawdatared[grep("ice", 
                              df.rawdatared$EVTYPE, 
                              ignore.case = TRUE),]

unique(df.snow$EVTYPE)
unique(df.ice$EVTYPE)

head(df.rawdata)