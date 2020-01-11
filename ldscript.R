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
                                   rep("NULL",14),
                                   rep("numeric",3),
                                   "character", 
                                   "numeric", 
                                   "character", 
                                   rep("NULL",9)))
end.time <- Sys.time()
time.taken <- end.time - start.time 
time.taken

names(df.rawdata)

## Considering events where at least one fatality, injury, prop or crop demage
## happened 

df.rawdatared <- df.rawdata[!(df.rawdata$FATALITIES == 0 &
                              df.rawdata$INJURIES == 0 &
                              df.rawdata$PROPDMG == 0 &
                              df.rawdata$CROPDMG == 0),]

## convert Begin Date to Date format
df.rawdatared$BGN_DATE <- as.Date(sub(" .*", "", df.rawdatared$BGN_DATE), format("%m/%d/%Y"))

## Event type
# Copy of dataframe

df.test <- df.rawdatared

df.test[grep("Winter w",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Winter Wheather"
df.test[grep("Agricultural f",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Extreme Cold Wind Chill"
df.test[grep("Funn",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Funnel Cloud"
df.test[grep("Marine ha",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Marine Hail"
df.test[grep("Marine str",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Marine Strong Wind"
df.test[grep("Marine th",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Marine Thunderstorm Wind"
df.test[grep("Marine ts",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Marine Thunderstorm Wind"
df.test[grep("Marine",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Marine High Wind"
df.test[grep("Astronomical L",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Astronomical Low Tide"
df.test[grep("Ava",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Avalanche"
df.test[grep("Bli",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Blizzard"
df.test[grep("Coastal f",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Coastal Flood"
df.test[grep("Chil",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Cold Wind Chill"
df.test[grep("Extreme c",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Extreme Cold Wind Chill"
df.test[grep("Dense f",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Dense Fog"
df.test[grep("Dense s",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Dense Smoke"
df.test[grep("Drough",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Drought"
df.test[grep("Devi",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Dust Devil"
df.test[grep("Dust s",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Dust Storm"
df.test[grep("Dust",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Dust Storm"
df.test[grep("Extreme h",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Excessive Heat"
df.test[grep("Excessive h",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Excessive Heat"
df.test[grep("Cold/win",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Excessive Cold Wind Chill"
df.test[grep("Flash",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Flash Flood"
df.test[grep("Frost",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Frost Freeze"
df.test[grep("Funne",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Funnel Cloud"
df.test[grep("Freezing Fog",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Freezing Fog"
df.test[grep("Hail",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Hail"
df.test[grep("Heat",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Heat"
df.test[grep("heavy r",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Heavy Rain"
df.test[grep("hvy r",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Heavy Rain"
df.test[grep("heavy s",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Heavy Snow"
df.test[grep("high tide",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "High Surf"
df.test[grep("high s",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "High Surf"
df.test[grep("high w",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "High Surf"
df.test[grep("Typ",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Hurricane Typhoon"
df.test[grep("Hur",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Hurricane Typhoon"
df.test[grep("Ice S",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Ice storm"
df.test[grep("Lakes",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Lakeshore Flood"

df.test[grep("Lake-",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Lake effect Snow"
df.test[grep("Lake e",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Lake effect Snow"
df.test[grep("Lightn",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Lighting"
df.test[grep("Lighti",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Lighting"
df.test[grep("rip c",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Rip Current"
df.test[grep("seic",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Seiche"
df.test[grep("sle",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Sleet"
df.test[grep("Storm s",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Storm Surge"
df.test[grep("Storm w",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Strong Wind"
df.test[grep("Thunderstorm",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Thunderstorm Wind"
df.test[grep("tstm w",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Thunderstorm Wind"
df.test[grep("tstmw",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Thunderstorm Wind"

df.test[grep("Tornado",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Tornado"
df.test[grep("Tropical de",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Tropical Depression"
df.test[grep("Tropical st",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Tropical Storm"
df.test[grep("Tsu",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Tsunami"
df.test[grep("Volc",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Volcanic Ash"
df.test[grep("Waters",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Waterspout"
df.test[grep("Wild",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Wildfire"
df.test[grep("Winter",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Winter Storm"

df.test[grep("Waters",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Waterspout"
df.test[grep("Wild",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Wildfire"

df.test[grep("Fl",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Flood"
df.test[grep("THUNDERSTORM",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Thunderstorm"

df.test[grep("Ice",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Cold Wind Chill"
df.test[grep("Ic",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Cold Wind Chill"
df.test[grep("snow",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Extreme Cold Wind Chill"
df.test[grep("Hypot",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Extreme Cold Wind Chill"
df.test[grep("Wind",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Extreme Cold Wind Chill"
df.test[grep("Cold",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Extreme Cold Wind Chill"

df.test[grep("Torn",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Tornado"
df.test[grep("Hyper",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Extreme Cold Wind Chill"
df.test[grep("Fire",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Wildfire"
df.test[grep("Rain",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Heavy Rain"
df.test[grep("surf",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "High Surf"
df.test[grep("freez",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Frost Freeze"
df.test[grep("fog",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Dense Fog"
df.test[grep("storm",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Winter Storm"
df.test[grep("spout",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Waterspout"
df.test[grep("slide",df.test$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Avalanche"


length(unique(df.test$EVTYPE))
r <- unique(df.test$EVTYPE)
sort(r[r != toupper(r)])
sort(unique(varnames))

unique(df.test$EVTYPE) == toupper(unique(df.test$EVTYPE))

length(unique(df.rawdatared$EVTYPE))

varnames <- c("Funnel Cloud","Astronomical Low Tide","Avalanche","Blizzard","Coastal Flood",
              "Cold Wind Chill","Extreme Cold Wind Chill","Dense Fog","Dense Smoke","Drought",
              "Dust Devil","Dust Storm","Excessive Heat","Excessive Heat","Excessive Cold Wind Chill",
              "Flash Flood","Frost Freeze","Funnel Cloud","Freezing Fog","Hail","Heat","Heavy Rain",
              "Heavy Rain","Heavy Snow","High Surf","High Surf","High Surf","Hurricane Typhoon",
              "Hurricane Typhoon","Ice storm","Lakeshore Flood","Lake effect Snow","Lake effect Snow",
              "Lighting","Lighting","Marine Hail","Marine Strong Wind","Marine Thunderstorm Wind",
              "Marine Thunderstorm Wind","Marine High Wind","Rip Current","Seiche","Sleet",
              "Storm Surge","Strong Wind","Thunderstorm Wind","Thunderstorm Wind","Tornado",
              "Tropical Depression","Tropical Storm","Tsunami","Volcanic Ash","Waterspout",
              "Wildfire","Winter Wheather","Winter Storm","Waterspout","Wildfire","Flood",
              "Thunderstorm","Cold Wind Chill","Cold Wind Chill","Extreme Cold Wind Chill",
              "Extreme Cold Wind Chill","Extreme Cold Wind Chill","Extreme Cold Wind Chill",
              "Extreme Cold Wind Chill","Tornado")


sort(unique(varnames))
k <- df.test[!(df.test$EVTYPE %in% varnames),]
sort(unique(k$EVTYPE))
unique(df.test$PROPDMGEXP)
unique(df.test$PROPDMGEXP)

df.test[df.test$PROPDMGEXP == "5",]
unique(grep("THUNDERSTORM",df.rawdatared$EVTYPE, ignore.case = TRUE,value = TRUE))
unique(grep("THUNDERSTORM",df.test$EVTYPE, ignore.case = TRUE,value = TRUE))

#  

unique(df.rawdata$PROPDMGEXP)

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

#### Converting the Property and Crop damages values

unique(df.test$CROPDMGEXP)
unique(df.test$PROPDMGEXP)


head(df.test[df.test$CROPDMG != 0,])

length(sort(unique(c(unique(df.test$CROPDMGEXP) ,unique(df.test$PROPDMGEXP)))))

# B = 1000000000
# M,m = 1000000
# K,k = 1000
# H,h = 100
# "", "+", "-", "0", "2", "3", "4", "5", "6","7", "?" = 1

expvalues <- c(1000000000,
               rep(1000000,2),
               rep(1000,2),
               rep(100,2),
               rep(1,11))

names(expvalues) <- c("B","M","m","K","k","H","h",
                      "", "+","-","0","2","3","4","5","6","7","?")

df.test$PROPDMGEXP[1:5]
for(i in c(1:5)){
    print(expvalues[[df.test$PROPDMGEXP[i]]])    
}


convert.exp <- function(x)
{
    if(x == "B")
        return(1000000000)
    else if(x == "M" | x == "m")
        return(1000000)
    else if(x == "K" | x == "k")
        return(1000)
    else if(x == "H" | x == "h")
        return(100)
    return(1)
}

df.test$PROPDMGCONV <- sapply(df.test$PROPDMGEXP, convert.exp) *
    df.test$PROPDMG

df.test$CROPDMGCONV <- sapply(df.test$CROPDMGEXP, convert.exp) *
    df.test$CROPDMG 

convert.exp("m")
##########
df.test[df.test$STATE == "AL" & 
            df.test$EVTYPE =="High Surf" &
            df.test$BGN_DATE == "1995-10-04",]$PROPDMG * 1000
