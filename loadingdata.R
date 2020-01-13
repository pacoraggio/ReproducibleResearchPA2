## loading data tested script

### Loading raw data

rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)

names <- read.csv("./data/repdata_data_StormData.csv/repdata_data_StormData.csv", 
                  nrows = 10, header = TRUE)
names(names)

df.rawdata <- read.csv("./data/repdata_data_StormData.csv.bz2",
                       colClasses = c("NULL", 
                                      "character", 
                                      rep("NULL",4), 
                                      rep("character",2),
                                      rep("NULL",14),
                                      rep("numeric",3),
                                      "character", 
                                      "numeric", 
                                      "character", 
                                      rep("NULL",9)))

### df.wdata is the working data.frame consisting of raws containing at least
### one casualties or one economical damage (crop or property)

df.wdata <- df.rawdata[!(df.rawdata$FATALITIES == 0 &
                             df.rawdata$INJURIES == 0 &
                             df.rawdata$PROPDMG == 0 &
                             df.rawdata$CROPDMG == 0),]

## convert Begin Date to Date format
df.wdata$YEAR <- year(as.Date(sub(" .*", 
                                  "", 
                                  df.wdata$BGN_DATE), 
                              format("%m/%d/%Y")))

head(df.wdata)

dim(df.rawdata)
dim(df.wdata)

print(object.size(df.rawdata), units = "auto")
print(object.size(df.wdata), units = "auto")

## Converting the Property and Crop damages values

unique(df.wdata$CROPDMGEXP)
unique(df.wdata$PROPDMGEXP)

head(df.wdata[df.wdata$CROPDMG != 0,])

length(sort(unique(c(unique(df.wdata$CROPDMGEXP) ,unique(df.wdata$PROPDMGEXP)))))

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

df.wdata$PROPDMGCONV <- sapply(df.wdata$PROPDMGEXP, convert.exp) *
    df.wdata$PROPDMG

df.wdata$CROPDMGCONV <- sapply(df.wdata$CROPDMGEXP, convert.exp) *
    df.wdata$CROPDMG 

save(df.wdata, file = "workingdata.RData")

## Grouping Event Type

rm(list = ls())
load("workingdata.RData")

bk <- df.wdata

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
              "Wildfire","Winter Weather","Winter Storm","Waterspout","Wildfire","Flood",
              "Thunderstorm","Cold Wind Chill","Cold Wind Chill","Extreme Cold Wind Chill",
              "Extreme Cold Wind Chill","Extreme Cold Wind Chill","Extreme Cold Wind Chill",
              "Extreme Cold Wind Chill","Tornado")

# unique(grep("THUNDERSTORM",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("Mar",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("Thunderstorm w",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("Tstm w|tstmw",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("Tstmw",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("Thunderstorm",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("Thun",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("Astronomi",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("Slid",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("Aval",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("Bliz",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("Fl",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("Chil",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("Fog",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("Smoke",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("Devil",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("Heat",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("Drou",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("light",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("lightn",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("rain",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("snow",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("hur",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("ice",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("lake",bk$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("lake",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))

# unique(grep("curr",bk$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("curr",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))
# 
# unique(grep("seich",bk$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("sei",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))
# 
# unique(grep("sle",bk$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("sle",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))
# 
# unique(grep("surg",bk$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("surg",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))
# 
# unique(grep("wind",bk$EVTYPE, ignore.case = TRUE,value = TRUE))
# unique(grep("wind",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))

unique(grep("Free",df.wdata$EVTYPE, ignore.case = TRUE,value = TRUE))


# different Marine
df.wdata[grep("Marine hail",df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Marine Hail"
df.wdata[grep("Marine High Wind",df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Marine High Wind"
df.wdata[grep("Marine Strong Wind",df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Marine Strong Wind"
df.wdata[grep("Marine Tstm|Marine Thunde",df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Marine Thunderstorm Wind"
# different lightning
df.wdata[grep("Lightn|lighting|LIGNTNING|Lightning",df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Lightning"
# Thunderstorm wind is a category
df.wdata[grep("Thunderstorm  winds|Tstm w|tstmw|Thunderstormwinds|Thunderstorms w|Thunderstormw|Thunderstom winds|Tunderstom winds",df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Thunderstorm Wind"
df.wdata[grep("THUND|THUNE",df.wdata$EVTYPE, ignore.case = FALSE),]$EVTYPE <- "Thunderstorm"

# Astronomical Low tide
df.wdata[grep("Astronomical L",df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Astronomical Low Tide"
# Slides are considered Avalanches
df.wdata[grep("Avalanc|Slid",df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Avalanche"
# Blizzards
df.wdata[grep("Bliz",df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Blizzard"
# Flood are all grouped - no difference between Costal, Flash etdc 
df.wdata[grep("Flood|Fld",df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Flood"
# Cold Wind Chill contains also Extreme and Excessive
df.wdata[grep("Chill",df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Cold Wind Chill"
# Dense Fog and Freezing fog
df.wdata[grep("Freezing Fog",df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Freezing Fog"
df.wdata[grep("FOG",df.wdata$EVTYPE, ignore.case = FALSE),]$EVTYPE <- "Dense Fog"
# Dense Smoke
df.wdata[grep("Dense Smoke",df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Dense Smoke"
# Drought
df.wdata[grep("Droug",df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Drought"
# Dust Devil
df.wdata[grep("Devil",df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Dust Devil"
# Dust Storm
df.wdata[grep("DUST",df.wdata$EVTYPE, ignore.case = FALSE),]$EVTYPE <- "Dust Storm"
# Excessive Heat contains all references to Heat
df.wdata[grep("Heat",df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Heat"
# Frost Freeze
df.wdata[grep("Frost",df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Frost"
# Funnel Cloud
df.wdata[grep("Funnel",df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Funnel Cloud"
# Heavy rain
df.wdata[grep("Heavy Rain|hvy rain|Rainfall|Rainstorm",
              df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Heavy Rain"
# Snow contains all snow related ones (also light)
df.wdata[grep("snow",
              df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Snow"
# Surf contains all surf related entries
df.wdata[grep("Surf",
              df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Surf"
# Hurricane Typhoon
df.wdata[grep("Hurr|Typh",
              df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Hurricane Typhoon"
# Sleet
df.wdata[grep("Sleet",
              df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Sleet"
# Ice contains Ice storm and all Ice related events
df.wdata[grep("Ice",
              df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Ice"
# Rip current
df.wdata[grep("Curre",
              df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Rip Current"
# Rip current
df.wdata[grep("Seiche",
              df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Seiche"
# Storm Surge
df.wdata[grep("Storm Surge",
              df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Storm Surge"
# Strong Wind consists of diffetent entries - leaving out Thunderstorm winds etc
df.wdata[grep("WIND",
              df.wdata$EVTYPE, ignore.case = FALSE),]$EVTYPE <- "Strong Wind"
df.wdata[grep("Strong Wind|Strong Winds|Whirlwind|Gusty Winds|Gusty wind|Wind Damage|Gradient wind|gradient wind",
              df.wdata$EVTYPE, ignore.case = FALSE),]$EVTYPE <- "Strong Wind"
# Tornado
df.wdata[grep("Torn",
              df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Tornado"
# Tropical Depression
df.wdata[grep("Tropical dep",
              df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Tropical Depression"
# Tropical Storm
df.wdata[grep("Tropical sto",
              df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Tropical Storm"
# Tsunami
df.wdata[grep("Tsu",
              df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Tsunami"
# Volcanic Ash
df.wdata[grep("Volcanic",
              df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Volcanic Ash"
# Waterspout
df.wdata[grep("Waterspou",
              df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Waterspout"
# Wildfires all fire related accidents
df.wdata[grep("fire",
              df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Wildfire"
# Winterstorm contains also Coastal Storm, Hail Storm and (some) Rainstorm
df.wdata[grep("STORM",
              df.wdata$EVTYPE, ignore.case = FALSE),]$EVTYPE <- "Winterstorm"
# Winterstorm contains also Coastal Storm, Hail Storm and (some) Rainstorm
df.wdata[grep("WINTER",
              df.wdata$EVTYPE, ignore.case = FALSE),]$EVTYPE <- "Winter Weather"
# Hail
df.wdata[grep("HAIL",df.wdata$EVTYPE, ignore.case = FALSE),]$EVTYPE <- "Funnel Cloud"
# Remaining Cold and Hypothermia condition goes on Frost Freeze
df.wdata[grep("COLD|HYPO",df.wdata$EVTYPE, ignore.case = FALSE),]$EVTYPE <- "Frost Freeze"
# Remaining Freeze condition goes on Frost Freeze
df.wdata[grep("Freeze|Frost",df.wdata$EVTYPE, ignore.case = TRUE),]$EVTYPE <- "Frost Freeze"
df.wdata[grep("Frost Freeze|FREEZING RAIN|FREEZING DRIZZLE|Freezing Spray|Freezing Drizzle|Freezing Rain|Freezing drizzle|LIGHT FREEZING RAIN",df.wdata$EVTYPE, ignore.case = FALSE),]$EVTYPE <- "Frost Freeze"

varnames <- c("Funnel Cloud","Astronomical Low Tide","Avalanche","Blizzard","Coastal Flood",
              "Cold Wind Chill","Extreme Cold Wind Chill","Dense Fog","Dense Smoke","Drought",
              "Dust Devil","Dust Storm","Excessive Heat","Excessive Heat","Excessive Cold Wind Chill",
              "Flash Flood","Frost Freeze","Funnel Cloud","Freezing Fog","Hail","Heat","Heavy Rain",
              "Heavy Rain","Heavy Snow","High Surf","High Surf","High Surf","Hurricane Typhoon",
              "Hurricane Typhoon","Ice storm","Lakeshore Flood","Lake effect Snow","Lake effect Snow",
              "Lightning","Lighting","Marine Hail","Marine Strong Wind","Marine Thunderstorm Wind",
              "Marine Thunderstorm Wind","Marine High Wind","Rip Current","Seiche","Sleet",
              "Storm Surge","Strong Wind","Thunderstorm Wind","Thunderstorm Wind","Tornado",
              "Tropical Depression","Tropical Storm","Tsunami","Volcanic Ash","Waterspout",
              "Wildfire","Winter Weather","Winter Storm","Waterspout","Wildfire","Flood",
              "Thunderstorm","Cold Wind Chill","Cold Wind Chill","Extreme Cold Wind Chill",
              "Extreme Cold Wind Chill","Extreme Cold Wind Chill","Extreme Cold Wind Chill",
              "Extreme Cold Wind Chill","Tornado")

varnames <- sort(unique(varnames))
uniqueET <- sort(unique(df.wdata$EVTYPE))

uniqueET[!(uniqueET %in% varnames)]

save(df.wdata, file = "workingdata.RData")
