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

df.wdata$PROPDMGEXP[12500:12525]
df.wdata$PROPDMG[12500:12525]
df.wdata$PROPDMGCONV[12500:12525]