#1. Across the United States, which types of events (as indicated in the EVTYPE 
#  variable) are most harmful with respect to population health?
rm(list = ls())
load("workingdata.RData")

library(ggplot2)
library(dplyr)


df.harmf <- df.wdata[df.wdata$FATALITIES != 0 | df.wdata$INJURIES != 0,]

sum(df.harmf$FATALITIES)
sum(df.harmf$INJURIES)
names(df.harmf)

ragg <- aggregate(FATALITIES ~ EVTYPE, data = df.harmf, sum)
class(ragg)

sort(ragg$FATALITIES, decreasing = TRUE)[1:10]

rgb <- df.harmf %>% group_by(EVTYPE) %>%
    summarise(sum_fatalities = sum(FATALITIES), sum_injuries = sum(INJURIES)) %>%
    mutate(total_sum = sum_fatalities + sum_injuries)


top10Harm <- as.data.frame(rgb[order(rgb$total_sum, decreasing = TRUE)[1:10], ])

library(reshape)
rs <- melt(top10Harm[,c("EVTYPE", "sum_fatalities","sum_injuries")], id.vars = 1)

head(rs)
windows()
ggplot(rs, aes(x = reorder(EVTYPE,value), y = value)) +
    geom_bar(aes(fill = variable), stat = "identity", position = "dodge") +
    coord_flip()


top10Harm

library(ggplot2)
windows()
ggplot(rs, aes(reorder(EVTYPE, value), value)) +
    geom_col(aes(fill = variable)) +
    coord_flip()

head(top10Harm)

library(lubridate)
head(df.harmf)

kY <- df.harmf %>% 
    group_by(YEAR) %>%
    summarise(sf = sum(FATALITIES), 
              si = sum(INJURIES), 
              st = sum(FATALITIES) + sum(INJURIES))

kYE <- df.harmf %>% 
    group_by(YEAR, EVTYPE) %>%
    summarise(sf = sum(FATALITIES), 
              si = sum(INJURIES), 
              st = sum(FATALITIES) + sum(INJURIES))

head(kYE)
#kYE_tornado <- as.data.frame(kYE[kYE$EVTYPE == "Tornado",])
kYE_tornado <- as.data.frame(kYE[kYE$EVTYPE == "TORNADO",])

kYE_tornado_plot <- melt(kYE_tornado[,c("YEAR", "sf", "si", "st")], id.vars = 1)

head(kYE_tornado_plot)

windows()
ggplot(kYE_tornado_plot, aes(YEAR,value, color = variable)) +
    geom_line()
    