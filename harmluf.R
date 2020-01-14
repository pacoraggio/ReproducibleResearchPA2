#1. Across the United States, which types of events (as indicated in the EVTYPE 
#  variable) are most harmful with respect to population health?

# To address this question I am considering the reported casualties 
# (FATALITIES and INJURIES variables of the dataset). 

rm(list = ls())
load("workingdata.RData")

library(ggplot2)
library(dplyr)

# df.harmf <- df.wdata[df.wdata$FATALITIES != 0 | df.wdata$INJURIES != 0,]

sum(df.wdata$FATALITIES)
sum(df.wdata$INJURIES)
# names(df.harmf)
# ragg <- aggregate(FATALITIES ~ EVTYPE, data = df.harmf, sum)
# class(ragg)
# sort(ragg$FATALITIES, decreasing = TRUE)[1:10]

df.casualties <- df.wdata %>% group_by(EVTYPE) %>%
    summarise(sum_fatalities = sum(FATALITIES), sum_injuries = sum(INJURIES)) %>%
    mutate(total_sum = sum_fatalities + sum_injuries)

head(df.casualties)

df.top10Harm <- as.data.frame(df.casualties[order(df.casualties$sum_fatalities, 
                                                  decreasing = TRUE)[1:10], ])

library(reshape)
df.top10Harmplot <- melt(df.top10Harm[,c("EVTYPE", 
                                         "sum_fatalities",
                                         "sum_injuries",
                                         "total_sum")], id.vars = 1)

windows()
ggplot(df.top10Harmplot, aes(x = reorder(EVTYPE,value), y = value)) +
    geom_bar(aes(fill = variable), stat = "identity", position = "dodge") +
    coord_flip()

ggplot(df.top10Harmplot, aes(reorder(EVTYPE, value), value)) +
    geom_col(aes(fill = variable)) +
    facet_grid(~variable) + 
    coord_flip()

head(top10Harm)

library(lubridate)

## from 1995

t <- df.wdata[df.wdata$YEAR > 1995,]
head(t)

df.casualties <- df.wdata[df.wdata$YEAR > 1995,] %>% group_by(EVTYPE) %>%
    summarise(sum_fatalities = sum(FATALITIES), sum_injuries = sum(INJURIES)) %>%
    mutate(total_sum = sum_fatalities + sum_injuries)

head(df.casualties)

df.top10Harm <- as.data.frame(df.casualties[order(df.casualties$sum_fatalities, 
                                                  decreasing = TRUE)[1:10], ])

df.top10Harm <- as.data.frame(df.casualties[order(df.casualties$total_sum, 
                                                  decreasing = TRUE)[1:10], ])

head(df.top10Harm)
df.top10Harmplot <- melt(df.top10Harm[,c("EVTYPE", 
                                         "sum_fatalities",
                                         "sum_injuries",
                                         "total_sum")], id.vars = 1)

windows()
ggplot(df.top10Harmplot, aes(x = reorder(EVTYPE,value), y = value)) +
    geom_bar(aes(fill = variable), stat = "identity", position = "dodge") +
    coord_flip()

### Temporal analysis - for future works
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
    