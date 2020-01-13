rm(list = ls())
load("workingdata.RData")

library(ggplot2)
library(dplyr)
library(reshape)
library(lubridate)

length(unique(df.wdata$EVTYPE))

sum(df.wdata$CROPDMGCONV) 
sum(df.wdata$PROPDMGCONV)

df.cost <- df.wdata %>% group_by(EVTYPE) %>%
    summarise(cropdmg = sum(CROPDMGCONV), 
              propdmg = sum(PROPDMGCONV)) %>%
    mutate(total_sum = cropdmg + propdmg)

head(df.cost)

df.top10PROPcost <- as.data.frame(df.cost[order(df.cost$propdmg, 
                                            decreasing = TRUE)[1:10], ])

df.top10CROPcost <- as.data.frame(df.cost[order(df.cost$cropdmg, 
                                            decreasing = TRUE)[1:10], ])


top10costPROPplot <- melt(df.top10cost[,c("EVTYPE", "propdmg")], id.vars = 1)
top10costCROPplot <- melt(df.top10cost[,c("EVTYPE", "cropdmg")], id.vars = 1)
top10costplot <- melt(df.top10cost[,c("EVTYPE", "cropdmg","propdmg")], id.vars = 1)

windows()
ggplot(top10costPROPplot, aes(reorder(EVTYPE, value), value)) +
    geom_col() +
    coord_flip()

windows()
ggplot(top10costCROPplot, aes(reorder(EVTYPE, value), value)) +
    geom_col() +
    coord_flip()

windows()
ggplot(top10costplot, aes(reorder(EVTYPE, value), value)) +
    geom_col(aes(fill = variable)) +
    coord_flip()
