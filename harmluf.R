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


###################
## PLOTS

library("ggpubr")

df.casualties <- df.wdata[df.wdata$YEAR > 1995,] %>% 
    group_by(EVTYPE) %>%
    summarise(sum_fatalities = sum(FATALITIES), 
              sum_injuries = sum(INJURIES)) %>%
    mutate(total_sum = sum_fatalities + sum_injuries)

df.top10sumfatalities <- 
    as.data.frame(df.casualties[order(df.casualties$sum_fatalities, 
                                      decreasing = TRUE)[1:10], ])

df.top10suminjuries <- 
    as.data.frame(df.casualties[order(df.casualties$sum_injuries, 
                                      decreasing = TRUE)[1:10], ])

df.top10total <- 
    as.data.frame(df.casualties[order(df.casualties$total_sum, 
                                      decreasing = TRUE)[1:10], ])


resume <- data.frame('Event Type' = df.top10sumfatalities$EVTYPE,
                     'Fatalities' = df.top10sumfatalities$sum_fatalities,
                     'Event Type' = df.top10suminjuries$EVTYPE,
                     'Injuries' = df.top10suminjuries$sum_injuries,
                     'Event Type' = df.top10total$EVTYPE,
                     'Total' = df.top10total$total_sum)

g1 <- ggplot(df.top10sumfatalities, 
             aes(reorder(EVTYPE, -sum_fatalities), sum_fatalities)) +
    geom_bar(stat = "identity", position = "dodge", fill = "salmon") +
    labs(title = "Sum Fatalities",
        x = "Event Type",
         y = "Number of Casualties") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
    #coord_flip()

# g1
g2 <- ggplot(df.top10suminjuries, 
             aes(reorder(EVTYPE, -sum_injuries), sum_injuries)) +
    geom_bar(stat = "identity", position = "dodge", fill = "springgreen4") +
    # coord_flip() +
    labs(title = "Sum Injuries",
        x = "Event Type",
         y = "Number of Casualties") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

library(grid)
textG
# windows()
# g2

df.top10totalplot <- melt(df.top10total[,c("EVTYPE", 
                                           "sum_fatalities",
                                           "sum_injuries",
                                           "total_sum")], id.vars = 1)

g3 <- ggplot(df.top10totalplot, 
             aes(reorder(EVTYPE, -value), value)) +
    geom_bar(aes(fill = variable), stat = "identity", position = "dodge") +
    labs(title = "aggregate",
         x = "Event Type",
         y = "Number of Casualties") +
    scale_fill_discrete(name = "", labels = c("Fatalities", "Injuries", "Sum")) + 
    # coord_flip() +
    theme(strip.text.x = element_blank(),
          strip.background = element_rect(colour="white", fill="white"),
          legend.position=c(.7,.85),
          axis.text.x = element_text(angle = 90, hjust = 1))
    
windows()
g3
# ggarrange(g1, g2, g3, nrow = 2)
windows()
gridExtra::grid.arrange(g1, g2, g3, nrow =1)

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

df.wdata <- df.wdata[df.wdata$YEAR > 1995,]
head(df.wdata$YEAR)


df.casualties <- df.wdata[df.wdata$YEAR > 1995,] %>% 
    group_by(EVTYPE) %>%
    summarise(sum_fatalities = sum(FATALITIES), 
              sum_injuries = sum(INJURIES)) %>%
    mutate(total_sum = sum_fatalities + sum_injuries)

head(df.casualties)

df.top10sumfatalities <- 
    as.data.frame(df.casualties[order(df.casualties$sum_fatalities, 
                                                  decreasing = TRUE)[1:10], ])

df.top10suminjuries <- 
    as.data.frame(df.casualties[order(df.casualties$sum_injuries, 
                                                  decreasing = TRUE)[1:10], ])

df.top10total <- 
    as.data.frame(df.casualties[order(df.casualties$total_sum, 
                                      decreasing = TRUE)[1:10], ])


resume <- data.frame('Event Type' = df.top10sumfatalities$EVTYPE,
                     'Fatalities' = df.top10sumfatalities$sum_fatalities,
                     'Event Type' = df.top10suminjuries$EVTYPE,
                     'Injuries' = df.top10suminjuries$sum_injuries,
                     'Event Type' = df.top10total$EVTYPE,
                     'Total' = df.top10total$total_sum)

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
    