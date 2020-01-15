# 2. Across the United States, which types of events have the greatest economic 
# consequences?

# To address this question I am considering the reported economical damages 
# (CROPDMGCONV and PROPDMGCONV variables of the dataset). 

rm(list = ls())
load("workingdata.RData")

library(ggplot2)
library(dplyr)
library(reshape)
library(lubridate)

length(unique(df.wdata$EVTYPE))

sum(df.wdata$CROPDMGCONV) 
sum(df.wdata$PROPDMGCONV)

round(sum(df.wdata$PROPDMGCONV)/(sum(df.wdata$CROPDMGCONV) +
                               sum(df.wdata$PROPDMGCONV)),3)*100


df.cost <- df.wdata %>% group_by(EVTYPE) %>%
    summarise(cropdmg = sum(CROPDMGCONV), 
              propdmg = sum(PROPDMGCONV)) %>%
    mutate(total_sum = cropdmg + propdmg)

head(df.cost)

df.top10PROPcost <- as.data.frame(df.cost[order(df.cost$propdmg, 
                                            decreasing = TRUE)[1:10], ])

df.top10CROPcost <- as.data.frame(df.cost[order(df.cost$cropdmg, 
                                            decreasing = TRUE)[1:10], ])

df.top10cost <- as.data.frame(df.cost[order(df.cost$total_sum, 
                                                decreasing = TRUE)[1:10], ])

windows()
g1 <- ggplot(df.top10CROPcost, 
             aes(reorder(EVTYPE, -cropdmg), cropdmg)) +
    geom_bar(stat = "identity", position = "dodge", fill = "salmon") +
    labs(title = "Crop Damages",
         x = "Event Type",
         y = "Cost in US $") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

g2 <- ggplot(df.top10PROPcost, 
             aes(reorder(EVTYPE, -propdmg), propdmg)) +
    geom_bar(stat = "identity", position = "dodge", fill = "springgreen3") +
    labs(title = "Property Damages",
         x = "Event Type",
         y = "Cost in US $") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

df.top10costplot <- melt(df.top10cost[,c("EVTYPE", 
                                      "cropdmg",
                                      "propdmg",
                                      "total_sum")], id.vars = 1)

g3 <- ggplot(df.top10costplot, 
             aes(reorder(EVTYPE, -value), value)) +
    geom_bar(aes(fill = variable), stat = "identity", position = "dodge") +
    labs(title = "Property + Crop damages",
         x = "Event Type",
         y = "Cost in US $") +
    scale_fill_discrete(labels = c("Crop", "Property", "Sum")) + 
    theme(strip.text.x = element_blank(),
          strip.background = element_rect(colour="transparent", fill="transparent"),
          legend.background = element_rect(fill="transparent", ),
          legend.position=c(.75,.85),
          legend.key.height = unit(0.2, "cm"),
          legend.key.width = unit(0.2, "cm"),
          axis.text.x = element_text(angle = 90, hjust = 1))

windows()
g3

windows()
gridExtra::grid.arrange(g1, g2, g3, nrow = 1,
                        bottom=textGrob("Figure 2: 10 ten most economical costly events",gp=gpar(fontsize=11)))

top10costPROPplot <- melt(df.top10cost[,c("EVTYPE", "propdmg")], id.vars = 1)
top10costCROPplot <- melt(df.top10cost[,c("EVTYPE", "cropdmg")], id.vars = 1)

top10costplot <- melt(df.top10cost[,c("EVTYPE", 
                                      "cropdmg",
                                      "propdmg",
                                      "total_sum")], id.vars = 1)

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
    geom_col(aes(fill = variable), position = "dodge", stat = "identity") +
    coord_flip()
