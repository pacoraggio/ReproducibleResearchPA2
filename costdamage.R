library(ggplot2)
library(dplyr)
library(reshape)
library(lubridate)

df.test <- df.rawdata
length(unique(df.test$EVTYPE))


df.costdam <- df.test[df.test$PROPDMGCONV != 0 | df.test$CROPDMGCONV != 0,] 
sum(df.costdam$CROPDMGCONV) 
sum(df.costdam$PROPDMGCONV)

sum(df.test$CROPDMGCONV)
sum(df.test$PROPDMGCONV)

sum(df.costdam$CROPDMGCONV) == sum(df.test$CROPDMGCONV) 
sum(df.costdam$PROPDMGCONV) == sum(df.test$PROPDMGCONV)

head(df.costdam)

df.costplot <- df.costdam %>% group_by(EVTYPE) %>%
    summarise(cropdmg = sum(CROPDMGCONV), 
              propdmg = sum(PROPDMGCONV)) %>%
    mutate(total_sum = cropdmg + propdmg)


top10cost <- as.data.frame(df.costplot[order(df.costplot$total_sum, decreasing = TRUE)[1:10], ])
top10cost

top10costplot <- melt(top10cost[,c("EVTYPE", "cropdmg","propdmg")], id.vars = 1)

library(ggplot2)

windows()
ggplot(top10costplot, aes(reorder(EVTYPE, value), value)) +
    geom_col(aes(fill = variable)) +
    coord_flip()
