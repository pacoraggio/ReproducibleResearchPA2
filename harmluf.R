#1. Across the United States, which types of events (as indicated in the EVTYPE 
#  variable) are most harmful with respect to population health?
library(ggplot2)
library(dplyr)
    
df.harmf <- df.test[df.test$FATALITIES != 0 | df.test$INJURIES != 0,]
names(df.harmf)

sum(df.harmf$FATALITIES)
sum(df.harmf$INJURIES)
names(df.harmf)

ragg <- aggregate(FATALITIES ~ EVTYPE, data = df.harmf, sum)
class(ragg)
sort(r$FATALITIES, decreasing = TRUE)[1:10]

rgb <- df.harmf %>% group_by(EVTYPE) %>%
    summarise(sum_fatalities = sum(FATALITIES), sum_injuries = sum(INJURIES)) %>%
    mutate(total_sum = sum_fatalities + sum_injuries)

head(rgb)

top10Harm <- as.data.frame(rgb[order(rgb$total_sum, decreasing = TRUE)[1:10], ])
ggplot(top10Harm, aes(EVTYPE, total_sum)) +
    geom_bar(stat = "identity", aes(fill = sum_injuries)) +
    coord_flip()

rgb
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

#library(dplyr)
#df2 <- df %>%
#    group_by(dose) %>%
#    arrange(dose, desc(supp)) %>%
#    mutate(lab_ypos = cumsum(len) - 0.5 * len) 
#df2