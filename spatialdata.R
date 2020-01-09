library(tidyverse)
library(urbnmapr)

windows()
ggplot() +
    geom_polygon(data = urbnmapr::states,
                 mapping = aes(x = long, y = lat, group = group),
                 fill = "grey", color = "white") +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45)


