library(tidyverse)          #ggplot
library(sf)                 #st_read()

setwd("~/GitHub/DSCI605/Mod12")

##All states in USA
state <- st_read("data/tl_2019_us_state/tl_2019_us_state.shp")

IN = state[state$STUSPS == "IN",]
IN1 <- state %>% filter(STUSPS == "IN")

ggplot() +
  geom_sf(data=IN) 