library(tidyverse)          #ggplot
library(sf)                 #st_read()
library(ggnewscale)         #new_scale_color() and new_scale_fill()

setwd("~/GitHub/DSCI605/Mod12")

##All states in USA
state <- st_read("data/tl_2019_us_state/tl_2019_us_state.shp")

#Select Indiana (IN) and three other states
states3 <- state %>% filter(STUSPS == 'KY' | state$STUSPS == "OH" | state$STUSPS == "IL", )

IN <- state %>% filter(STUSPS == "IN")
MI <- state %>% filter(STUSPS == "MI")

col = colorspace::diverge_hcl(20)

ggplot() +
  #three states
  geom_sf(data=states3, aes(fill=STUSPS)) +
  scale_fill_manual(values = col) +
  guides(fill=guide_legend(title="Three States")) +
  
  #IN
  new_scale_color() +
  geom_sf(data=IN, linewidth=1, alpha=0, aes(color="A"), show.legend = "polygon") +
  scale_color_manual(values = c("A" = "red"),
                     labels = c("Indiana"),
                     name = "") +
  
  #MI
  new_scale_fill() +
  geom_sf(data=MI, aes(fill=STUSPS)) +
  scale_fill_manual(values = c("green")) +
  guides(fill=guide_legend(title="Michigan"))



ggplot() +
  #three states
  geom_sf(data=states3, aes(fill=STUSPS)) +
  scale_fill_manual(values = col) +
  guides(fill=guide_legend(title="Three States")) +
  
  #IN
  new_scale_color() +
  geom_sf(data=IN, linewidth=1, alpha=0, aes(color="A"), show.legend = "polygon") +
  geom_sf(data=MI, alpha=0, aes(color="B"), show.legend = "polygon") +
  scale_color_manual(values = c("A" = "red", "B" = "green"),
                     labels = c("Indiana", "Michigan"),
                     name = "") +
  
  #MI
  new_scale_fill() +
  geom_sf(data=MI, aes(fill=STUSPS)) +
  scale_fill_manual(values = c("green")) +
  guides(fill=guide_legend(title="Michigan"))
