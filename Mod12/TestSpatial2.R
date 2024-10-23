library(tidyverse)
library(raster)             #raster
library(sf)                 #st_read()
library(ggspatial)          #annotation_scale, annotation_north_arrow
library(ggnewscale)         #new_scale_color()
library(ggsn)
library(viridis)

setwd("~/GitHub/DSCI605/Mod12")

#Read files
point_df <- read_csv("data/HARV_PlotLocations.csv")
polygon_sf <- st_read("data/HarClip_UTMZ18.shp")
CHM_HARV <- raster("data/HARV_chmCrop.tif")
CHM_HARV_df <- as.data.frame(CHM_HARV, xy=TRUE)

#Basic spatial mapping
ggplot(point_df) +
  geom_raster(data = CHM_HARV_df, aes(x=x, y=y, fill=HARV_chmCrop)) +
  geom_point(aes(easting, northing, color="red"), size=2) +
  geom_sf(data = polygon_sf) +
  ggtitle("NEON Harvard Forest Field Site Canopy Height Model") +
  theme_bw()

#Polish the map
ggplot(point_df) +
  
  #Plot the raster image
  geom_raster(data = CHM_HARV_df, aes(x=x, y=y, fill=HARV_chmCrop)) +
  #Change the title of legend
  guides(fill=guide_legend("Canopy Height")) +
  #Choose suitable color system
  scale_fill_viridis(option = "D") +
  
  #Plot points, set color inside aes() for a legend
  geom_point(aes(easting, northing, color="A"), size=3) +
  scale_color_manual(values = c("A"="red"), labels = c("Samples"), name="") +
  #Use a new scale color for polygon because there is already one for points
  
  new_scale_color() +
  #Plot polygon, set color inside aes() for a legend
  geom_sf(data = polygon_sf, alpha=0, aes(color="B"), labels = c("Area of Interest"), name="") +
  scale_color_manual(values = c("B"="blue"), labels = c("Area of Interest"), name="") +
  
  #scale bar
  #annotation_scale(location = "bl", width_hint=0.5) +
  scalebar(data = point_df, location = "bottomleft", anchor = c(x=731500, y=47124000), dist=400, dist_unit = "m", transform=FALSE, st.dist=0.04) +
  #north arrow
  annotation_north_arrow(location= "bl", which_north="true",
                         pad_x = unit(0, "in"), pad_y=unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  #Set legend position
  theme(legend.position = "bottom") +
  #Set font size for all texts
  theme(text=element_text(size=20)) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("NEON Harvard Forest Field Site Canopy Height Model")

ggsave("map_NEON.png", width=12, height=10, dpi=300, unit='in')
