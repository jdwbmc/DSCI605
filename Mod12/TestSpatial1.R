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

#### R; plot()
plot(CHM_HARV, main="Canopy Height Model\nNEON Harvard Forest Field Site")

####tmap
library(tmap)

watershed <- st_read("data/watersheds_HUC08_2009/WATERSHEDS_HUC08_2009_USDA_IN.shp")

tm_shape(watershed) +
  tm_polygons(col= "REGION")

tm_shape(CHM_HARV) +
  tm_raster()
