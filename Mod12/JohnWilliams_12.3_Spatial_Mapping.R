library(tidyverse)          #ggplot
library(sf)                 #st_read()
library(ggnewscale)         #new_scale_color() and new_scale_fill()
library(colorspace)         #mapping between assorted color spaces
library(ggspatial)          #easier mapping when input data are in the form of spatial objects
library(grid)               #primitive graphical functions that underlie the ggplot2
library(ggsn)               #improves the GIS capabilities of R

# Set working directory
setwd("~/Module12")

# Read all states in USA, Indiana places, and Indiana watersheads
state <- st_read("tl_2019_us_state/tl_2019_us_state.shp")
place <- st_read("tl_2016_18_place/tl_2016_18_place.shp")
INWS <- st_read("Watersheds_HUC08_2009/WATERSHEDS_HUC08_2009_USDA_IN.shp")

#See all data frames
#View(state)
#View(place)
#View(INWS)

# Get Indiana for state
IN <- state %>% filter(STUSPS == "IN")

# Select cities of interest
somecities <- place %>% filter(NAME=="Muncie" | NAME =="Carmel")
# Add centroid coordinates for the selected cities
cnt_points <- st_centroid(somecities)
cnt_points <- cbind(st_drop_geometry(cnt_points), st_coordinates(st_centroid(somecities$geometry)))

#Set col for watershed fill
col = colorspace::diverge_hcl(20)

# Plot basic IN, Watershed, and places
ggplot() +
  geom_sf(data = IN, color = 'black', alpha = 0) +         # Plot IN
  geom_sf(data = INWS, color = 'black', alpha = 0) +        # Plot INWS
  geom_sf(data = place, color="black", alpha=0) +           # Plot IN cities
  scale_x_continuous(limits = c(-89, -83)) +           # Set x-axis limits
  scale_y_continuous(limits = c(37, 43)) +           # Set y-axis limits
  xlab("Longitude") + ylab("Latitude") +                    # Add labels for x and y axis
  theme_minimal() +                                         # Set theme
  labs(title = "Indiana map\n(678 places)")                 # Set plot title

# Plot professional map with Indiana, Watershed, Carmel and Muncie
ggplot(data=IN) + ## Why need data here because geom_text inherits the global mapping
  
  # watershed with legend
  geom_sf(data=INWS, aes(fill=REGION))+
  scale_fill_manual(values = col)+
  guides(fill=guide_legend(title="Watershed Regions"))+
  
  # watershed+IN: legend - no title
  new_scale_fill()+
  geom_sf(data=IN, linewidth=1, alpha=0, aes(color="A"), show.legend = "polygon") +
  geom_sf(data=INWS, alpha=0, aes(color="B"), show.legend = "polygon") +
  scale_color_manual(values = c("A"="red", "B"="black"),
                     labels = c("Indiana", "Watershed"),
                                name="") +
  #Cities
  new_scale_color() +
  geom_sf(data=somecities, aes(fill = as.factor(NAME))) +
  ## Add fill for selected cities
  scale_fill_manual(values = c("red", "green")) +
  guides(fill=guide_legend(title="Cities"))+
  ## Add city labels
  geom_text(data= cnt_points, aes(x=X, y=Y+0.15, label=NAME),
            color = "black", fontface = "bold", check_overlap = FALSE) +
  
  # Other map items
  ## Add scalebar
  scalebar(data=IN, location = "bottomleft", anchor = c(x=-88.5, y=37), dist = 150,
           dist_unit="km", transform=TRUE, model="WGS84", st.dist=0.04)+
  ## Add North Arrow
  annotation_north_arrow(location="br", which_north="true",
                         # pad_x = unit(2.5, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  ## Add titles
  theme(legend.position="right",       #change 'top' to specific position by c(2.5,0.8)
        plot.title = element_text(hjust = 0.5,
                                  color = "Gray40",
                                  size = 16,
                                  face = "bold"),
        plot.subtitle = element_text(color="blue"),
        plot.caption = element_text(color="Gray60")) +
  coord_sf(xlim = c(-89, -83), ylim = c(37, 43))+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Map of Indiana Watershed and Major Cities",
          subtitle = paste0("(", length(unique(somecities$NAME)),
                            " cities selected)"))

# Note the warning of the inaccurate scale bar: since the map us unprojected data in longitude/latitude
# regions or projected data will often allow for more accurate scale bars.

ggsave("map.pdf", width=6, height=10, dpi=300, unit="in")
ggsave("map_web.png", width=6, height=8, dpi=300, unit="in")