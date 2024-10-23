library(tidyverse)          #ggplot
library(sf)                 #st_read()
library(ggnewscale)         #new_scale_color() and new_scale_fill()
library(colorspace)
library(ggtext)
library(grid)
library(ggspatial)


setwd("~/GitHub/DSCI605/Mod12")

##All states in USA, places, and watersheads
state <- st_read("data/tl_2019_us_state/tl_2019_us_state.shp")
place <- st_read("data/tl_2016_18_place/tl_2016_18_place.shp")
INWS <- st_read("data/Watersheds_HUC08_2009/WATERSHEDS_HUC08_2009_USDA_IN.shp")

#See all data frames
View(state)
View(place)
View(INWS)

#Get Indiana for state
IN <- state %>% filter(STUSPS == "IN")

#Get Carmel and Muncie from place
city1 <- place %>% filter(NAME == "Carmel")
city2 <- place %>% filter(NAME == "Muncie")

col = colorspace::diverge_hcl(5)


# Assuming IN is an sf object
bbox_in <- st_bbox(IN)

# Extract the bounding box values
min_x <- bbox_in["xmin"]
max_x <- bbox_in["xmax"]
min_y <- bbox_in["ymin"]
max_y <- bbox_in["ymax"]

# Add the 'City' column to each dataset using mutate
city1 <- city1 %>% mutate(City = "Carmel")
city2 <- city2 %>% mutate(City = "Muncie")


# Combine the two city datasets into one using bind_rows
cities_combined <- bind_rows(city1, city2)



# Plot IN and INWS using the bounding box to set axis limits
ggplot() +
  geom_sf(data = IN, fill = 'yellow', alpha = 0.5) +         # Plot IN
  geom_sf(data = INWS, fill = 'blue', alpha = 0.5) +        # Plot INWS
  geom_sf(data = city1, fill="orange", alpha=0.5) +
  geom_sf(data = city2, fill="green", alpha=0.5) +
  scale_x_continuous(limits = c(min_x, max_x)) +           # Set x-axis limits
  scale_y_continuous(limits = c(min_y, max_y)) +           # Set y-axis limits
  theme_minimal() +
  labs(title = "Plot of IN and INWS with Scaled Axes")


# Plot basic IN, Watershed, and places
ggplot() +
  geom_sf(data = IN, color = 'black', alpha = 0) +         # Plot IN
  geom_sf(data = INWS, color = 'black', alpha = 0) +        # Plot INWS
  geom_sf(data = place, color="black", alpha=0) +
  scale_x_continuous(limits = c(min_x, max_x)) +           # Set x-axis limits
  scale_y_continuous(limits = c(min_y, max_y)) +           # Set y-axis limits
  xlab("Longitude") + ylab("Latitude") +
  theme_minimal() +
  labs(title = "Indiana map\n(678 places)")


# Plot basic IN, Watershed, Carmel, Muncie
ggplot() +
  # Plot Indiana boundary with color for the shared legend
  geom_sf(data = IN, aes(color = "Indiana"), alpha = 0.5, size = 1.5, lwd = 1.5) +
  
  # Plot Watershed boundary, using the same color scale as Indiana
  geom_sf(data = INWS, aes(color = "Watershed"), size = 1.5, alpha = 0.5) +
  
  # Manual scale for both IN and INWS colors (combined into a single legend without a title)
  scale_color_manual(name = NULL,  # No title for this legend
                     values = c("Indiana" = "red", "Watershed" = "black")) +
  
  # Plot the watershed regions with fill colors mapped to REGION
  new_scale_fill() +
  geom_sf(data = INWS, aes(fill = REGION), color = 'black', alpha = 0.5) +
  scale_fill_manual(values = colorspace::diverge_hcl(5), name = "Watershed Regions") +  # Title for Watershed Regions
  
  # Plot the cities with a different aesthetic (fill_city)
  new_scale_fill() +
  geom_sf(data = cities_combined, aes(fill = City), alpha = 0.5) +
  scale_fill_manual(values = c("Carmel" = "red", "Muncie" = "green"), name = "Cities", guide = "legend") +
  
  # Add manual scale for Indiana boundary
  #scale_color_manual(name = NULL, values = c("Indiana" = "red")) +
  
  # Use guides to ensure both legends are displayed
  guides(
    color = guide_legend(title = NULL),  # No title for Boundary legend
    fill = guide_legend(override.aes = list(alpha = 0.5))  # No title for Watershed Regions legend
  ) +
  
  # Set axis limits and labels
  scale_x_continuous(limits = c(min_x, max_x)) +           # Set x-axis limits
  scale_y_continuous(limits = c(min_y, max_y)) +           # Set y-axis limits
  xlab("Longitude") + ylab("Latitude") +
  
  #scale bar
  annotation_scale(location = "bl", width_hint=0.5,
                   unit_category = "metric", dist_unit = "km", dist = 1, 
                   height = unit(0.25, "cm"), text_cex = 0.8, text_pad = unit(0.1, "cm")) +
  #scalebar(data = IN, location = "bottomleft", anchor = c(x=731500, y=47124000), transform=FALSE, st.dist=0.04) +
  #north arrow
  annotation_north_arrow(location= "br", which_north="true",
                         pad_x = unit(0, "in"), pad_y=unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  
  # Theme and title
  theme_minimal() +
  labs(title = "Map of Indiana Watershed and Major Cities", subtitle = "(2 cities selected)") +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black", size = 12, face="bold"),
    plot.subtitle = element_text(hjust = 0, color = "blue", size = 12)
  )
 


#Basic spatial mapping
ggplot() +
  geom_sf(data = IN, alpha=0, aes(color="A"), labels = c("Indiana"), name="") +
  scale_color_manual(values = c("A"="red"), labels = c("Indiana"), name="") +
  
  new_scale_color() +
  #Plot polygon, set color inside aes() for a legend
  geom_sf(data = INWS, alpha=0, aes(color="B"), labels = c("Watershead"), name="") +
  scale_color_manual(values = c("B"="blue"), labels = c("Watershead"), name="") +
  
  new_scale_color() +
  geom_sf(data = city1, alpha=0.5, aes(fill="O"), color="orange") +  # Plot city
  scale_fill_manual(values = c("O" = "orange"), labels = c("Carmel"), name = "") +
  scale_color_manual(values = c("O"="orange"), labels = c("Carmel"), name="") +

  new_scale_color() +
  geom_sf(data = city2, alpha=0.5, aes(fill="P"), color="purple") +
  scale_fill_manual(values = c("P" = "purple"), labels = c("Muncie"), name = "") +
  scale_color_manual(values = c("P"="purple"), labels = c("Muncie"), name="") +
  
  #Set legend position
  theme(legend.position = "bottom") +
  #Set font size for all texts
  theme(text=element_text(size=16)) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Indiana, Watershed, Carmel and Muncie")
  theme_minimal()

#Basic spatial mapping
ggplot() +
  
  geom_sf(data = IN, alpha=0, aes(color="black"), labels = c("Indiana"), name="") +
  geom_sf(data = INWS, alpha=0, aes(color="black"), labels = c("Watershead"), name="") +
  geom_sf(data = place, alpha=0, aes(color="black"), labels = c("Cities"), name="") +
  scale_color_manual(values = col) +
  #scale_color_manual(values = c("B"="black"), labels = c("Indiana"), name="") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Indiana") +
  theme_minimal()
  
