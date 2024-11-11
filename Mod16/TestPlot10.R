library(tidyverse)
library(raster)          #raster()
library(sf)              #st_read()
library(ggspatial)       #annotation_scale,annotation_north_arrow
library(ggnewscale)      #new_scale_color() 
#library(ggsn)            #scalebar()
library(shiny)           #Shiny app
library(plotly)          #plot_ly()
library(gridExtra)       #grid.arrange()

setwd("~/GitHub/DSCI605/Mod15")

##################Read csv and shape file into R
# learners will have this data loaded from an earlier episode, st_read needs sf packages
Unemployrate <-read_csv("data/unemployment_county.csv")
Crimerate<-read_csv("data/crime_and_incarceration_by_state.csv")
States<-st_read("data/tl_2019_us_state/tl_2019_us_state.shp")

######## Focus on Contiguous USA
Contiguous_state <- States%>% filter(STUSPS!="AK"& 
                                       STUSPS!="AS"& 
                                       STUSPS!="MP"& 
                                       STUSPS!="PR"& 
                                       STUSPS!="VI"& 
                                       STUSPS!="HI"& 
                                       STUSPS!="GU")

###Check the length of states.

length(unique(Contiguous_state$STUSPS))

##############Unemployrate 

##Check the length of State
unique(Unemployrate $State)
length(unique(Unemployrate $State))
Unemployrate <-  Unemployrate %>% filter(State!="AK"& State!="HI") %>%
  group_by(State,Year) %>% 
  summarise(Totalforce=sum(`Labor Force`),
            Totalemployed=sum(Employed),
            Totalunemployed=sum(Unemployed),
            Meanrate=mean(`Unemployment Rate`,rm.na=TRUE)
            #Meanrate = (Totalunemployed/Totalforce)*100
  )
##Check the length of State again  
length(unique(Unemployrate $State))
##Change the column name “State” into  "STUSPS" by using rename().Use   filter() to pick some years data from 2007 to 2014.
Unemployrate <- Unemployrate %>% 
  rename("STUSPS"="State") %>% 
  filter(Year %in% c(2007:2014))

##############Crimerate

##Check the length of states
unique(Crimerate$jurisdiction)
length(unique(Crimerate$jurisdiction))
head(Crimerate)
###Change the column name 
Crimerate <-  Crimerate %>% 
  rename("STUSPS"="jurisdiction") %>% 
  rename("Year"="year") %>% 
  filter(STUSPS!="FEDERAL"& STUSPS!="ALASKA"& STUSPS!="HAWAII") %>% 
  filter(Year %in% c(2007:2014))

##Recheck the data
length(unique(Crimerate$STUSPS))
head(Crimerate)

###Changes the state names in the state column "STUSPS"
Crimerate$STUSPS <- state.abb[match(str_to_title(Crimerate$STUSPS),state.name)]
###Calculate the crimerate
Crimerate <- Crimerate %>% 
  mutate(Crimerate=(violent_crime_total/state_population)*100) %>% 
  dplyr::mutate_if(is.numeric, round, 1)

save(list = c("Crimerate", "Unemployrate","Contiguous_state"), file = "CleaneData.Rdata")
##################Read data file into R
load(file = "CleaneData.Rdata")
########Join the rational tables and check the missing values
CS_Erate<-right_join(Contiguous_state, Unemployrate, 
                     by = c("STUSPS"))

CS_Erate_Crate <- right_join(CS_Erate, Crimerate, 
                             by = c("STUSPS","Year"))
CS_Erate_Crate1 <- CS_Erate_Crate %>% 
  dplyr::select(REGION,STUSPS,NAME,Year,Meanrate,Crimerate) %>% 
  dplyr::rename("Unemplyrate"="Meanrate")

#############Check the missing values

which(is.na(CS_Erate_Crate1$REGION))

###########Save the object CS_Erate_Crate1 into CS_Erate_CrateCombined.Rds
saveRDS(CS_Erate_Crate1, file = "CS_Erate_CrateCombined1.Rds")


### Create Plots
## Set up variables before plotting
# Perform the filter for the Year, preserving geometry only from CS_Erate_Crate1
CS_Erate_Crate2 <- CS_Erate_Crate1 %>%
  filter(Year == 2014)

# Plot 1 The unemployment spatial map in 2014 over contiguous USA
CS_Erate_Crate2 %>%
  ggplot() +
  geom_sf(aes(fill = Unemplyrate), color = "white", size = 0.3) +
  scale_fill_gradientn(colors = c("darkblue", "lightblue"), name = "Year2014-Unemployment Rate",
                       limits = c(3, 10)) +
  #scale_color_manual(values = c("B"="blue"), labels = c("Area of Interest"), name="") +
  labs(title = "Unemployment Rate Map Over Contiguous US") +
  #scale bar
  annotation_scale(location = "bl", width_hint=0.5) +
  #scalebar(data=IN, location = "bottomleft", anchor = c(x=-88.5, y=37), dist = 150,
           #dist_unit="km", transform=TRUE, model="WGS84", st.dist=0.04)+
  ## Add North Arrow
  annotation_north_arrow(location="br", which_north="true",
                         # pad_x = unit(2.5, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(legend.position="right",       #change 'top' to specific position by c(2.5,0.8)
        plot.title = element_text(hjust = 0.5,
                                  color = "Gray40",
                                  size = 16,
                                  face = "bold"),
        plot.subtitle = element_text(color="blue"),
        plot.caption = element_text(color="Gray60")) +
  xlab("Longitude") + ylab("Latitude")
 
#Plot 1a The crimerate spatial map in 2014 over contiguous USA
CS_Erate_Crate2 %>%
  ggplot() +
  geom_sf(aes(fill = Crimerate), color = "white", size = 0.3) +
  scale_fill_gradientn(colors = c("darkblue", "lightblue"), name = "Year2014-Crime Rate",
                       limits = c(.1, .6)) +
  #scale_color_manual(values = c("B"="blue"), labels = c("Area of Interest"), name="") +
  labs(title = "Crime Rate Map Over Contiguous US") +
  #scale bar
  annotation_scale(location = "bl", width_hint=0.5) +
  #scalebar(data=IN, location = "bottomleft", anchor = c(x=-88.5, y=37), dist = 150,
  #dist_unit="km", transform=TRUE, model="WGS84", st.dist=0.04)+
  ## Add North Arrow
  annotation_north_arrow(location="br", which_north="true",
                         # pad_x = unit(2.5, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(legend.position="right",       #change 'top' to specific position by c(2.5,0.8)
        plot.title = element_text(hjust = 0.5,
                                  color = "Gray40",
                                  size = 16,
                                  face = "bold"),
        plot.subtitle = element_text(color="blue"),
        plot.caption = element_text(color="Gray60")) +
  xlab("Longitude") + ylab("Latitude")

# Plot 2 Region 1 Unemployment
Region1 <- CS_Erate_Crate2 %>%
  filter(REGION == 1, Year == 2014)

Region1 %>%
  ggplot() +
  geom_sf(aes(fill = Unemplyrate), color = "white", size = 0.3) +
  scale_fill_gradientn(colors = c("darkblue", "lightblue"), name = "Region 1 - Unemployment Rate") +
  #scale_color_manual(values = c("B"="blue"), labels = c("Area of Interest"), name="") +
  labs(title = "Region 1 Unemployment Rate Map") +
  #scale bar
  annotation_scale(location = "bl", width_hint=0.5) +
  #scalebar(data=IN, location = "bottomleft", anchor = c(x=-88.5, y=37), dist = 150,
  #dist_unit="km", transform=TRUE, model="WGS84", st.dist=0.04)+
  ## Add North Arrow
  annotation_north_arrow(location="br", which_north="true",
                         # pad_x = unit(2.5, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(legend.position="right",       #change 'top' to specific position by c(2.5,0.8)
        plot.title = element_text(hjust = 0.5,
                                  color = "Gray40",
                                  size = 16,
                                  face = "bold"),
        plot.subtitle = element_text(color="blue"),
        plot.caption = element_text(color="Gray60")) +
  xlab("Longitude") + ylab("Latitude")

#Plot 2a Crime Rate Region 2
Region2 <- CS_Erate_Crate2 %>%
  filter(REGION == 2, Year == 2014)

Region2 %>%
  ggplot() +
  geom_sf(aes(fill = Crimerate), color = "white", size = 0.3) +
  scale_fill_gradientn(colors = c("darkblue", "lightblue"), name = "Region 2 - Crime Rate") +
  #scale_color_manual(values = c("B"="blue"), labels = c("Area of Interest"), name="") +
  labs(title = "Region 2 Crime Rate Map") +
  #scale bar
  annotation_scale(location = "bl", width_hint=0.5) +
  #scalebar(data=IN, location = "bottomleft", anchor = c(x=-88.5, y=37), dist = 150,
  #dist_unit="km", transform=TRUE, model="WGS84", st.dist=0.04)+
  ## Add North Arrow
  annotation_north_arrow(location="br", which_north="true",
                         # pad_x = unit(2.5, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(legend.position="right",       #change 'top' to specific position by c(2.5,0.8)
        plot.title = element_text(hjust = 0.5,
                                  color = "Gray40",
                                  size = 16,
                                  face = "bold"),
        plot.subtitle = element_text(color="blue"),
        plot.caption = element_text(color="Gray60")) +
  xlab("Longitude") + ylab("Latitude")

## Plot 3 Scatter Plot with colors by Region

# Create a plot with colors by Region
p <- ggplot(data = CS_Erate_Crate2, aes(x = Crimerate, y = Unemplyrate, color = REGION)) +
  geom_point(aes(text = paste("Crime Rate:", Crimerate, "<br>Unemployment Rate:", Unemplyrate))) +  # Add tooltip text
  labs(x = "CrimeRate per 100 people", y = "UnemploymentRates per 100 people") +  # Labels for the axes
  ggtitle("Unemployment and Crime Rate in 2014") +  # Optional title
  theme_minimal() +  # Clean, minimalistic theme
  theme(
    legend.position = "right",              # Positions legend outside the grid on the right
    legend.justification = "top",           # Aligns legend to the top
    legend.box.margin = margin(0, 10, 0, 0), # Adds space between the plot and the legend
    legend.title = element_blank()
  )

# Add centroids for tooltips
#p <- p +
#  geom_point(data = centroids_df,
#             aes(x = lon, y = lat, text = paste(NAME, "<br>Unemployment:", Unemplyrate)),
#             size = 0.01, color = "transparent") 

# Convert to interactive plot with tooltip at centroid
interactive_map <- ggplotly(p, tooltip = "text") %>%
  layout(hovermode = 'text')

interactive_map

## Plot 4 The time series visualization for some states
#Select states
states_to_plot <- c("California", "Idaho", "Illinois", "Indiana")
#Get states and 8 years of data
ts_data <- CS_Erate_Crate1 %>%
  filter(Year >= 2007, Year <= 2014, NAME %in% states_to_plot)
# Plot the time series
p <- ggplot(data = ts_data, aes(x = Year, y = Unemplyrate, color = NAME, group = NAME)) +
  geom_line(size = 1) +  # Creates a line for each state
  geom_point(aes(text = paste("Year:", Year, "<br>Unemployment Rate:", Unemplyrate))) + # Add tooltips for each point
  labs(x = "Year", y = "Unemployment Rate", color = "") +  # Label axes and remove legend title
  ggtitle("Unemployment Rate changes along with Years") +
  theme_minimal() +
  scale_color_manual(values = c("green","orange", "blue", "purple")) +  # Specify colors for each state
  scale_x_continuous(breaks = 2007:2014) +  # Sets x-axis to start at 2007 with labels for each year
  scale_y_continuous(limits = c(3, 14)) +   # Sets y-axis limits from 4 to 14
  theme(
    legend.position = "right",              # Positions legend outside the grid on the right
    legend.justification = "top",           # Aligns legend to the top
    legend.box.margin = margin(0, 10, 0, 0) # Adds space between the plot and the legend
  )

# Convert to interactive plot with tooltip at centroid
interactive_map <- ggplotly(p, tooltip = "text") %>%
  layout(hovermode = 'text')

interactive_map