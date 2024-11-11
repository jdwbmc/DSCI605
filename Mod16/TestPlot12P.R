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

## Plot 3 Scatter Plot with colors by Region
# Create an interactive scatter plot using plot_ly
# Create an interactive scatter plot using plot_ly
plot_ly(
  data = CS_Erate_Crate2,
  x = ~Crimerate,             # Use Crime Rate for x-axis
  y = ~Unemplyrate,           # Use Unemployment Rate for y-axis
  type = 'scatter',           # Set type to scatter
  mode = 'markers'            # Use markers only (no lines)
) %>%
  layout(
    title = "Basic Scatter Plot",   # Simple title for the plot
    xaxis = list(title = "Crime Rate per 100 people"),  # Label for x-axis
    yaxis = list(title = "Unemployment Rates per 100 people")  # Label for y-axis
  )





## Plot 4 The time series visualization of Unemployment for some states
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

#Plot4a The time series visualization of Crimerate for some states
#Select states
states_to_plot <- c("California", "Idaho", "Florida", "Indiana")
#Get states and 8 years of data
ts_data <- CS_Erate_Crate1 %>%
  filter(Year >= 2007, Year <= 2014, NAME %in% states_to_plot)
# Plot the time series
p <- ggplot(data = ts_data, aes(x = Year, y = Crimerate, color = NAME, group = NAME)) +
  geom_line(size = 1) +  # Creates a line for each state
  geom_point(aes(text = paste("Year:", Year, "<br>Crime Rate:", Crimerate))) + # Add tooltips for each point
  labs(x = "Year", y = "Crime Rate", color = "") +  # Label axes and remove legend title
  ggtitle("Crime Rate changes along with Years") +
  theme_minimal() +
  scale_color_manual(values = c("green","orange", "blue", "purple")) +  # Specify colors for each state
  scale_x_continuous(breaks = 2007:2014) +  # Sets x-axis to start at 2007 with labels for each year
  scale_y_continuous(limits = c(0.0, 0.7)) +   # Sets y-axis limits from 4 to 14
  theme(
    legend.position = "right",              # Positions legend outside the grid on the right
    legend.justification = "top",           # Aligns legend to the top
    legend.box.margin = margin(0, 10, 0, 0) # Adds space between the plot and the legend
  )

# Convert to interactive plot with tooltip at centroid
interactive_map <- ggplotly(p, tooltip = "text") %>%
  layout(hovermode = 'text')

interactive_map