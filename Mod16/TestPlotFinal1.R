library(tidyverse)
library(raster)          #raster()
library(sf)              #st_read()
library(ggspatial)       #annotation_scale,annotation_north_arrow
library(ggnewscale)      #new_scale_color() 
#library(ggsn)            #scalebar()
library(shiny)           #Shiny app
library(plotly)          #plot_ly()
library(gridExtra)       #grid.arrange()
library(psych)

setwd("~/GitHub/DSCI605/Mod16")

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

Unemployrate <- Unemployrate %>%
  arrange(STUSPS, Year)

statsunemp <- describe(Unemployrate$Meanrate) %>%
  select("mean", "min", "max", "range", "skew", "kurtosis")
saveRDS(statsunemp, file = "statsunemp.Rds")

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

Crimerate <- Crimerate %>%
  arrange(STUSPS, Year)

statscrime <- describe(Crimerate$Crimerate) %>%
  select("mean", "min", "max", "range", "skew", "kurtosis")
saveRDS(statscrime, file = "statscrime.Rds")

### Create correlation data frame
new_table <- cbind(Unemployrate$Meanrate, Crimerate$Crimerate)
new_table <- as.data.frame(new_table)  # Convert to data frame if you need it in that format
# For Spearman correlation
correlation <- cor(new_table$V1, new_table$V2, method = "spearman")
# Calculate correlation for each row between `col1` and `col2`, and store it in a new column
new_table$correlation <- apply(new_table[, c("V1", "V2")], 1, function(x) cor(x[1], x[2]))
saveRDS(new_table, file = "new_table.Rds")

#other statistical test
# Perform One-Way ANOVA to compare crime rates across years
anova_crime <- aov(Crimerate ~ factor(Year), data = Crimerate)

# View the summary of the ANOVA result
summary(anova_crime)
print(anova_crime)

# Perform One-Way ANOVA to compare Unemployment rates across years
anova_unemployment <- aov(Meanrate ~ factor(Year), data = Unemployrate)

# View the summary of the ANOVA result
summary(anova_unemployment)
print(anova_unemployment)


save(list = c("anova_crime", "anova_unemployment"), file = "AnovaData.Rdata")


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
CS_Erate_Crate2 %>%   # Provide data to ggplot
  ggplot() +          # Start the graph
  geom_sf(aes(fill = Unemplyrate), color = "white", size = 0.3) +   # Plot the US map
  scale_fill_gradientn(colors = c("darkblue", "lightblue"), name = "Year2014-Unemployment Rate",
                       limits = c(3, 10)) +    # Set up the legend
  #scale_color_manual(values = c("B"="blue"), labels = c("Area of Interest"), name="") +
  labs(title = "Unemployment Rate Map Over Contiguous US") +   # Plot title
  #scale bar
  annotation_scale(location = "bl", width_hint=0.5) +   #Alternative scale bar for distance
  #scalebar(data=IN, location = "bottomleft", anchor = c(x=-88.5, y=37), dist = 150,
           #dist_unit="km", transform=TRUE, model="WGS84", st.dist=0.04)+
  ## Add North Arrow
  annotation_north_arrow(location="br", which_north="true",
                         style = north_arrow_fancy_orienteering) +
  theme(legend.position="right",       # Put legend on the right
        plot.title = element_text(hjust = 0.5,       # Tweak plot title
                                  color = "Gray40",
                                  size = 16,
                                  face = "bold")) +
  xlab("Longitude") + ylab("Latitude")    # Label x and y axis
 
#Plot 2 The crimerate spatial map in 2014 over contiguous USA
CS_Erate_Crate2 %>%        # Provide data to ggplot
  ggplot() +              # Start the graph
  geom_sf(aes(fill = Crimerate), color = "white", size = 0.3) +     # Plot the US map
  scale_fill_gradientn(colors = c("darkblue", "lightblue"), name = "Year2014-Crime Rate",
                       limits = c(.1, .6)) +        # Set up the legend
  #scale_color_manual(values = c("B"="blue"), labels = c("Area of Interest"), name="") +
  labs(title = "Crime Rate Map Over Contiguous US") +     # Set up the title
  #scale bar
  annotation_scale(location = "bl", width_hint=0.5) +     #Alternative scale bar for distance
  #scalebar(data=IN, location = "bottomleft", anchor = c(x=-88.5, y=37), dist = 150,
  #dist_unit="km", transform=TRUE, model="WGS84", st.dist=0.04)+
  ## Add North Arrow
  annotation_north_arrow(location="br", which_north="true",
                         style = north_arrow_fancy_orienteering) +
  theme(legend.position="right",       #change 'top' to specific position by c(2.5,0.8) Position legend to the right
        plot.title = element_text(hjust = 0.5,         # Tweak plot title
                                  color = "Gray40",
                                  size = 16,
                                  face = "bold")) +
  xlab("Longitude") + ylab("Latitude")                 # Label x and y axis

# Plot 3 Region 1 Unemployment
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

#Plot 4 Crime Rate for Region 2
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

## Plot 5 Scatter Plot with colors by Region
# Compare Crime Rate and Unemployment rates
CS_Erate_Crate2$geometry <- NULL  #Drop geometry column for plotly

plot_ly(
  data = CS_Erate_Crate2,          # Use this data file 
  x = ~Crimerate,                  # Use Crimerate for the x axis
  y = ~Unemplyrate,                # Use Unemplyrate for the y axis
  type = 'scatter',                # Create an interactive scatter plot
  mode = 'markers',                # Create the points at the intersection of the two
  color = ~as.factor(REGION)       # Color the points by the region they are in
) %>%
  layout(
    title = "Unemployment Rate and Crime Rate in 2014",       # Title
    xaxis = list(title = "Crime Rate per 100 people"),        # x-axis label
    yaxis = list(title = "UnemploymentRates per 100 people")  # y-axis label
  )

## Plot 6 The time series visualization for some states
#Select states
states_to_plot <- c("California", "Idaho", "Illinois", "Indiana")
CS_Erate_Crate1$geometry <- NULL      #Drop geometry column for plotly

# Filter data for the specified states and years
ts_data <- CS_Erate_Crate1 %>%
  filter(Year >= 2007, Year <= 2014, NAME %in% states_to_plot)

# Create the plotly Unemployment time series plot
plot_ly(data = ts_data,
        x = ~Year,                # Use Year for time series x axis variable
        y = ~Unemplyrate,         # Use Unemployrate for y axis
        color = ~NAME,            # Color points by state name
        type = 'scatter',         # Set type to scatter for lines and markers
        mode = 'lines+markers',   # Use lines and markers
        text = ~paste("Year:", Year, "<br>Unemployment Rate:", Unemplyrate),
        hoverinfo = 'text'        # Show tooltip text
) %>%
  layout(
    title = "Unemployment Rate Changes Along with Years",  # Title
    xaxis = list(title = "Year", tickvals = 2007:2014),    # x-axis label and ticks
    yaxis = list(title = "Unemployment Rate", range = c(3, 14)),  # y-axis label and range
    legend = list(title = list(text = ""))  # Remove legend title
  )

#Plot 7 The time series visualization of Crimerate for some states
# Define states to plot
states_to_plot <- c("California", "Idaho", "Florida", "Indiana")

# Filter data for the specified states and years
ts1_data <- CS_Erate_Crate1 %>%
  filter(Year >= 2007, Year <= 2014, NAME %in% states_to_plot)

# Create the plotly time series plot
plot_ly(data = ts1_data,
        x = ~Year,                # Use Year for time series x axis variable
        y = ~Crimerate,           # Use Crimerate variable for y axis
        color = ~NAME,            # Color points by state name
        type = 'scatter',         # Set type to scatter for lines and markers
        mode = 'lines+markers',   # Use lines and markers
        text = ~paste("Year:", Year, "<br>Crime Rate:", Crimerate),
        hoverinfo = 'text'        # Show tooltip text
) %>%
  layout(
    title = "Crime Rate Changes Along with Years",  # Title
    xaxis = list(title = "Year", tickvals = 2007:2014),    # x-axis label and ticks
    yaxis = list(title = "Crime Rate", range = c(.1, .8)),  # y-axis label and range
    legend = list(title = list(text = ""))  # Remove legend title
  )
