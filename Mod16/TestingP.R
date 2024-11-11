# Load the plotly library
library(plotly)
library(tidyverse)

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

# Use the first 10 rows for simplicity
CS_Erate_Crate2$geometry <- NULL

plot_ly(
  data = CS_Erate_Crate2,
  x = ~Crimerate,
  y = ~Unemplyrate,
  type = 'scatter',
  mode = 'markers',
  color = ~as.factor(REGION)
) %>%
  layout(
    title = "Unemployment Rate and Crime Rate in 2014",
    xaxis = list(title = "Crime Rate per 100 people"),
    yaxis = list(title = "UnemploymentRates per 100 people")
  )

##Plot 4
# Define states to plot
states_to_plot <- c("California", "Idaho", "Illinois", "Indiana")
CS_Erate_Crate1$geometry <- NULL

# Filter data for the specified states and years
ts_data <- CS_Erate_Crate1 %>%
  filter(Year >= 2007, Year <= 2014, NAME %in% states_to_plot)

# Create the plotly time series plot
interactive_plot <- plot_ly(data = ts_data,
                            x = ~Year,
                            y = ~Unemplyrate,
                            color = ~NAME,
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

# Show the plot
interactive_plot

#Plot 5
# Define states to plot
states_to_plot <- c("California", "Idaho", "Florida", "Indiana")

# Filter data for the specified states and years
ts1_data <- CS_Erate_Crate1 %>%
  filter(Year >= 2007, Year <= 2014, NAME %in% states_to_plot)

# Create the plotly time series plot
interactive_plot <- plot_ly(data = ts1_data,
                            x = ~Year,
                            y = ~Crimerate,
                            color = ~NAME,
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

# Show the plot
interactive_plot