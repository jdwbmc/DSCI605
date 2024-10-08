library(tidyverse)
library(plotly)
library(readxl)


setwd("~/GitHub/DSCI605/Mod9")
crime = readxl::read_xlsx('Sampledata2.xlsx')

selected_state <- "California"  # Replace "California" with your desired state
filtered_data <- crime %>% filter(State %in% selected_state)

plot <- plot_ly(filtered_data, x = ~Year, y = ~CrimeRate, type = "scatter", mode = "lines+markers",
                marker = list(color = "blue")) %>%
  layout(title = paste("Crime Rate for", selected_state), xaxis = list(title = "Year"),
         yaxis = list(title = "Crime Rate"))

plot