library(tidyverse)
library(shiny)
library(plotly)
library(readxl)

setwd("~/GitHub/DSCI605/Mod9")
crime = readxl::read_xlsx('Sampledata2.xlsx')

# Define UI for application that draws a histogram
ui <- fluidPage(
  selectInput(inputId = "inputstates",
              label = "Select a State",
              choices = unique(crime$State),
              selected = "Alabama",
              multiple = TRUE),
  selectInput(inputId = "inputyears",
              label = "Select a Year",
              choices = unique(crime$Year),
              selected = "2013",
              multiple = TRUE),
  plotlyOutput(outputId = "outstates"),
  plotlyOutput(outputId = "outyears")
)

# Define server logic required to draw a histogram
server <- function(input, output, ...) {
  output$outstates <- renderPlotly({
    plot_ly(crime, x=~Year, y=~CrimeRate, type = 'scatter' , mode = 'lines+markers',
            marker = list(color = "blue")) %>%
      filter(State %in% input$inputstates) %>%
      layout(title="Crime Rate Changes over Year",
             xaxis= list(title="Year"),
             yaxis = list(title="Crime Rate per 100,000 people"))
  })
  output$outyears <- renderPlotly({
    plot_ly(crime, x=~CrimeRate, type="histogram") %>%
      filter(Year %in% input$inputyears) %>%
      layout(title="Crime Rate Histogram over all States",
             xaxis= list(title="Crime Rate per 100,000 people"),
             yaxis = list(title="Count"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
