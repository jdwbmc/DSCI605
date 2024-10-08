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
  # Reactive value to store filtered data
  filtered_data1 <- reactive({
    if (!is.null(input$inputstates)) {
      filtered_data <- crime %>%
        filter(State %in% input$inputstates)
      #cat("filtered_data1:\n")
      #print(filtered_data)
      #return(filtered_data)
    } else {
      crime  # Return the original data if no states are selected
    }
  })
  # Reactive value to store filtered data
  filtered_data2 <- reactive({
    if (!is.null(input$inputyears) && length(input$inputyears) > 0) {
      filtered_data <- crime %>%
        filter(Year %in% input$inputyears)
      #cat("filtered_data2:\n")
      #print(filtered_data)
      #return(filtered_data)
    } else {
      crime  # Return the original data if no years are selected
    }
  })
  output$outstates <- renderPlotly({
    plot_ly(filtered_data1(), x=~Year, y=~CrimeRate, type = 'scatter' , mode = 'lines+markers',
            marker = list(color = "blue")) %>%
      layout(title="Crime Rate Changes over Year",
             xaxis= list(title="Year"),
             yaxis = list(title="Crime Rate per 100,000 people"))
  })
  output$outyears <- renderPlotly({
    plot_ly(filtered_data2(), x=~CrimeRate, type="histogram") %>%
      layout(title="Crime Rate Histogram over all States",
             xaxis= list(title="Crime Rate per 100,000 people"),
             yaxis = list(title="Count"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
