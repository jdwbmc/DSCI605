#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(tidyverse)
library(shiny)
library(plotly)
data(txhousing) ##ggplot

# Define UI for application that draws a histogram
ui <- fluidPage(
  selectInput(
    inputId = "inputcities",
    label = "Select a City",
    choices = unique(txhousing$city),
    selected = "Abilene",
    multiple = TRUE
  ),
  plotlyOutput(outputId = "outcities")
)


# Define server logic required to draw a histogram
server <- function(input, output, ...) {
    output$outcities <- renderPlot({
      plot_ly(txhousing, x=~date, y=~median, color=~city) %>%
        filter(city %in% input$inputcities) %>%
        add_markers() %>%
        group_by(city) %>%
        add_lines() %>%
        layout(title="Housing Sales in TX",
               xaxis= list(title="Date"),
               yaxis = list(title="Median sale price($))"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
