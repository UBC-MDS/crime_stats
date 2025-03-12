options(shiny.port = 1133, shiny.autoreload = TRUE)

library(shiny)
library(dplyr)
library(bslib)
library(lubridate)
library(ggplot2)

df <- read.csv("data/processed/combined.csv")
df <-  df |> select(-X)

ui <- fluidPage(
  selectInput(
    "neighborhood", 
    "Select Neighborhood:", 
    choices = unique(df$Neighbourhood), 
    selected = unique(df$Neighbourhood)[1] 
  ),
  selectInput(
    "y_var",
    "Select Y-axis Variable:",
    choices = colnames(df)[!colnames(df) %in% c("Neighbourhood", "date")],
    selected = "Assaults"  # Default selection
  ),
  plotOutput("plot", width = "600px") 
)

# Server side callbacks/reactivity
server <- function(input, output, session) {
  output$plot <- renderPlot({
    df |> filter(Neighbourhood == input$neighborhood) |> 
      ggplot( aes(x = date, y = .data[[input$y_var]])) +
      geom_point() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}


shinyApp(ui, server)