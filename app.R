options(shiny.port = 1133, shiny.autoreload = TRUE)

library(shiny)
library(dplyr)
library(bslib)
library(lubridate)
library(ggplot2)

df <- read.csv("data/processed/combined.csv")
df <-  df |> select(-X)

ui <- page_fluid(
  selectInput(
    "neighborhood", 
    "Select Neighborhood:", 
    choices = unique(df$neighborhood), 
    selected = unique(df$neighborhood)[1] 
  ),
  plotOutput("plot", width = "600px") 
)

# Server side callbacks/reactivity
server <- function(input, output, session) {
  output$plot <- renderPlot({
    ggplot(df, aes(x = date, y = Assaults)) +
      geom_point()
  })
}

# Run the app/dashboard
shinyApp(ui, server)