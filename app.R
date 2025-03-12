options(shiny.port = 1133, shiny.autoreload = TRUE)

library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)

df <- read.csv("data/processed/combined.csv")
df <- df |> select(-X)

df$date <- as.Date(df$date)

df$Year <- year(df$date)
df$Month <- month(df$date, label = TRUE, abbr = FALSE)  # Full month names

# Get the min/max years and months in the dataset
min_year <- min(df$Year, na.rm = TRUE)
max_year <- max(df$Year, na.rm = TRUE)

ui <- fluidPage(
  selectInput("neighborhood", "Select Neighborhood:", 
              choices = unique(df$Neighbourhood), 
              selected = unique(df$Neighbourhood)[1]),
  
  selectInput("y_var", "Crime Committed:", 
              choices = colnames(df)[!colnames(df) %in% c("Neighbourhood", "date", "Month", "Year")], 
              selected = "Assaults"),
  
  # Year and Month Selection
  fluidRow(
    column(3, 
           selectInput("start_year", "Start Year:", 
                       choices = min_year:max_year, 
                       selected = min_year)),
    column(3,
           selectInput("end_year", "End Year:",
                       choices = min_year:max_year, selected = max_year))
  ),
  fluidRow(
    column(3, 
           selectInput("start_month", "Start Month:",
                          choices = month.name, 
                       selected = "January")),
    column(3, selectInput("end_month", "End Month:",
                          choices = month.name, 
                          selected = "December"))
  ),
  
  plotOutput("plot", width = "600px") 
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    df |> 
      filter(
        Neighbourhood == input$neighborhood,
         (Year >= input$start_year & Month >= input$start_month),
        (Year <= input$end_year & Month <= input$end_month)
        ) |> 
      ggplot(aes(x = date, y = .data[[input$y_var]])) +  
      geom_point() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui, server)
