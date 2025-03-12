options(shiny.port = 1133, shiny.autoreload = TRUE)

library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(shinyWidgets)  # For airMonthpickerInput

df <- read.csv("data/processed/combined.csv")
df <- df |> select(-X)

df$date <- as.Date(df$date)

df$Year <- year(df$date)
df$Month <- month(df$date, label = TRUE, abbr = FALSE)  
df$Month_Num <- month(df$date)  


min_date <- min(df$date, na.rm = TRUE)
max_date <- max(df$date, na.rm = TRUE)


ui <- fluidPage(
  
  titlePanel("📊 Vancouver Crime Stats "),
  
  fluidRow(
    
    column(4,
           wellPanel(  
             h4("🔍 Filters"),
             
             selectInput("neighborhood", "Select Neighborhood:", 
                         choices = unique(df$Neighbourhood), 
                         selected = unique(df$Neighbourhood)[1]),
             
             selectInput("y_var", "Crime Type:", 
                         choices = colnames(df)[!colnames(df) %in% c("Neighbourhood", "date", "Month", "Year")], 
                         selected = "Assaults"),
             
             airMonthpickerInput("start_date", "Start Date:",
                                 minDate = min_date, maxDate = max_date, 
                                 value = min_date, view = "months"),
             
             airMonthpickerInput("end_date", "End Date:",
                                 minDate = min_date, maxDate = max_date, 
                                 value = max_date, view = "months")
           )
    ),
    
    column(8, 
           plotOutput("plot", width = "100%")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  output$plot <- renderPlot({
    start_date <- as.Date(paste0(input$start_date, "-01"))
    end_date <- as.Date(paste0(input$end_date, "-01"))
    
    title_text <- paste("Crime Trends in", input$neighborhood, 
                        "from", input$start_date, "to", input$end_date)
    
    df |> 
      filter(
        Neighbourhood == input$neighborhood,
        date >= start_date & date <= end_date
      ) |> 
      ggplot(aes(x = date, y = .data[[input$y_var]])) +  
      geom_line(color = "steelblue") +  
      geom_point(color = "darkred", size = 2) +  
      labs(title = title_text, x = "Date", y = "Crime Count") +  
      theme_classic() +  
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
  })
}

shinyApp(ui, server)
