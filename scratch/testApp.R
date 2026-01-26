
library(shiny)
library(bslib)
library(tidyverse)

ui <- page_fluid(
  selectInput(
  "var", 
  "Select Variable:", 
  choices = names(mtcars)),

  fileInput("upload", "Upload CSV File", accept = ".csv"),
  
  plotOutput("distPlot")
  )

server <- function(input, output, session) {
  raw_data <- reactive({
  req(input$upload)
  read_csv(input$upload$datapath)
})

# Generate the dropdown dynamically based on uploaded data
output$var_selector <- renderUI({
  req(raw_data())
  selectInput("var_choice", "Select Variable:", choices = names(raw_data()))
})

output$distPlot <- renderPlot({
  req(input$var_choice)
  
  raw_data() |>
    ggplot(aes(x = .data[[input$var_choice]])) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    theme_minimal()
})
}

shinyApp(ui, server)
