library(tidyverse)
glimpse(progression_summary_re)
library(forcats)

progression_summary_re |> 
    filter(!is.na(students_stu_class)) |> 
    mutate(students_stu_class = factor(students_stu_class, 
           levels = c("FR", "SO", "JR", "SR"))) |>  # <--- This forces the order
    group_by(re) |> 
    ggplot(aes(x = students_stu_class, y = avg_gpa, group = re, color = re)) +
    geom_line()


library(shiny)
library(ggplot2)
library(dplyr)

# --- UI ---
ui <- fluidPage(
  titlePanel("Single Line Highlighter"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown to select the specific line ID
      selectInput(inputId = "selected_id", 
                  label = "Select a Line to Highlight:", 
                  choices = unique(progression_summary_re$re)) # Populates list from data
    ),
    
    mainPanel(
      plotOutput("main_plot")
    )
  )
)

# --- SERVER ---
server <- function(input, output) {
  
  output$main_plot <- renderPlot({
    
    # Create a subset of data for the SINGLE selected line
    highlighted_data <- progression_summary_re %>% 
      filter(re == input$selected_id)
    
    ggplot() +
      # Layer 1: Plot ALL lines in light grey (Context)
      geom_line(data = progression_summary_re, 
                aes(x = students_stu_class, y = avg_gpa, group = re), 
                color = "lightgrey", size = 0.8) +
      
      # Layer 2: Plot SELECTED line in color (Focus)
      geom_line(data = highlighted_data, 
                aes(x = students_stu_class, y = avg_gpa, group = re), 
                color = "#007BC2", size = 1.5) +
      
      theme_minimal() +
      labs(title = paste("Highlighting Line:", input$selected_id))
  })
}

shinyApp(ui, server)
