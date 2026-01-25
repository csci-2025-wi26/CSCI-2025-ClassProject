library(shiny)
library(ggplot2)

source("re_global.R")

ui <- fluidPage(
  titlePanel("Institutional Retention Analytics"),
  
  tabsetPanel(
    tabPanel("Department Retention by Major", 
             plotOutput("deptPlot", height = "800px")),
    
    tabPanel("Department Retention after Intro Course", 
             helpText("Analysis of students who continued in the department after an intro class."),
             plotOutput("introPlot", height = "800px"))
  )
)

server <- function(input, output) {
  
  # First Chart: Department Metention by Major
  output$deptPlot <- renderPlot({
    ggplot(retention_data, aes(x = reorder(dept, retention_rate), y = retention_rate)) +
      geom_bar(stat = "identity", fill = "#533860") +
      coord_flip() +
      theme_minimal() + 
      labs(title = "Major Retention by Department", x = "Department", y = "Retention Rate (%)") +
      theme(text = element_text(family = "Roboto Slab"), plot.title = element_text(family = "Nova Proxima", face = "bold", size = 18)) +
      geom_text(aes(label = paste0(round(retention_rate, 1), "%")), hjust = -0.1, size = 3)
  })
  
  # Second Chart: Department Retention after Intro Course (need to be fixed)
  output$introPlot <- renderPlot({
    ggplot(intro_retention_data |> head(30), # Showing only top 30 courses, yet is showing to be wrong - 100%+ not what we want -(Working on that...)
           aes(x = reorder(paste(intro_course_code, "-", intro_course_title), retention_rate), y = retention_rate)) +
      geom_bar(stat = "identity", fill = "#228B22") +
      coord_flip() +
      theme_minimal() + 
      labs(title = "Top 30 Gateway Courses", subtitle = "Retention within department after taking course", x = "Course", y = "Retention Rate (%)") +
      theme(text = element_text(family = "Roboto Slab"), plot.title = element_text(family = "Nova Proxima", face = "bold", size = 18)) +
      geom_text(aes(label = paste0(round(retention_rate, 1), "%")), hjust = -0.1, size = 3)
  })
}

shinyApp(ui = ui, server = server)