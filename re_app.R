library(shiny)
library(ggplot2)

source("re_global.R")

ui <- fluidPage(
  titlePanel("Institutional Retention Analytics"),
  
  tabsetPanel(
    tabPanel("Department Retention by Major", 
             helpText("Analysis of GRADUATES only. Shows the % of students who graduated in a department out of all graduates who ever declared it."),
             plotOutput("deptPlot", height = "800px")),
    
    tabPanel("Department Retention after Intro Course", 
             helpText("Analysis of students who continued in the department after an intro class."),
             plotOutput("introPlot", height = "800px")),
             
    tabPanel("Individual Major Lookup",
             sidebarLayout(
               sidebarPanel(
                 selectInput("selectedMajor", 
                             "Select a Major:", 
                             choices = sort(unique(retention_stats$dept))) 
               ),
               mainPanel(
                 plotOutput("majorSpecificPlot")
               )
             ))
  )
)

server <- function(input, output) {
  
  my_theme <- theme_minimal() +
    theme(
      text = element_text(family = "sans"),
      plot.title = element_text(face = "bold", size = 18),
      axis.text.y = element_text(size = 10),
      panel.grid.major.y = element_blank()
    )
  
  # First Chart: Department Retention by Major  
  output$deptPlot <- renderPlot({
    plot_data <- retention_stats |>
      filter(total >= 5) |> 
      arrange(desc(rate))
    
    ggplot(plot_data, aes(x = reorder(dept, rate), y = rate)) +
      geom_bar(stat = "identity", fill = "#533860") +
      coord_flip() +
      my_theme + 
      labs(title = "Major Retention Rate (Graduates Only)", 
           subtitle = "% of students who declared this major and successfully graduated with it",
           x = "Department", y = "Retention Rate (%)") +
      geom_text(aes(label = paste0(round(rate, 1), "% (n=", total, ")")), 
                hjust = -0.1, size = 3.5) +
      ylim(0, 110) 
  })
  
  # Second Chart: Intro Course Retention
  output$introPlot <- renderPlot({
    plot_data <- intro_stats |>
      filter(total >= 5) |> 
      arrange(desc(rate))
    
    ggplot(plot_data, aes(x = reorder(dept, rate), y = rate)) +
      geom_bar(stat = "identity", fill = "#228B22") + 
      coord_flip() +
      my_theme + 
      labs(title = "Intro Course to Major Conversion", 
           subtitle = "% of students taking a 100-level course who graduated with that major",
           x = "Department", y = "Conversion Rate (%)") +
      geom_text(aes(label = paste0(round(rate, 1), "% (n=", total, ")")), 
                hjust = -0.1, size = 3.5) +
      ylim(0, 110)
  })
  
  # Third Chart: Individual Lookup
  output$majorSpecificPlot <- renderPlot({
    filtered_data <- retention_stats[retention_stats$dept == input$selectedMajor, ]
    
    ggplot(filtered_data, aes(x = dept, y = rate)) +
      geom_bar(stat = "identity", fill = "#B22222", width = 0.4) +
      theme_minimal() +
      ylim(0, 100) +
      labs(title = paste("Retention Details:", input$selectedMajor),
           x = "Department/Major",
           y = "Retention Rate (%)") +
      theme(text = element_text(family = "sans"),
            plot.title = element_text(size = 18, face = "bold")) 
  })
}

shinyApp(ui = ui, server = server)