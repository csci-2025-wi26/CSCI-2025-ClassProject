library(shiny)
library(ggplot2)

source("re_global.R")

ui <- fluidPage(
  titlePanel("Institutional Retention Analytics"),
  
  tabsetPanel(
    tabPanel("Department Retention by Major", 
             helpText("Analysis of GRADUATES only. Shows the % of students who graduated in a department out of all graduates who ever declared it."),
             plotOutput("deptPlot", height = "800px")),
    
    tabPanel("Recruitment Efficiency (Intro to Major)", 
             helpText("Analysis of students who took an Intro Course (100-level) in a department and eventually GRADUATED with that major."),
             plotOutput("introPlot", height = "800px"))
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
  
  output$deptPlot <- renderPlot({
    plot_data <- retention_data <- retention_stats |>
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
}

shinyApp(ui = ui, server = server)