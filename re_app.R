library(shiny)
library(ggplot2)

source("re_global.R")

ui <- fluidPage(
  titlePanel("Student Retention Analysis"),
  
  mainPanel(
    plotOutput("retentionPlot", height = "800px")
  )
)

server <- function(input, output) {
  
  output$retentionPlot <- renderPlot({
    
    ggplot(retention_data, aes(x = reorder(major, retention_rate), y = retention_rate)) +
      geom_bar(stat = "identity", fill = "#533860") +
      coord_flip() +
      theme_minimal() + 
      labs(
        title = "Nova Proxima: Major Retention Rates",
        subtitle = "Percentage of students who remain in their declared major",
        x = "Major", 
        y = "Retention rate (%)",
        caption = "Data Source: Registrar Office | Calculations based on all major declarations after 'OPEN' status"
      ) +
      theme(
        plot.title = element_text(family = "Nova Proxima", face = "bold", size = 18),
        plot.subtitle = element_text(family = "Roboto Slab", size = 12),
        axis.title = element_text(family = "Roboto Slab", size = 10),
        axis.text = element_text(family = "Roboto Slab", size = 9),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()
      ) +
      geom_text(aes(label = paste0(round(retention_rate, 1), "%")), 
                hjust = -0.1, family = "Roboto Slab", size = 3)
  })
}

shinyApp(ui = ui, server = server)