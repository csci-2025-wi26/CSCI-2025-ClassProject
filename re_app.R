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
    
    ggplot(retention_data, aes(x = reorder(starting_major, retention_rate), y = retention_rate)) +
      geom_bar(stat = "identity", fill = "#533860") + # Brand Single-Series Color
      coord_flip() +
      theme_minimal() + 
      labs(
        title = "Nova Proxima: Major Retention Rates",
        subtitle = "Percentage of students who remain in their first declared major",
        x = "Starting major", 
        y = "Retention rate (%)",
        caption = "Data Source: Registrar Office | Calculations based on first major after 'OPEN' status"
      ) +
      theme(
        # Typography implementation
        plot.title = element_text(family = "Nova Proxima", face = "bold", size = 18),
        plot.subtitle = element_text(family = "Roboto Slab", size = 12),
        axis.title = element_text(family = "Roboto Slab", size = 10),
        axis.text = element_text(family = "Roboto Slab", size = 9),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()
      ) +
      # Add percentage labels to the bars
      geom_text(aes(label = paste0(round(retention_rate, 1), "%")), 
                hjust = -0.1, family = "Roboto Slab", size = 3)
  })
}

shinyApp(ui = ui, server = server)