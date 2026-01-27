library(shiny)
library(ggplot2)

source("re_global.R")

real_min <- min(processed_students$grad_year, na.rm = TRUE)
real_max <- max(processed_students$grad_year, na.rm = TRUE)

if(is.infinite(real_min)) real_min <- 2020
if(is.infinite(real_max)) real_max <- 2025

default_start <- max(real_min, 2020)
default_end   <- min(real_max, 2025)

ui <- fluidPage(
  titlePanel("Institutional Retention Analytics"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Global Filters"),
      sliderInput("yearRange", "Graduation Year Range:",
                  min = real_min, max = real_max,
                  value = c(default_start, default_end), sep = "", step = 1),
      
      sliderInput("minClassSize", "Minimum Program Size (Total Declared):",
                  min = 1, max = 50, value = 5),
      helpText("Filters out small majors to remove statistical noise."),
      
      hr(),
      h4("Drill-Down"),
      helpText("Select a major below to see detailed student pathways in Tab 2."),
      selectInput("selectedMajor", "Select Major:", 
                  choices = sort(unique(valid_depts$Dept_Code))),
      
      # --- MOVED QUICK GUIDE HERE ---
      br(),
      wellPanel(
        h5("Quick Guide:"),
        tags$ul(
          tags$li(strong("Top Right:"), " Stars (High Popularity, High Retention)"),
          tags$li(strong("Bottom Right:"), " Churn (High Popularity, Low Retention)"),
          tags$li(strong("Top Left:"), " Niche Gems (Small, Dedicated)"),
          tags$li(strong("Bottom Left:"), " At Risk (Small, Leaky)")
        ),
        style = "font-size: 0.9em; color: #555;" # Optional: Makes it look a bit neater in the sidebar
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Retention Landscape (Quadrants)", 
                 br(),
                 p("Visualizing the relationship between Popularity (Total Students) and Success (Retention Rate)."),
                 plotOutput("landscapePlot", height = "600px", click = "plot_click")
                 # --- QUICK GUIDE REMOVED FROM HERE ---
        ),
        
        tabPanel("Student Pathways (Where did they go?)", 
                 br(),
                 h3(textOutput("flowTitle")),
                 p("Of all students who ever declared this major, here is their final academic outcome."),
                 plotOutput("flowPlot", height = "600px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  landscape_data <- reactive({
    data <- get_bubble_data(input$yearRange[1], input$yearRange[2])
    data |>
      filter(total >= input$minClassSize)
  })
  
  flow_data <- reactive({
    get_major_flow(input$selectedMajor, input$yearRange[1], input$yearRange[2])
  })
  
  output$landscapePlot <- renderPlot({
    df <- landscape_data()
    
    if(nrow(df) == 0) return(NULL)
    
    avg_rate <- mean(df$rate, na.rm = TRUE)
    avg_pop  <- median(df$total, na.rm = TRUE)
    
    df <- df |> mutate(
      color_group = ifelse(dept == input$selectedMajor, "Selected", "Others"),
      alpha_val   = ifelse(dept == input$selectedMajor, 1, 0.7)
    )
    
    ggplot(df, aes(x = total, y = rate, size = graduated)) +
      geom_vline(xintercept = avg_pop, linetype = "dashed", color = "gray60") +
      geom_hline(yintercept = avg_rate, linetype = "dashed", color = "gray60") +
      geom_point(aes(fill = color_group, alpha = alpha_val), shape = 21, color = "white") +
      geom_text(aes(label = dept), vjust = -1, size = 3.5, check_overlap = FALSE) +
      scale_fill_manual(values = c("Others" = "#533860", "Selected" = "#FF4500")) +
      scale_size_continuous(range = c(3, 15)) +
      scale_alpha_identity() +
      theme_minimal() +
      labs(title = paste("Retention Landscape (Grads:", input$yearRange[1], "-", input$yearRange[2], ")"),
           subtitle = "Size of bubble = Number of Graduates",
           x = "Total Students (Ever Declared)",
           y = "Retention Rate (%)") +
      theme(legend.position = "none",
            plot.title = element_text(size = 16, face = "bold"))
  })
  
  output$flowTitle <- renderText({
    paste("Pathways for:", input$selectedMajor)
  })
  
  output$flowPlot <- renderPlot({
    df <- flow_data()
    
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    
    df <- df |>
      mutate(
        type = case_when(
          str_detect(outcome, "Retained") ~ "Retained",
          str_detect(outcome, "Switched") ~ "Switched Major",
          str_detect(outcome, "Still Enrolled") ~ "Still Enrolled",
          str_detect(outcome, "Dropped Out") ~ "Dropped Out",
          TRUE ~ "Other"
        )
      )
    
    ggplot(df, aes(x = reorder(outcome, n), y = n, fill = type)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      geom_text(aes(label = n), hjust = -0.2, size = 5) +
      scale_fill_manual(values = c(
        "Retained" = "#228B22",         # Green
        "Switched Major" = "#FFD700",   # Yellow
        "Still Enrolled" = "#1E90FF",   # Blue
        "Dropped Out" = "#B22222",      # Red
        "Other" = "gray"
      )) +
      theme_minimal() +
      labs(x = NULL, y = "Number of Students", fill = "Outcome Group") +
      theme(
        axis.text.y = element_text(size = 12, face = "bold"),
        legend.position = "bottom"
      )
  })
  
  observeEvent(input$plot_click, {
    res <- nearPoints(landscape_data(), input$plot_click, allRows = TRUE)
    selected <- res |> filter(selected_) |> pull(dept)
    if(length(selected) > 0) {
      updateSelectInput(session, "selectedMajor", selected = selected[1])
    }
  })
}

shinyApp(ui = ui, server = server)