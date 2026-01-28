library(shiny)
library(bslib)
library(shinycssloaders)
library(tidyverse)
library(stringi)
library(scales)

registrar_weighted <- read_csv("../cleaned_weighted_registrar_data.csv")


# Define Lists for Inputs and Plots
unique_majors <- sort(unique(registrar_weighted$major))
unique_races <- sort(unique(registrar_weighted$person_per_races))

race_labels <- c(
  "AN" = "American Indian", 
  "AS" = "Asian",
  "BL" = "Black", 
  "HP" = "Hawaiian or Pacific Islander",
  "ME" = "Mexican",
  "WH" = "White",
  "NA" = "Not Listed"
)

race_map <- c(
  "WH" = "#6A5ACD",
  "ME" = "#000080",
  "AS" = "#00BFFF",
  "BL" = "#E2725B",
  "AN" = "#228B22",
  "HP" = "#008080",
  "NA" = "#FF7F50"
)

ui <- fluidPage(
  titlePanel("CSCI2025 Class Project Dashboard"),
  navlistPanel(
    id = "tabset",
    # This 'Home' tab is an example of how to insert your own inputs/outputs into the boilerplate
    tabPanel(
      "Home", # tabPanel display title
      h3("Welcome!"), # it's all components from here - add as many as you like!
      p("This serves as the base of what will become our collective Shiny App.\n This home tab includes some example code."),
      textInput("home_ex_text_in", "Example Input"),
      textOutput("home_ex_text_out")
    ),
    tabPanel(
      "Enrollment", 
      h3("Enrollment Dashboard"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          # Major Dropdown
          selectInput("enroll_major", "Select Major:", 
                      choices = c("All Majors", unique_majors), 
                      selected = "All Majors"),
          
          hr(),
          
          # Race Checkbox
          checkboxGroupInput("enroll_races", "Select Race(s):",
                             choices = unique_races,
                             selected = unique_races) # Select all by default
        ),
        mainPanel(
          # Row for the Line Chart
          h4("Total Enrollment Trends"),
          plotOutput("enroll_line_plot", height = "300px"),
          p("This dashboard displays student enrollment trends over a five-year period by academic major and race. Users may select a single major or view all majors using the dropdown menu."),

          p("To simplify the visualization, program-specific major codes were consolidated into broader major categories. For example, multiple Business-related codes were grouped under a single Business classification."),

          p("The bar chart presents academic year on the x-axis, enrollment counts on the y-axis, and race as the color encoding. Users may select which racial groups to display."),

          p("Students who identify with multiple races may appear in more than one race category, and students with multiple majors may appear in multiple major views. As a result, counts reflect enrollment occurrences rather than unique individuals."),

          br(),
          
          # Row for the Stacked Bar Chart
          h4("Enrollment by Race"),
          plotOutput("enroll_bar_plot", height = "400px"),
          #p("Hristina put your desciprtion here"),
        )
      )
    )
    ),

    tabPanel(
      "Performance", 
      p("Performance content goes here!")
    ),
    tabPanel(
      "Retention", 
      p("Retention content goes here!")
    ),
    tabPanel(
      "Outcomes", 
      p("Outcomes content goes here!")
    )
  )

 
server <- function(input, output, session) {
  ### Home Tab ###
  home_ex_reactive <- reactive({ # Example reactive component
    req(input$home_ex_text_in)
    str_length(input$home_ex_text_in)
  })
  output$home_ex_text_out <- renderText({ # Example output functionality
    length <- home_ex_reactive()
    str_glue("Your text is {length} character(s) long!")
  })

  ### Enrollment Tab ###
 enroll_filtered_data <- reactive({
    req(input$enroll_races) 
    
    data <- registrar_weighted
    
    # Filter by Major if specific major selected
    if (input$enroll_major != "All Majors") {
      data <- data |> filter(major == input$enroll_major)
    }
    
    # Filter by Race
    data <- data |> filter(person_per_races %in% input$enroll_races)
    
    return(data)
  })
  
  # 2. Line Chart: Total Enrollment by Year
  output$enroll_line_plot <- renderPlot({
    plot_data <- enroll_filtered_data() |>
      group_by(term_reporting_year) |>
      summarize(total_students = sum(student_weight, na.rm = TRUE))
    
    ggplot(plot_data, aes(x = term_reporting_year, y = total_students)) +
      geom_line(color = "#333333", size = 1.2) +
      geom_point(color = "#333333", size = 3) +
      theme_minimal() +
      scale_x_continuous(breaks = unique(plot_data$term_reporting_year)) +
      labs(x = "Year", y = "Total Enrollment") +
      theme(text = element_text(size = 14))
  })
  
  # 3. Stacked Bar Chart: Enrollment by Race
  output$enroll_bar_plot <- renderPlot({
    plot_data <- enroll_filtered_data() |>
      group_by(term_reporting_year, person_per_races) |>
      summarize(total_weighted_students = sum(student_weight, na.rm = TRUE), .groups = "drop")
    
    ggplot(data = plot_data, aes(x = term_reporting_year, y = total_weighted_students, fill = fct_rev(fct_infreq(person_per_races)))) +
      geom_col() +
      scale_fill_manual(
        values = race_map,
        labels = race_labels,
        name = "Race"
      ) +
      theme_minimal() +
      scale_x_continuous(breaks = unique(plot_data$term_reporting_year)) +
      labs(
        x = "Reporting Year",
        y = "Number of Students"
      ) +
      theme(
        legend.position = "bottom",
        text = element_text(size = 14)
      )
  })


  ### Performance Tab ###
  # Performance server stuff goes here!


  ### Retention Tab ###
  # Retention server stuff goes here!


  ### Outcomes Tab ###
  # Outcomes server stuff goes here!


}

shinyApp(ui, server)