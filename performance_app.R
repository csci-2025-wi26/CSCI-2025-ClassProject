library(shiny)
library(bslib)
library(shinycssloaders)
library(tidyverse)
library(forcats)
 
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
      p("Enrollment content goes here!")
    ),
    tabPanel(
      "Performance",
      tabsetPanel(
        tabPanel("DFW Rate",
          fluidRow(
            column(6,
              selectInput("dfwvar", "Plot by:", 
                choices = c("Departments" = "stc_depts", 
              "Course Level (1000, 2000, 3000, etc.)" = "crs_level", 
              "Course Name" = "stc_course_name", 
              "Student Year (Freshman, Sophomore, etc.)" = "students_stu_class")),
              radioButtons("dfwdem", "Select Demographic", choices = c("None" = "NULL",
                "Gender" = "person_gender", 
                "Race (re)" = "re", 
                "Pell (Need-based financial aid)" = "pell"))
            ),
            column(6,
              textOutput("dfwtext")
            )
          ),
          fluidRow(
            column(12,
              plotOutput("plotdfw")
            )
          )
        ),
        tabPanel("GPA",
          fluidRow(
            column(6,
              selectInput("gpavar", "Plot by:",
                choices = c("Departments" = "stc_depts", 
              "Course Level (1000, 2000, 3000, etc.)" = "crs_level", 
              "Course Name" = "stc_course_name", 
              "Student Year (Freshman, Sophomore, etc.)" = "students_stu_class")),
              selectInput("gpaplotchoice", "Select Graph", choices = c("Column", "Boxplot", "Jitterplot")),
              radioButtons("gpadem", "Select Demographic", choices = c("Gender" = "person_gender", 
                "Race (re)" = "re", 
                "Pell (Need-based financial aid)" = "pell"))
            ),
            column(6,
              textOutput("gpatext")
            )
          ),
          fluidRow(
            column(12,
              plotOutput("plotgpa")
            )
          )
        ),
        tabPanel("GPA Over Time",
          fluidRow(
            column(6,
              selectInput("revar", "Choose Race",
              choices = c("American Indian or Alaska Native", "Asian", "Black or African American",
                "Hispanic/Latino", "Native Hawaiian or Other Pacific Islander", "Nonresident alien",
                "Race or ethnicity unknown", "Two or more races", "White"))
            ),
            column(6,
              textOutput("timetext")
            )
          ),
          fluidRow(
            column(12,
              plotOutput("plot_re_over_time")
            )
          )
        ),
        tabPanel("Experiment",
          fluidRow(
            column(12,
              selectInput("variable", "Name", choice = c(""))
            )
          ),
          fluidRow(
            column(12,
              plotOutput("test")
            )
          )
        )
      )
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
  # Enrollment server stuff goes here!


  ### Performance Tab ###
  # Performance server stuff goes here!
  source("scratch\\performance_global.R")

  dfw_rates_grouped <- reactive({
    if (input$dfwdem == "NULL") {
      cleaned_data |>
      group_by(.data[[input$dfwvar]]) |>
      summarize(mean(dfw, na.rm = TRUE)) |>
      rename(dfw_rate = 'mean(dfw, na.rm = TRUE)')
    } else {
      cleaned_data |>
      group_by(.data[[input$dfwvar]]) |>
      summarize(mean(dfw, na.rm = TRUE), .data[[input$dfwdem]]) |>
      rename(dfw_rate = 'mean(dfw, na.rm = TRUE)')
    }
  })

  gpa_grouped <- reactive({cleaned_data |> 
    group_by(.data[[input$gpavar]]) |> 
    summarise(mean(grade_numeric, na.rm = TRUE), .data[[input$dfwgpa]]) |>
    rename(gpa = 'mean(grade_numeric, na.rm = TRUE)')
  })

  plot_geom <- reactive({
    switch(input$gpaplotchoice,
    Column = geom_col(),
    Boxplot = geom_boxplot(),
    Jitterplot = geom_jitter(alpha = 0.5))
  })

  output$plotdfw <- renderPlot({
    ggplot(dfw_rates_grouped(), aes(x = .data[[input$dfwvar]], y = dfw_rate)) +
      geom_col()
  })

  output$plotgpa <- renderPlot({
    if (input$gpaplotchoice == "Column") {
      ggplot(gpa_grouped(), aes(x = .data[[input$gpavar]], y = gpa)) +
        plot_geom()
    } else {
      ggplot(cleaned_data, aes(x = .data[[input$gpavar]], y = grade_numeric)) +
        plot_geom()
    }
  })

  progression_factored <- progression_summary_re |> 
    filter(!is.na(students_stu_class)) |> 
    mutate(students_stu_class = factor(students_stu_class, 
      levels = c("FR", "SO", "JR", "SR"))) |>  # <--- This forces the order
    group_by(re)

  output$plot_re_over_time <- renderPlot({
    highlighted_data <- progression_factored |>
      filter(re == input$revar)
    ggplot() +
    # Layer 1: Plot ALL lines in light grey (Context)
    geom_line(data = progression_factored,
      aes(x = students_stu_class,y = avg_gpa, group = re), 
      color = "lightgrey", size = 0.8) +
    # Layer 2: Plot SELECTED line in color (Focus)
    geom_line(data = highlighted_data,
      aes(x = students_stu_class, y = avg_gpa, group = re), 
      color = "#007BC2", size = 1.5) +
    theme_minimal() +
    labs(title = paste("Highlighting Line:", input$revar))
  })

  output$dfwtext <- renderText({"Stuff about dfw plot goes here"})

  output$gpatext <- renderText({"Stuff about gpa plot goes here. Might need to
    use if statements to change text output depending on plot and variables chosen"})
  
  output$timetext <- renderText({"More text output that I will leave Aayush to take
    care of if he doesn't mind"})

  ### Retention Tab ###
  # Retention server stuff goes here!


  ### Outcomes Tab ###
  # Outcomes server stuff goes here!


}

shinyApp(ui, server)