library(shiny)
library(bslib)
library(shinycssloaders)
library(tidyverse)
 
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
            column(12,
              selectInput("dfwopt1", "Plot by Departments, Course Level, Course Name, or Student Class Standing", 
                choices = c("stc_depts", "crs_level", "stc_course_name", "students_stu_class")),
              selectInput("dfwopt3", "Choose graph", choices = c("Histogram", "Boxplot", "Jitterplot")),
              radioButtons("dfwopt2", "Select Demographic", choices = c("person_gender", "re", "pell"))
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
            column(12,
              selectInput("gpaopt1", "Plot by Departments, Course Level, Course Name, or Student Class Standing", 
                choices = c("stc_depts", "crs_level", "stc_course_name", "students_stu_class"))
            )
          ),
          fluidRow(
            column(12,
              plotOutput("plotq2")
            )
          )
        ),
        tabPanel("Experiment",
          fluidRow(
            column(12
            )
          ),
          fluidRow(
            column(12,
              plotOutput("plotq3")
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

  dfw_rates_grouped <- reactive({cleaned_data |> 
    group_by(.data[[input$dfwopt1]]) |> 
    summarize(mean(dfw, na.rm = TRUE)) |>
    rename(dfw_rate = 'mean(dfw, na.rm = TRUE)')
  })
  
  plot_geom <- reactive({
    switch(input$dfwopt3,
    Boxplot = geom_boxplot(),
    Jitterplot = geom_jitter(),
    Histogram = geom_col())
  })

  output$plotdfw <- renderPlot({
    ggplot(dfw_rates_grouped(), aes(x = .data[[input$dfwopt1]], y = dfw_rate)) + 
      plot_geom() # can't filter by demographic yet
  }, res = 96)

  ### Retention Tab ###
  # Retention server stuff goes here!


  ### Outcomes Tab ###
  # Outcomes server stuff goes here!


}

shinyApp(ui, server)