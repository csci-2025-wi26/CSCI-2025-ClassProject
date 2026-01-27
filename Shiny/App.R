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


  ### Retention Tab ###
  # Retention server stuff goes here!


  ### Outcomes Tab ###
  # Outcomes server stuff goes here!


}

shinyApp(ui, server)