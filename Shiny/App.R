library(shiny)
library(bslib)
library(shinycssloaders)
library(tidyverse)

ui <- page_fluid(
  theme = bs_theme(bootswatch = "darkly"),
  titlePanel("CSCI-2026-Class-Project Homepage"),
  
  fluidRow(
    column(4,
      wellPanel(
        h3("Project Features"),
        navlistPanel(
          id = "tabset",
          widths = c(12, 12),
          tabPanel("Home", value = "home"),
          tabPanel("Enrollment", value = "enrollment"),
          tabPanel("Performance", value = "performance"),
          tabPanel("Outcomes", value = "outcomes"),
          tabPanel("Retention", value = "retention")
        )
      )
    ),

column(8,
uiOutput("main_display")
    )
  )
)

server <- function(input, output, session) {
  output$main_display <- renderUI({
    if (input$tabset == "home") {
      tagList(
        h2("Welcome to the Project Dashboard"),
        p("Select a feature from the sidebar on the left to begin.")
      )
    } else if (input$tabset == "enrollment") {
      tagList(
        h2("Enrollment Data"),
        # This is for the enrollment ui person, start writing your code for your tab here
        p("Put enrollment UI captions or what not.")
      )
    } else if (input$tabset == "performance") {
      tagList(
        h2("Performance Analysis"),
        # This is for the performance ui person, start writing your code for your tab here
        p("Put performance UI captions or what not.")
      )
    } else if (input$tabset == "outcomes") {
      tagList(
        h2("Student Outcomes"),
        # This is for the outcomes ui person, start writing your code for your tab here
        p("Put outcomes UI captions or what not.")
      )
    } else if (input$tabset == "retention") {
      tagList(
        h2("Retention Rates"),
        # This is for the retention ui person, start writing your code for your tab here
        p("Put retention UI captions or what not.")
      )
    }
  })
}

shinyApp(ui, server)