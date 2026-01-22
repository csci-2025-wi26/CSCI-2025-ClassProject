library(shiny)
library(bslib)
library(tidyverse)

source("out_global.R")


ui <- page_fluid(
  selectInput(
    inputId = "major_select",
    label = "Select Primary Major:",
    choices = sort(unique(cleaned_data$primary_major)),
    selected = "POE"
  ),
)

server <- function(input, output, session) {}

shinyApp(ui, server)
