library(tidyverse)
library(dplyr)

raw_data <- vroom::vroom("C:\\Users\\logan\\CSCI-2025-ClassProject\\data\\raw\\registrar_data.csv")

#adding dfw column
clean_data <- raw_data |> 
  mutate(dfw = case_when(
    xstc_verified_lettr_grade %in% c("A+", "A", "A-", "B+", "B", "B-", "C+", "C", "C-", "P") ~ 1, 
    xstc_verified_lettr_grade %in% c('W', "F", "D-", "D", "D+")  ~ 0
  ))

#__________add numeric gpa_______________

updated_clean_data <- clean_data |> 
  mutate(grade_numeric = case_when(
    xstc_verified_lettr_grade == "A" ~ 4.0,
    xstc_verified_lettr_grade == "A-" ~ 3.70,
    xstc_verified_lettr_grade == "B+" ~ 3.30,
    xstc_verified_lettr_grade == "B" ~ 3.00,
    xstc_verified_lettr_grade == "B-" ~ 2.70,
    xstc_verified_lettr_grade == "C+" ~ 2.30,
    xstc_verified_lettr_grade == "C" ~ 2.00,
    xstc_verified_lettr_grade == "C-" ~1.70,
    xstc_verified_lettr_grade == "D+" ~ 1.30,
    xstc_verified_lettr_grade == "D" ~ 1.00,
    xstc_verified_lettr_grade == "D-" ~ 0.70,
    xstc_verified_lettr_grade == "F" ~ 0.00 
  ))

# Shiny stuff
library(shiny)
library(bslib)

ui <- fluidPage(
  navlistPanel(
    "Academic Performance",
    tabPanel("Q1", value = "Q1",
      plotOutput("plotq1")
    ),
    tabPanel("Q2", value = "Q2"),
    tabPanel("Q3", value = "Q3")
  )
)

server <- function(input, output, session) {
  output$plotq1 <- renderPlot({
    ggplot(data.frame(x = c(1:5), y = c(6:10)), aes(x = x, y = y)) +
      geom_point()})
}

shinyApp(ui, server)