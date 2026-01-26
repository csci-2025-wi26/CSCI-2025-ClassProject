library(shiny)
library(bslib)
library(shinycssloaders)
library(tidyverse)
library(vroom)

# outcomes variables
outcomes_data <- vroom("../data/clean/registrar_cleaned.csv")

dept <- outcomes_data |> 
  distinct(stc_person, .keep_all = TRUE) |> 
  select(stc_depts) |> 
  mutate(stc_depts = str_match(stc_depts, "(\\w+),?")[,2]) |> # get first value
  distinct(stc_depts) |> 
  mutate(
    stc_depts = case_when(
      stc_depts == "POLEC" ~ "POE",
      stc_depts == "ENVI" ~ "ENV",
      stc_depts == "ACC" ~ "ACCT",
      str_detect(stc_depts, "^ATH$|^SOC$|") 
    )
  ) |> 
  pull() |> 
  sort()

majors_by_dept <- outcomes_data |> 
  distinct(stc_person, .keep_all = TRUE) |> 
  select(acad_dept = stu_acad_programs, major = students_xstu_grad_app_major.x) |>
  mutate(across(everything(), ~ str_match(., "(\\w+),?")[,2])) |> # get first value
  filter(!(major %in% c("NON", "NONGR", "OPEN")) & acad_dept %in% dept) |> 
  group_by(acad_dept) |> 
  distinct(major) |> 
  mutate(major = if_else(major != acad_dept & major %in% acad_dept, NA, major)) |> 
  drop_na(major) |> 
  arrange(acad_dept)


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
      selectInput("select_dept", "Department", choices = dept),
      uiOutput("select_major_ui")
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
  output$select_major_ui <- renderUI({
    req(input$select_dept)

    selectInput(
      "select_major",
      "Major",
      choices = majors_by_dept
    )
  })

}

shinyApp(ui, server)
