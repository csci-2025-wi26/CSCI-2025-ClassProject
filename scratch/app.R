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

plots_by_major <- list(
  "Proportion graduated" = "prop_grad"
)

student_year_summary <- outcomes_data |> 
    mutate(dept = str_extract(primary_major, "^[^,]+")) |> 
    group_by(stc_person, term_year) |> 
    mutate(
      dept = first(dept),
      status = status,
      .groups = "drop"
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
      fluidRow(
        column(3,
          selectInput("select_dept", "Department", choices = dept)
        ),
        column(3,
          uiOutput("select_major_ui")
        ),
        column(3,
          uiOutput("select_plot_ui")
        )
      ),
      fluidRow(
        column(12,
          plotOutput("dept_major_plot")
        )
      )
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

  output$select_plot_ui <- renderUI({
    req(input$select_major)

    selectInput(
      "select_plot",
      "Plot",
      choices = plots_by_major
    )
  })

  output$dept_major_plot <- renderPlot({
    #Graduation Proportion for target department over time
    req(c(input$select_major, input$select_plot))
    
    switch(
      input$select_plot,
      "prop_grad" = {
        target_dept <- input$select_major # we can handle select for department over time
        student_year_summary |> 
          filter(status != "Currently Enrolled" & dept == target_dept) |> 
          ggplot(aes(x = as.factor(term_year), fill = status)) +
          geom_bar(position = "fill") +
          labs(
            title = sprintf("Student status — %s", target_dept),
            subtitle = "Retention and graduation, by academic year",
            x = "Academic year",
            y = "Share of students (proportion)",
            fill = "Graduation status"
          ) +
          theme_minimal() +
          scale_fill_manual(
            values = c("Dropped" = "#533860", "Graduated" = "#FFF42A"),
            labels = c("Dropped", "Graduated")
          ) +
          theme(
            plot.title = element_text(family = "Proxima Nova"),
            text = element_text(family = "Roboto Slab")
          )
     })
  })
}

shinyApp(ui, server)
