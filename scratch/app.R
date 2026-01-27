library(shiny)
library(bslib)
library(shinycssloaders)
library(tidyverse)
library(vroom)
library(scales)

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
  select(acad_dept = stu_acad_programs, major = primary_major) |>
  mutate(
    acad_dept = str_extract(acad_dept, "^[^,]+"),
    major     = str_extract(major, "^[^,]+")
  ) |> 
  filter(!(major %in% c("NON", "OPEN")) & acad_dept %in% dept) |> 
  filter(str_detect(major, acad_dept) | major == acad_dept) |> 
  distinct(acad_dept, major) |> 
  arrange(acad_dept)

outcome_plots <- list(
  "By Major" = c("Proportion Graduated" = "status_by_major")
)

status_by_major <- outcomes_data |> 
    select(stc_person, primary_major, term_year, status, stc_depts) |> 
    mutate(
      acad_dept = str_match(stc_depts, "(\\w+),?")[,2], 
      major = str_extract(primary_major, "^[^,]+"),
      status = if_else(status == "Dropped", "Unenrolled", status),
      status = fct(status, levels = c("Currently Enrolled", "Graduated", "Unenrolled"))
    ) |> 
    distinct(stc_person, .keep_all = TRUE) |> 
    group_by(acad_dept, major, term_year) |> 
    count(status) |> 
    ungroup()


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
      fluidRow(column(12,
        h3("Data Selection Criteria"), # Using h3 for a clean header
        p("Please use the dropdowns below to choose which graph to see. 
          You can choose which metric to plot, then further filter by department 
          as well as each major within that department.", 
          style = "color: #666; font-style: italic;")
      )
      ),
      fluidRow(
        column(3,
          selectInput("select_plot", "Metric to Plot", choices = outcome_plots)
        ),
        column(3,
          selectInput("select_dept", "Department", choices = dept)
        ),
        column(3,
          uiOutput("select_major_ui")
        )
      ),
      fluidRow(
        column(12, 
          p("The below plot demonstrates the portportion of students in each enrollment status per year.", 
            style = "font-size: 16px; color: #2c3e50;"),
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

  # if plot with major, render major selectors
  output$select_major_ui <- renderUI({
    req(input$select_dept)

    if (!str_detect(input$select_plot, "major")) {
      return(NULL)
    }

    dept_majors <- majors_by_dept |> 
      filter(acad_dept == input$select_dept) |> 
      pull(major)

      selectInput(
        "select_major",
        "Major",
        choices = c("Entire department", dept_majors)
      )
  })

  output$dept_major_plot <- renderPlot({
    #Graduation Proportion for target department over time
    req(c(input$select_major, input$select_plot))
    
    switch(
      input$select_plot,
      "status_by_major" = {
        if(input$select_major == "Entire department") {
          prop <- status_by_major |> 
            filter(acad_dept == input$select_dept) |> 
            count(status, term_year)
        } else {
          prop <- status_by_major |> 
            filter(acad_dept == input$select_dept & major == input$select_major)
        }

        prop |> 
          ggplot(aes(x = as.factor(term_year), fill = status)) +
          geom_bar(position = "fill") +
          labs(
           title = sprintf("Student status — %s", input$select_major),
            subtitle = "Retention and graduation, by academic year",
            x = "Academic year",
            y = "Proportion of students",
            fill = "Student status"
          ) +
          theme_minimal() +
          scale_y_continuous(labels = scales::percent) +
          scale_fill_manual(
            values = c(
              "Currently Enrolled" = "#533860", 
              "Graduated" = "#FFF42A", 
              "Unenrolled" = "#F05155"
            )
          ) +
          theme(
            plot.title = element_text(family = "Proxima Nova"),
            text = element_text(family = "Roboto Slab")
          )
      },
      "status_by_demographic" = {
        prop |> 
          ggplot(aes(x = as.factor(term_year), fill = status)) +
          geom_bar(position = "fill") +
          labs(
           title = sprintf("Student status — %s", input$select_major),
            subtitle = "Retention and graduation, by academic year",
            x = "Academic year",
            y = "Proportion of students",
            fill = "Student status"
          ) +
          theme_minimal() +
          scale_y_continuous(labels = scales::percent) +
          scale_fill_manual(
            values = c(
              "Currently Enrolled" = "#533860", 
              "Graduated" = "#FFF42A", 
              "Unenrolled" = "#F05155"
            )
          ) +
          theme(
            plot.title = element_text(family = "Proxima Nova"),
            text = element_text(family = "Roboto Slab")
          )
      }
    )
  })
}

shinyApp(ui, server)
