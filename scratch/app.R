library(shiny)
library(bslib)
library(shinycssloaders)
library(tidyverse)
library(vroom)
library(scales)

# outcomes variables
outcomes_data <- vroom("../data/clean/registrar_cleaned.csv")

outcome_plots <- list(
  "By major" = c(
    "Student enrollment status" = "status_by_major",
    "Years to graduation" = "graduation_by_major",
    "Major change" = "switch_by_major"
  ),
  "By demographic" = c(
    "Student enrollment status" = "status_by_demographic",
    "Years to graduation" = "graduation_by_demographic",
    "Major change" = "switch_by_demographic"
  )
)

dept <- outcomes_data |>
  distinct(stc_person, .keep_all = TRUE) |>
  select(stc_depts) |>
  mutate(stc_depts = str_match(stc_depts, "(\\w+),?")[, 2]) |> # get first value
  distinct(stc_depts) |>
  pull() |>
  sort()

majors_by_dept <- outcomes_data |>
  distinct(stc_person, .keep_all = TRUE) |>
  select(
    acad_dept = stu_acad_programs,
    major = students_xstu_grad_app_major.x
  ) |>
  mutate(across(everything(), ~ str_match(., "(\\w+),?")[, 2])) |> # get first value
  filter(!(major %in% c("NON", "NONGR", "OPEN")) & acad_dept %in% dept) |>
  group_by(acad_dept) |>
  distinct(major) |>
  mutate(
    acad_dept = str_extract(acad_dept, "^[^,]+"),
    major = str_extract(major, "^[^,]+")
  ) |>
  filter(!(major %in% c("NON", "OPEN")) & acad_dept %in% dept) |>
  distinct(acad_dept, major) |>
  arrange(acad_dept)

demographic_list <- outcomes_data |>
  select(race_ethnicity, gender, pell_recipient) |>
  names()

titled_demographic_list <- outcomes_data |>
  select(race_ethnicity, gender, pell_recipient) |>
  names() |>
  str_replace("race_ethnicity", "race/ethnicity") |>
  str_replace("_", " ") |>
  str_to_title()

filtered_data <- outcomes_data |>
  distinct(stc_person, .keep_all = TRUE) |>
  select(
    # precise_years,
    primary_major,
    term_year,
    status,
    stc_depts,
    switched_majors,
    all_of(demographic_list)
  ) |>
  mutate(
    acad_dept = str_match(stc_depts, "(\\w+),?")[, 2],
    major = str_extract(primary_major, "^[^,]+"),
    status = if_else(status == "Dropped", "Unenrolled", status),
    status = fct(
      status,
      levels = c("Currently Enrolled", "Graduated", "Unenrolled")
    )
  )

status_by <- outcomes_data |>
  distinct(stc_person, .keep_all = TRUE) |>
  select(
    primary_major,
    term_year,
    status,
    stc_depts,
    all_of(demographic_list)
  ) |>
  mutate(
    acad_dept = str_match(stc_depts, "(\\w+),?")[, 2],
    major = str_extract(primary_major, "^[^,]+"),
    status = if_else(status == "Dropped", "Unenrolled", status),
    status = fct(
      status,
      levels = c("Currently Enrolled", "Graduated", "Unenrolled")
    )
  )

# graduation_by <- outcomes_data |>
#   distinct(stc_person, .keep_all = TRUE) |>
#   select(precise_years, primary_major, term_year, stc_depts, all_of(demographic_list)) |>
#   filter(precise_years > 3) |>
#   mutate(precise_years = ceiling(precise_years))

demographic_list <- setNames(as.list(demographic_list), titled_demographic_list)

ui <- fluidPage(
  titlePanel("CSCI2025 Class Project Dashboard"),
  navlistPanel(
    id = "tabset",
    # This 'Home' tab is an example of how to insert your own inputs/outputs into the boilerplate
    tabPanel(
      "Home", # tabPanel display title
      h3("Welcome!"), # it's all components from here - add as many as you like!
      p(
        "This serves as the base of what will become our collective Shiny App.\n This home tab includes some example code."
      ),
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
      fluidRow(column(
        12,
        h3("Data Selection Criteria"), # Using h3 for a clean header
        p(
          "Please use the dropdowns below to choose which graph to see. 
          You can choose which metric to plot, then further filter by department 
          as well as each major within that department.",
          style = "color: #666; font-style: italic;"
        )
      )),
      fluidRow(
        column(
          3,
          selectInput(
            "select_plot",
            "Metric to Plot",
            choices = outcome_plots,
            selected = outcome_plots[names(outcome_plots)[1]][1]
          )
        ),
        column(
          3,
          selectInput(
            "select_dept",
            "Department",
            choices = dept,
            selected = dept[1]
          )
        ),
        column(
          3,
          uiOutput("select_major_ui"),
          uiOutput("select_demographic_ui")
        )
      ),
      fluidRow(
        column(
          12,
          p(
            "The below plot demonstrates the portportion of students in each enrollment status per year.",
            style = "font-size: 16px; color: #2c3e50;"
          ),
          plotOutput("dept_major_plot")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  ### Home Tab ###
  home_ex_reactive <- reactive({
    # Example reactive component
    req(input$home_ex_text_in)
    str_length(input$home_ex_text_in)
  })
  output$home_ex_text_out <- renderText({
    # Example output functionality
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
  demo_col <- reactive({
    req(input$select_demographic)
    input$select_demographic
  })

  demo_name <- reactive({
    req(input$select_demographic)
    names(demographic_list)[demographic_list == demo_col()]
  })

  # if plot with major, render major selectors
  output$select_major_ui <- renderUI({
    req(input$select_dept)

    if (!str_detect(input$select_plot, "major")) {
      return(NULL)
    }

    if (str_detect(input$select_plot, "switch_by_major")) {
      choices <- "Entire department"
    } else {
      dept_majors <- majors_by_dept |>
        filter(acad_dept == input$select_dept) |>
        pull(major)

      choices <- c("Entire department", dept_majors)
    }

    selectInput(
      "select_major",
      "Major",
      choices = choices,
      selected = "Entire department"
    )
  })

  output$select_demographic_ui <- renderUI({
    req(input$select_dept)

    if (!str_detect(input$select_plot, "demographic")) {
      return(NULL)
    }

    selectInput(
      "select_demographic",
      "Demographic",
      choices = demographic_list,
      selected = titled_demographic_list[1]
    )
  })

  output$dept_major_plot <- renderPlot({
    #Graduation Proportion for target department over time
    req(input$select_plot)

    switch(
      input$select_plot,
      "status_by_major" = {
        req(input$select_major)
        if (input$select_major == "Entire department") {
          prop <- status_by |>
            filter(acad_dept == input$select_dept)
        } else {
          prop <- status_by |>
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
              "Unenrolled" = "#494C5A"
            )
          ) +
          theme(
            plot.title = element_text(family = "Proxima Nova"),
            text = element_text(family = "Roboto Slab")
          )
      },
      "switch_by_major" = {
        req(input$select_major)

        prop <- filtered_data |>
          filter(acad_dept == input$select_dept) |>
          mutate(
            switched_majors = fct(
              as.character(switched_majors),
              levels = c("TRUE", "FALSE")
            )
          )

        prop |>
          ggplot(aes(x = as.factor(major), fill = switched_majors)) +
          geom_bar(position = "fill") +
          labs(
            title = sprintf("Major change — %s", input$select_dept),
            subtitle = "Student major change, by major",
            x = "Major",
            y = "Proportion of students",
            fill = "Major change?"
          ) +
          theme_minimal() +
          scale_y_continuous(labels = scales::percent) +
          scale_fill_manual(
            values = c(
              "FALSE" = "#533860",
              "TRUE" = "#FFF42A"
            ),
            labels = c(
              "TRUE" = "Major change",
              "FALSE" = "No major change"
            )
          ) +
          theme(
            plot.title = element_text(family = "Proxima Nova"),
            text = element_text(family = "Roboto Slab")
          )
      },
      # "graduation_by_major" = {
      #   req(input$select_major)
      #   if(input$select_major == "Entire department") {
      #     prop <- status_by |>
      #       filter(acad_dept == input$select_dept)
      #   } else {
      #     prop <- status_by |>
      #       filter(acad_dept == input$select_dept & major == input$select_major)
      #   }

      #   prop |>
      #     ggplot(aes(x = fct(precise_years))) +
      #     geom_bar(fill = "#533860") +
      #     labs(
      #      title = sprintf("Years to Graduation — %s", input$select_major),
      #       subtitle = "Retention and graduation, by years to graduate",
      #       x = "Years to graduate",
      #       y = "# of students"
      #     ) +
      #     theme_minimal() +
      #     theme(
      #       plot.title = element_text(family = "Proxima Nova"),
      #       text = element_text(family = "Roboto Slab")
      #     )
      # },
      "status_by_demographic" = {
        req(input$select_demographic)
        prop <- status_by |>
          filter(acad_dept == input$select_dept)

        prop |>
          ggplot(aes(x = .data[[demo_col()]], fill = status)) +
          geom_bar(position = "fill") +
          labs(
            title = sprintf("Student status — %s", demo_name()),
            subtitle = sprintf("Retention & graduation — %s", demo_name()),
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
              "Unenrolled" = "#494C5A"
            )
          ) +
          theme(
            plot.title = element_text(family = "Proxima Nova"),
            text = element_text(family = "Roboto Slab")
          )
      },
      "yr_to_grad_by_major" = {
        prop |>
          ggplot(aes(x = years_to_grad, fill = status)) +
          geom_bar() +
          labs(
            title = "Years to Graduation by Major",
            x = "Years to Graduate",
            y = "Number of Students",
            fill = "Student status"
          ) +
          theme_minimal() +
          scale_y_continuous(labels = scales::percent) +
          scale_fill_manual(
            values = c(
              "Currently Enrolled" = "#533860",
              "Graduated" = "#FFF42A",
              "Unenrolled" = "#494C5A"
            )
          ) +
          theme(
            plot.title = element_text(family = "Proxima Nova"),
            text = element_text(family = "Roboto Slab")
          )
        # },
        # "graduation_by_demographic" = {
        #   req(input$select_demographic)
        #   prop <- status_by |>
        #     filter(acad_dept == input$select_dept)

        #   prop |>
        #     ggplot(aes(x = .data[[demo_col()]], fill = fct(precise_years))) +
        #     geom_bar(position = "fill") +
        #     labs(
        #      title = sprintf("Years to Graduation — %s", demo_name()),
        #       subtitle = sprintf("Years to graduate, by %s", demo_name()),
        #       x = "Years to graduate",
        #       y = "Proporation of students"
        #     ) +
        #     theme_minimal() +
        #     scale_y_continuous(labels = scales::percent) +
        #     theme(
        #       plot.title = element_text(family = "Proxima Nova"),
        #       text = element_text(family = "Roboto Slab")
        #     )
      },
      "switch_by_demographic" = {
        req(input$select_demographic)

        prop <- filtered_data |>
          filter(acad_dept == input$select_dept) |>
          mutate(
            switched_majors = fct(
              as.character(switched_majors),
              levels = c("TRUE", "FALSE")
            )
          )

        prop |>
          ggplot(aes(x = .data[[demo_col()]], fill = switched_majors)) +
          geom_bar(position = "fill") +
          labs(
            title = sprintf("Major change — %s", demo_name()),
            subtitle = sprintf("Major change — %s", demo_name()),
            x = demo_name(),
            y = "Proportion of students",
            fill = "Major change?"
          ) +
          theme_minimal() +
          scale_y_continuous(labels = scales::percent) +
          scale_fill_manual(
            values = c(
              "FALSE" = "#533860",
              "TRUE" = "#FFF42A"
            ),
            labels = c(
              "TRUE" = "Major changed",
              "FALSE" = "No major change"
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
