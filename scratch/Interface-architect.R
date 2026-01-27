library(tidyverse)
library(DT)

# reg_data <- read_csv("data/raw/registrar_data.csv")

library(shiny)
library(bslib)

re_dept_names <- sort(unique((reg_data$stc_depts)))
re_course_names <- sort(unique(reg_data$stc_course_name))

ui <- fluidPage(
  titlePanel("Retention Group"),
  selectInput("re_dept_single", "department", choices = re_dept_names),
  plotOutput("dep_class_count"),

  selectInput("re_class", "class", choices = re_course_names),
  selectInput("re_dept_double", "department", choices = re_dept_names),
  plotOutput("class_combo")

  
)
# put the selectInput as the dept name for the x variable and change the current plot to read off of reg_data
server <- function(input, output, session) {
  output$dep_class_count <- renderPlot({
    filtered_data <- reg_data |>
      filter(stc_depts == input$re_dept_single)|>
        count(stc_person) |>
        count(n, name = "freq")
    ggplot(filtered_data, aes(x = factor(n), y = freq)) +
    geom_col(fill = "steelblue") +
    labs(x = "Number of classes", y = "count", title = "Dept class retention")
})
  
    output$class_combo <- renderPlot({
      students_of_interest <- registrar_data |>
        filter(str_detect(stc_course_name, fixed(input$re_class))) |>
        pull(stc_person) |>
        unique()
      plot_data_less <- registrar_data |>
        filter(stc_person %in% students_of_interest,
        stc_depts == input$re_dept_double) |>
        count(stc_person) |>
        count(n, name = "freq")
      ggplot(plot_data_less, aes(x = factor(n), y = freq)) +
      geom_col(fill = "steelblue")
    })
}
shinyApp(ui, server)




# here we make a unique dataframe that has students that have take this class
# students_of_interest <- registrar_data |>
#   filter(str_detect(stc_course_name, fixed("BUS-101", ignore_case = TRUE))) |>
#   pull(stc_person) |>
#   unique()

# # here we identify the students and pull the ones in the corresponding dept
# plot_data_less <- registrar_data |>
#   filter(stc_person %in% students_of_interest, 
#          stc_depts == "BUACC") |>
#   count(stc_person) |>
#   count(n, name = "freq")

# ggplot(plot_data_less, aes(x = factor(n), y = freq)) +
#   geom_col(fill = "steelblue")