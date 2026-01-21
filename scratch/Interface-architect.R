library(tidyverse)
library(DT)

# reg_data <- read_csv("data/raw/registrar_data.csv")

# glimpse(reg_data)

# department_counts <- reg_data |>
#   count(stc_depts)

# department_counts|>
#   print(n = 26)

# reg_data |>
#   group_by(stc_depts) |>
#   count(xstc_verified_lettr_grade)

# reg_data |>
#   group_by(stc_depts) |>
#   count(xstc_verified_lettr_grade) |>
#   filter(xstc_verified_lettr_grade == "F") |>
#   print(n = 23)

# reg_data |>
#   select(stc_depts, xstc_verified_lettr_grade)|>
#   filter(xstc_verified_lettr_grade == "F") |>
#   mutate(proportion_fail = xstc_verified_lettr_grade / count(stc_depts))

# reg_data |>
#   group_by(stc_depts)
#   mutate(proportion_fail = count())

# reg_data |>
#   count(students_stu_class)

#amount that failed and passed (TRUE = failed) that are not SRs
# reg_data |>
#   filter(students_stu_class != "SR")|>
#   select(students_stu_class, stc_depts, xstc_verified_lettr_grade) |>
#   count(xstc_verified_lettr_grade == "F")

# reg_data |>
#   glimpse()

plot_data <- reg_data |>
  filter(stc_depts == "MATPH") |>
  count(stc_person) |>
  count(n, name = "freq")

dep_class_count <- ggplot(plot_data, aes(x = factor(n), y = freq)) +
  geom_col(fill = "steelblue")




#beginning of app
library(shiny)
library(bslib)

ui <- fluidPage(
  titlePanel("Retention Group"),
  plotOutput("dep_class_count")
  
)

server <- function(input, output, session) {
  output$dep_class_count <- renderPlot(
    ggplot(plot_data, aes(x = factor(n), y = freq)) +
    geom_col(fill = "steelblue") +
    labs(x = "Number of classes", y = "count", title = "MAPS dept class retention")
  )

}

shinyApp(ui, server)