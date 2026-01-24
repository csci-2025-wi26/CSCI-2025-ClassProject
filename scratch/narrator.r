library(tidyverse)
registrar_data <- read_csv("data/raw/registrar_data.csv")


plot_data <- registrar_data |>
  filter(stc_depts == "BUACC") |>
  count(stc_person) |>
  count(n, name = "freq")

ggplot(plot_data, aes(x = factor(n), y = freq)) +
  geom_col(fill = "steelblue")



students_of_interest <- registrar_data |>
  filter(str_detect(stc_course_name, fixed("BUS-101", ignore_case = TRUE))) |>
  pull(stc_person) |>
  unique()


plot_data_less <- registrar_data |>
  filter(stc_person %in% students_of_interest, 
         stc_depts == "BUACC") |>
  count(stc_person) |>
  count(n, name = "freq")

ggplot(plot_data_less, aes(x = factor(n), y = freq)) +
  geom_col(fill = "steelblue")
