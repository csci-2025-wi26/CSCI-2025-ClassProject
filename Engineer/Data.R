library(tidyverse)

registrar_data <- read_csv('data/raw/registrar_data.csv')

clean_data <- registrar_data |>
  separate(stc_depts, into = c('first_major', 'second_major'), sep=',')

glimpse(clean_data)

clean_data |>
  select(students_xstu_grad_app_major) |>
  print(n = 50)

glimpse(clean_data19)
#twenty_enrollment <- registrar_data |>
#  filter(term_reporting_year == "2020")

#twentyone_enrollment <- registrar_data |>
#  filter(term_reporting_year == "2021")

#twentytwo_enrollment <- registrar_data |>
#  filter(term_reporting_year == "2022")


