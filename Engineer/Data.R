library(tidyverse)
library(dplyr)

enrollment_registrar_data <- read_csv('data/raw/registrar_data.csv')

enrollment_clean_data <- enrollment_registrar_data |>
  separate_longer_delim(students_stu_majors, delim = ',') |>
  separate_longer_delim(person_per_races, delim = ',') |>
  mutate(students_stu_majors  = trimws(students_stu_majors)) |>
  group_by(stc_person, term_reporting_year) |>
  mutate(major = first(students_stu_majors))

glimpse(enrollment_clean_data)