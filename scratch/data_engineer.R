library(tidyverse)

data_raw <- read.csv("data/raw/registrar_data.csv")


clean_enrollments <- data_raw |>
  select(stc_person, stc_course_name, term_numeric, stc_sec_name) |>
  rename(
    student_id = stc_person,
    term       = term_numeric,
    course_id  = stc_sec_name
  ) |>
  mutate(
    department = str_extract(
      str_to_upper(str_squish(stc_course_name)),
      "^[A-Z]+"
    )
  ) |>
  drop_na(student_id, department, term, course_id) |>
  distinct()



