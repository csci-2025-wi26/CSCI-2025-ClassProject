library(tidyverse)

data_raw <- read.csv("data/raw/registrar_data.csv")


clean_enrollments <- data_raw |>
  select(stc_person, stc_depts, term_numeric, stc_sec_name) |>
  rename(
    student_id = stc_person,
    department = stc_depts,
    term       = term_numeric,
    course_id  = stc_sec_name
  ) |>
  mutate(department = str_to_upper(str_squish(department))) |>
  drop_na() |>
  distinct()




