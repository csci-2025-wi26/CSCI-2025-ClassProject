library(tidyverse)
raw_data <- read_csv("data/raw/registrar_data.csv")

graduation_data <- raw_data |> 
  select(
    stc_person, 
    students_xstu_grad_acad_year, 
    students_stu_active_majors, 
    stc_acad_level, 
    term_reporting_year
  ) |> 
  filter(
    students_xstu_grad_acad_year >= term_reporting_year - 1 & # term reporting year isn't past the spring term
    stc_acad_level == "UG" & # remove graduate students for now
    students_stu_active_majors != "NON" # remove non-degree seeking students
  ) |>
  distinct(stc_person, .keep_all = TRUE) |> # unique students
  mutate(
    graduated = if_else(is.na(students_xstu_grad_acad_year), FALSE, TRUE)
  ) |> 
  select(stc_person, graduated)

graduation_data <- graduation_data |> 
  right_join(raw_data, join_by(stc_person)) |> 
  replace_na(graduated = FALSE) |> # if the student didn't graduate, they didn't graduate
  relocate(stc_person, graduated) |> 
  distinct(stc_person, .keep_all = TRUE)
