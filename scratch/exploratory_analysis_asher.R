library(tidyverse)
cleaned_data <- read_csv("data/clean/registrar_cleaned.csv")

graduation_data <- cleaned_data |> 
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
  right_join(cleaned_data, join_by(stc_person)) |> 
  mutate(graduated = coalesce(graduated, FALSE)) |> # if the student didn't graduate, they didn't graduate
  relocate(stc_person, graduated) |> 
  distinct(stc_person, .keep_all = TRUE)

View(graduation_data)
