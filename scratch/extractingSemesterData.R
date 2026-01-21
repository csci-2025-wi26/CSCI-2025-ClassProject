library(tidyverse)
library(arrow)
cleaned_data <- read_csv("data/processed/cleaned_data.csv")

cleaned_data <- cleaned_data |> 
  separate(student_course_sec_sec_term, into = c("year_digit", "term"), sep = "/") |> 
  mutate(
    year = 2000 + as.integer(year_digit),
    term_order = case_when(
      term == "SP" ~ 1,
      term == "SU" ~ 2,
      term == "FA" ~ 3,
      TRUE ~ NA_real_
    ),
    semester_index = year * 10 + term_order
  ) 

first_semester_grades <- cleaned_data |> 
  group_by(stc_person) |> 
  filter(semester_index == min(semester_index, na.rm = TRUE)) |>
  select(semester_index, stc_person, grade_numeric)

first_two_semesters <- cleaned_data |> 
  group_by(stc_person) |> 
  arrange(semester_index) |> 
  filter(row_number() <= 2) |> 
  ungroup() |> 
  head()