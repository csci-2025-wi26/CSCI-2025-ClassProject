library(tidyverse)
library(arrow)
cleaned_data <- read_csv("data/processed/cleaned_data.csv")

extracting_sem_data <- cleaned_data |> 
  separate(student_course_sec_sec_term, into = c("year_digit", "term"), sep = "/") |> 
  mutate(
    year = 2000 + as.integer(year_digit),
    term_order = case_when(
      term == "SP" ~ 1,
      term == "SU" ~ 2,
      term == "FA" ~ 3,
      TRUE ~ NA_real_
    ),
    semester_index = year * 10 + term_order    #This line is creating a numeric column to sort by
  ) 

first_semester_grades_summary <- extracting_sem_data |> 
  group_by(stc_person) |> 
  filter(semester_index == min(semester_index, na.rm = TRUE)) |> #select the first semester; min of semester_index
  summarise(
    first_semester_mean_grade = mean(grade_numeric, na.rm = TRUE),
    first_semester_dfw = mean(dfw, na.rm = TRUE)
  )

first_two_semesters <- extracting_sem_data |> 
  group_by(stc_person) |> 
  arrange(semester_index) |> 
  filter(row_number() <= 2) |> 
  ungroup() |> 
  head()

