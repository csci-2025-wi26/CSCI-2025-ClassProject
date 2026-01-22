library(tidyverse)
library(janitor)

raw_data <- read.csv("CSCI-2025-ClassProject/data/raw/registrar_data.csv") |>
  clean_names()

glimpse(raw_data)

cleaned_data <- raw_data |>
  mutate(across(where(is.character), ~na_if(.x, "NA")
                |> na_if("NONE"))
  ) |>
  mutate(
    race_ethnicity = str_remove(re, "^[0-9]+\\.\\s*"),
    gender = case_when(
      person_gender == "M" ~ "Male",
      person_gender == "F" ~ "Female",
      TRUE ~ "Other/Unknown"
    ),
    pell_recipient = if_else(pell == "Y", TRUE, FALSE)
  ) |>
  mutate(
    term_year = term_reporting_year,
    term_season = str_extract(student_course_sec_sec_term, "(FA|SP|SU)$"),
    term_val = case_when(
        term_season == "SP" ~ 0.0,
        term_season == "SU" ~ 0.1,
        term_season == "FA" ~ 0.2,
        TRUE ~ 0.0
    ),
    term_index = term_year + term_val
  ) |>
  mutate(primary_major = coalesce(students_stu_active_majors, stu_acad_programs))

# View(cleaned_data)
# # add a refined grad column
# grad_column <- cleaned_data |> 
#   mutate(
#     grad_year = { # need to handle double values; separate graduate level graduation date column?
#       if_else(
#         str_extract(person_xper_grad_term, "(FA|SP|WI|SU)") == "FA", 
#         2000 + parse_number(person_xper_grad_term), # fall term
#         2000 + parse_number(person_xper_grad_term) - 1 # spring, winter, and summer terms list academic year
#       )
#     }
#   )


write_csv(cleaned_data, "CSCI-2025-ClassProject/data/clean/registrar_cleaned.csv")

glimpse(cleaned_data)
