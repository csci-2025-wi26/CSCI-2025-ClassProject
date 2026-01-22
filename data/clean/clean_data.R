library(tidyverse)
library(janitor)

raw_data <- read.csv("data/raw/registrar_data.csv") |>
  clean_names()

glimpse(raw_data)
excluded_cols <- c(
  "person_xper_chosen_last_name",
  "person_xper_chosen_nick_first_name",
  "student_course_sec_xscs_stc_current_status",
  "person_xper_full_chosen_nick_name",
  "students_xstu_aci_email",
  "person_xper_pr1_email",
  "person_xper_pr2_email",
  "crs_no",
  "re",
  "minors_minors_desc",
  "students_stu_active_minors"
)
cleaned_data <- raw_data |>
  mutate(across(where(is.character), ~ na_if(.x, "NA") |> na_if("NONE"))) |>
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
  mutate(
    primary_major = coalesce(students_stu_active_majors, stu_acad_programs)
  ) |>
  select(-all_of(excluded_cols))

# add a refined grad column
cleaned_data <- cleaned_data |>
  mutate(
    grad_year = {
      if_else(
        # if NA, check grad_acad_year column
        is.na(person_xper_grad_term),
        if_else(
          # if grad_acad_year also NA, NA, else replace with value
          is.na(students_xstu_grad_acad_year),
          NA,
          students_xstu_grad_acad_year
        ),
        if_else(
          # extract year from grad_term
          str_extract(person_xper_grad_term, "(FA|SP|WI|SU)") == "FA", # extracts first one, UG, not grad
          2000 + parse_number(person_xper_grad_term), # fall term
          2000 + parse_number(person_xper_grad_term) - 1 # spring, winter, and summer terms list academic year
        )
      )
    }
  ) |>
  select(stc_person, grad_year) |>
  distinct(stc_person, .keep_all = TRUE) |>
  right_join(cleaned_data, join_by(stc_person))

write_csv(
  cleaned_data,
  "data/clean/registrar_cleaned.csv"
)

glimpse(cleaned_data)
