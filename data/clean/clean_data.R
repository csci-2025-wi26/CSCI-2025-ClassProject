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
  mutate( # students_xstu_grad_app_major align with coding in students_stu_active_majors and students_stu_majors
    students_xstu_grad_app_major = if_else(
      students_xstu_grad_app_major == "BUSMDM", 
      "BUMDM", 
      students_xstu_grad_app_major
    ),
    students_xstu_grad_app_major = if_else(
      str_detect(students_xstu_grad_app_major, "\\."),
      str_remove_all(students_xstu_grad_app_major, "\\.B\\w"),
      students_xstu_grad_app_major
    ),
    students_xstu_grad_app_major = if_else(
      str_detect(students_xstu_grad_app_major, "�"),
      str_replace_all(students_xstu_grad_app_major, "�", ","),
      students_xstu_grad_app_major
    ),
    across(
      c(students_xstu_grad_app_major, students_stu_majors),
      ~ if_else(. == "ACCT", "ACC", .)
    )
  ) |> 
  select(-all_of(excluded_cols))

# *** LOOK AT THIS ***
setdiff(cleaned_data$students_xstu_grad_app_major, cleaned_data$students_stu_majors)

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
  )

cleaned_data <- cleaned_data |>
  mutate(
    gender = case_when( # when visualizing, use male/female/other
      person_gender_identity == "TRANSGEN" ~ "Transgender",
      person_gender_identity == "AGEND" ~ "Agender",
      person_gender_identity == "BIGEND" ~ "Bigender",
      person_gender_identity == "POLYGEND" ~ "Polygender",
      person_gender_identity == "GENFLU" ~ "Genderfluid",
      TRUE ~ gender
    )
  ) |>
  group_by(stc_person) |>
  mutate(
    start_term_index = min(term_index, na.rm = TRUE),
    ever_graduated = any(!is.na(person_xper_grad_term)),
    years_to_grad = if_else(
      !is.na(person_xper_grad_term),
      term_index - start_term_index,
      NA_real_
    )
  ) |>
  mutate(
    has_next_year = any(term_year == (term_year + 1)),
    status = case_when(
      !is.na(person_xper_grad_term) ~ "Graduated",
      term_year == max(cleaned_data$term_year) ~ "Currently Enrolled",
      !ever_graduated & !has_next_year ~ "Dropped"
    )
  ) |>
  ungroup()

cleaned_data <- cleaned_data |>
  group_by(stc_person) |>
  mutate(classes_taken = n())

major_switched <- cleaned_data |> #col to look at major switching
  select(
    stc_person,
    students_stu_majors,
    students_xstu_grad_app_major
  ) |> 
  distinct(stc_person, .keep_all = TRUE) |>
  mutate(
    majors = if_else( # remove NON and OPEN majors
      str_detect(students_stu_majors, ","), 
      str_remove_all(students_stu_majors, ",?NON,?|,?OPEN,?"), 
      NA
    ),
    dropped_majors_list = map2_chr( # list of dropped majors, separated by ", "
      majors,
      students_xstu_grad_app_major,
      ~ {
        if (is.na(.x) || is.na(.y)) {
          return(NA_character_)
        }

        applied <- str_split(.x, ",", simplify = TRUE)
        current <- str_split(.y, ",", simplify = TRUE)
        dropped <- setdiff(applied, current)

        if_else(setequal(applied, current), NA, str_flatten_comma(dropped))
      }
    ),
    switched_majors = if_else(is.na(dropped_majors_list), FALSE, TRUE) # lgl, did switch majors?
    # .keep = "unused"
  ) |> 
  arrange(dropped_majors_list)

cleaned_data <- cleaned_data |> 
  left_join(major_switched, join_by(stc_person))

glimpse(cleaned_data)

write_csv(
  cleaned_data,
  "data/clean/registrar_cleaned.csv"
)

glimpse(cleaned_data)
