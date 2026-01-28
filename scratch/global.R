library(tidyverse)
library(janitor)

# CLEAN DATA
raw_data <- read.csv("data/raw/registrar_data.csv") |>
  clean_names()

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
  mutate(
    primary_major = coalesce(students_xstu_grad_app_major, primary_major),
    students_stu_majors = coalesce(students_stu_majors,
                                  students_xstu_grad_app_major)
  ) |>
  mutate(
    across(c(primary_major,
            students_xstu_grad_app_major,
            students_stu_majors), ~
      .x |>
        str_replace_all("\uFFFD", ",") |>
        str_replace_all("[[:space:]\u00A0]", "") |>   
        str_remove_all("\\.B\\w") |>         
        str_split(",") |>
        map_chr(~ {
          majors <- .x

          majors <- case_when(
            majors %in% c("BUSMDM", "BUMDM") ~ "BUMDM",
            majors %in% c("ACCT", "ACC")     ~ "ACC",
            majors %in% c("BIOC", "BIOCH")   ~ "BIOCH",
            majors %in% c("CSMA", "CSMAS")   ~ "CSMAS",
            majors %in% c("FINC", "FIN")     ~ "FIN",
            majors %in% c("ARTDS", "ARTDES") ~ "ARTDES", 
            majors %in% c("MATPH", "MATPHY") ~ "MATPHY",
            majors == "IPE"                  ~ "IPEC",
            majors == "HEA"                  ~ "HEALTH", 
            majors == "MATCS"                ~ "MAT",    
            TRUE ~ majors
          )

          majors <- unique(majors)
          paste(sort(majors), collapse = ",")
        })
    )
  ) |>
  mutate(stu_acad_programs = case_when(
    stu_acad_programs == "BUSMDM" ~ "BUMDM",
    stu_acad_programs == "BIOMD"  ~ "BIO",
    TRUE ~ stu_acad_programs
  )) |> 
  select(-all_of(excluded_cols))


cleaned_data <- cleaned_data |>
  mutate(
    grad_year = if_else(
      is.na(person_xper_grad_term),
      if_else(is.na(students_xstu_grad_acad_year), NA_real_, as.numeric(students_xstu_grad_acad_year)),
      if_else(
        str_extract(person_xper_grad_term, "(FA|SP|WI|SU)") == "FA",
        2000 + parse_number(person_xper_grad_term),
        2000 + parse_number(person_xper_grad_term) - 1
      )
    )
  )

# Demographic and Graduation Status Logic
cleaned_data <- cleaned_data |>
  mutate(
    gender = case_when( 
      person_gender_identity == "TRANSGEN" ~ "Transgender",
      person_gender_identity == "AGEND"    ~ "Agender",
      person_gender_identity == "BIGEND"   ~ "Bigender",
      person_gender_identity == "POLYGEND" ~ "Polygender",
      person_gender_identity == "GENFLU"   ~ "Genderfluid",
      TRUE ~ gender # Keeps the "Male"/"Female" from your previous step
    )
  ) |>
  group_by(stc_person) |>
  mutate(
    start_term_index = min(term_index, na.rm = TRUE),
    ever_graduated = any(!is.na(person_xper_grad_term)),
    years_to_grad = if_else(ever_graduated, max(term_index) - start_term_index, NA_real_),
    has_next_year = any(term_year %in% (term_year + 1)),
    status = case_when(
      ever_graduated ~ "Graduated",
      term_year == max(raw_data$term_reporting_year, na.rm = TRUE) ~ "Currently Enrolled",
      TRUE ~ "Dropped"
    ),
    classes_taken = n()
  ) |>
  ungroup()

cleaned_data <- cleaned_data |>
  group_by(stc_person) |>
  mutate(classes_taken = n()) |> 
  ungroup()

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
    switched_majors = if_else(is.na(dropped_majors_list), FALSE, TRUE), # lgl, did switch majors?
    .keep = "unused"
  )

cleaned_data <- cleaned_data |> 
  left_join(major_switched, join_by(stc_person))

graduation_year_data <- cleaned_data |>
  group_by(stc_person) |>
  mutate(
    entry_year      = min(term_reporting_year, na.rm = TRUE),
    
    years_to_grad   = grad_year - entry_year + 1,
    
    years_to_grad   = if_else(years_to_grad < 0, NA_real_, years_to_grad),
    
    precise_years   = if_else(!is.na(grad_year), 
                            first(grad_year) - first(start_term_index), 
                            NA_real_),
    precise_years   = if_else(precise_years < 0, NA_real_, precise_years)
  ) |> 
  select(stc_person, years_to_grad, precise_years)

cleaned_data <- cleaned_data |> 
  left_join(graduation_year_data, join_by(stc_person))

write_csv(
  cleaned_data,
  "data/clean/registrar_cleaned.csv"
)
