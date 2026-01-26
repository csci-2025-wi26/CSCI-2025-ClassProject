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
            majors == "BUSMDM" ~ "BUMDM",
            majors == "ACCT"   ~ "ACC",
            majors == "BIOC"   ~ "BIOCH",
            majors == "CSMA"   ~ "CSMAS",
            majors == "FINC"   ~ "FIN",
            TRUE ~ majors
          )

          paste(sort(majors), collapse = ",")
        })
    )
  ) |>
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
  mutate(
    first_major = first(primary_major, order_by = term_index),
    last_major  = last(primary_major, order_by = term_index),
    switched_majors = first_major != last_major
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
    switched_majors = if_else(is.na(dropped_majors_list), FALSE, TRUE) # lgl, did switch majors?
    # .keep = "unused"
  ) |> 
  arrange(dropped_majors_list)

cleaned_data <- cleaned_data |> 
  left_join(major_switched, join_by(stc_person))

write_csv(
  cleaned_data,
  "data/clean/registrar_cleaned.csv"
)

by_gender <- cleaned_data |>
  group_by(race_ethnicity) |>
  filter(status == "Dropped" | status == "Currently Enrolled") |> 
  mutate(
    prop_grad = mean(if_else(status == "Dropped", 0, 1)),
    .after = status
  ) |>
  relocate(race_ethnicity, .after = prop_grad) |>
  arrange(race_ethnicity) |>
  mutate(
    race_ethnicity = factor(
      race_ethnicity,
      levels = sort(unique(race_ethnicity))
    )
  ) |>
  mutate(race_ethnicity = fct_rev(race_ethnicity))

#Proportion of graduated students by race/ethnicity bar chart
ggplot(by_gender, aes(race_ethnicity, prop_grad)) +
  geom_col(fill = "#533860") +
  coord_flip() +
  labs(
    title = "Proportion graduated — Race / Ethnicity",
    subtitle = "Graduation rate by race and ethnicity",
    x = "Race / ethnicity",
    y = "Share of students (proportion)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Proxima Nova"),
    text = element_text(family = "Roboto Slab")
  )

student_year_summary <- cleaned_data |> 
  mutate(dept = str_extract(primary_major, "^[^,]+")) |> 
  group_by(stc_person, term_year) |> 
  mutate(
    dept = first(dept),
    .groups = "drop"
  )

#Graduation Proportion for target department over time
target_dept <- "PSY" # we can handle select for department over time
student_year_summary |> 
  filter(status != "Currently Enrolled" & dept == target_dept) |> 
  ggplot(aes(x = as.factor(term_year), fill = status)) +
  geom_bar(position = "fill") +
  labs(
    title = sprintf("Student status — %s", target_dept),
    subtitle = "Retention and graduation, by academic year",
    x = "Academic year",
    y = "Share of students (proportion)",
    fill = "Graduation status"
  ) +
  theme_minimal() +
  scale_fill_manual(
    values = c("Dropped" = "#533860", "Graduated" = "#FFF42A"),
    labels = c("Dropped", "Graduated")
  ) +
  theme(
    plot.title = element_text(family = "Proxima Nova"),
    text = element_text(family = "Roboto Slab")
  )

graduation_year_data <- cleaned_data |>
  group_by(stc_person) |>
  summarize(
    final_grad_year = first(grad_year),
    entry_year      = min(term_year, na.rm = TRUE),
    
    years_to_grad   = final_grad_year - entry_year,
    
    years_to_grad   = if_else(years_to_grad < 0, 0, years_to_grad),
    
    precise_years   = if_else(!is.na(final_grad_year), 
                            first(grad_year) - first(start_term_index), 
                            NA_real_),
    precise_years   = if_else(precise_years < 0, 0, precise_years),
    
    grad_speed_category = case_when(
      years_to_grad <= 2 ~ "Transfer (0-2 yrs)",
      years_to_grad == 3 ~ "Early Grad (3 yrs)",
      years_to_grad == 4 ~ "Traditional (4 yrs)",
      years_to_grad > 4  ~ "Extended (5+ yrs)"
    ),
    
    race   = first(race_ethnicity),
    gender = first(gender),
    .groups = "drop"
  ) |> 
  filter(!is.na(years_to_grad))

glimpse(graduation_year_data)