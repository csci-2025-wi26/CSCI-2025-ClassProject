library(tidyverse)
raw_data <- read_csv("data/raw/registrar_data.csv")
View(raw_data)
print(raw_data$race)
cleaned_data <- read_csv("data/clean/registrar_cleaned.csv")
View(cleaned_data)

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

#new work on determining graduation status
graduation_data <- cleaned_data |>
  select(
    stc_person,
    person_xper_grad_term,
    students_stu_active_majors,
    stc_acad_level,
    term_reporting_year,
    students_stu_class,
    students_xstu_grad_acad_year
  ) |>
  filter(
    students_xstu_grad_acad_year >= term_reporting_year - 1 & # term reporting year isn't past the spring term
      stc_acad_level == "UG" & # remove graduate students for now
      students_stu_active_majors != "NON" # remove non-degree seeking students
  ) |>
  mutate(graduated = )

View(graduation_data)

by_gender <- graduation_data |>
  group_by(race_ethnicity) |>
  mutate(
    prop_grad = mean(graduated),
    .after = graduated
  ) |>
  relocate(re, .after = prop_grad) |>
  arrange(race_ethnicity)


ggplot(by_gender, aes(race_ethnicity, prop_grad)) +
  geom_col()


student_year_summary <- cleaned_data %>%
  mutate(dept = str_extract(primary_major, "^[^,]+")) %>%
  group_by(stc_person, term_year) %>%
  summarise(
    dept = first(dept),
    graduated = any(!is.na(person_xper_grad_term)),
    .groups = "drop"
  )

target_dept <- "PSY" 
analysis_data %>%
  filter(dept == target_dept) %>%
  ggplot(aes(x = as.factor(term_year), fill = status)) +
  geom_bar(position = "fill") +
  labs(
    title = paste("Student Status for Department:", target_dept),
    subtitle = "Comparing retention and dropout rates by academic year",
    x = "Academic Year",
    y = "Number of Students",
    fill = "Status"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")