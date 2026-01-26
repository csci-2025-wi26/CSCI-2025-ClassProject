library(tidyverse)
library(stringr)

raw_data <- read_csv("data/raw/registrar_data.csv")
mapping_raw <- read_csv("data/raw/major-department_correlation.csv")

major_to_dept_lookup <- mapping_raw |>
  separate_rows(Code, sep = "/") |>
  mutate(Code = str_trim(Code)) |>
  select(Code, Department) |>
  deframe()

# DEPARTMENT RETENTION DATA

process_journey <- function(major_string) {
  if (is.na(major_string)) return(NULL)
  parts <- str_split(major_string, ",")[[1]] |> str_trim()
  parts <- rev(parts)
  parts <- parts[!parts %in% c("OPEN", "NON", "NONGR")]
  if (length(parts) == 0) return(NULL)
  dept_parts <- map_chr(parts, ~ {
    dept <- major_to_dept_lookup[.x]
    if (is.na(dept)) return(.x) else return(dept)
  })
  cleaned <- dept_parts[1]
  if (length(dept_parts) > 1) {
    for (i in 2:length(dept_parts)) {
      if (dept_parts[i] != dept_parts[i-1]) {
        cleaned <- c(cleaned, dept_parts[i])
      }
    }
  }
  return(cleaned)
}

retention_data <- raw_data |>
  group_by(stc_person) |>
  filter(term_numeric == max(term_numeric)) |>
  slice(1) |>
  ungroup() |>
  mutate(journey = map(students_stu_majors, process_journey)) |>
  filter(!map_lgl(journey, is.null)) |>
  mutate(transitions = map(journey, ~ {
    n <- length(.x)
    tibble(dept = .x, did_shift = c(rep(TRUE, n - 1), FALSE))
  })) |>
  select(stc_person, transitions) |>
  unnest(transitions) |>
  group_by(dept) |>
  summarise(total_students = n(), shifters = sum(did_shift), .groups = 'drop') |>
  mutate(retention_rate = (1 - (shifters / total_students)) * 100) |>
  filter(total_students >= 5) |>
  arrange(desc(retention_rate))

# INTRO COURSE RETENTION DATA

enroll <- raw_data |>
  select(stc_person, stc_course_name, student_course_sec_stc_title, term_numeric) |>
  rename(student_id = stc_person, course_code = stc_course_name, course_title = student_course_sec_stc_title, term = term_numeric) |>
  mutate(
    course_code  = str_to_upper(str_squish(as.character(course_code))),
    course_title = str_squish(as.character(course_title)),
    # CHANGED: Named this 'dept_code' instead of 'department' to match the filter below
    dept_code    = str_extract(course_code, "^[A-Z]+"),
    course_num   = as.numeric(str_extract(course_code, "\\d+"))
  ) |>
  # Now this filter will work because dept_code actually exists
  filter(!is.na(student_id), !is.na(term), !is.na(course_code), !is.na(dept_code))

# Keywords OR 100-level
intro_identify <- enroll |>
  filter(course_num >= 100 & course_num <= 199) |>
  filter(str_detect(str_to_lower(course_title), "intro|foundations|principles|general")) |>
  group_by(dept_code) |>
  filter(n_distinct(student_id) == max(n_distinct(student_id))) |>
  slice(1) |>
  select(dept_code, intro_code = course_code) |>
  ungroup()

intro_enrollments <- enroll |>
  inner_join(intro_identify, by = c("dept_code", "course_code" = "intro_code"))

future_enrollments <- enroll |>
  select(student_id, dept_code, future_term = term)

intro_retention_data <- intro_enrollments |>
  left_join(future_enrollments, by = c("student_id", "dept_code")) |>
  group_by(student_id, dept_code, term) |>
  summarise(continued = any(future_term > term), .groups = "drop") |>
  group_by(dept_code) |>
  summarise(
    n_students = n_distinct(student_id),
    n_continue = sum(continued),
    retention_rate = (n_continue / n_students) * 100,
    .groups = "drop"
  ) |>
  filter(n_students >= 5) |>
  arrange(desc(retention_rate))