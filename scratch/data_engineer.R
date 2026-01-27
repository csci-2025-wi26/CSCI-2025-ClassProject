library(tidyverse)

data_raw <- read.csv("data/raw/registrar_data.csv")


clean_enrollments <- data_raw |>
  select(stc_person, stc_course_name, term_numeric, stc_sec_name, students_stu_class) |>
  rename(
    student_id = stc_person,
    term       = term_numeric,
    course_id  = stc_sec_name
  ) |>
  mutate(
    department = str_extract(
      str_to_upper(str_squish(stc_course_name)),
      "^[A-Z]+"
    )
    ,students_stu_class = str_remove(str_squish(students_stu_class), "^,")) |>
  drop_na(student_id, department, term, course_id) |>
  distinct()



major_path <- data_raw |>
  distinct(stc_person, students_stu_majors, students_xstu_grad_app_major) |>
  pivot_longer(c(students_stu_majors, students_xstu_grad_app_major), names_to="src", values_to="m") |>
  separate_rows(m, sep = ",") |>
  mutate(m = str_to_upper(str_squish(m))) |>
  filter(m != "") |>
  transmute(student_id = stc_person, major = m, step = if_else(src=="students_xstu_grad_app_major", 999L, 1L)) |>
  group_by(student_id, step) |>
  mutate(step = if_else(step==1L, row_number(), step)) |>
  ungroup() |>
  arrange(student_id, step)






enroll <- data_raw |>
  select(stc_person, stc_course_name, student_course_sec_stc_title, term_numeric) |>
  rename(
    student_id = stc_person,
    course_code = stc_course_name,
    course_title = student_course_sec_stc_title,
    term = term_numeric
  ) |>
  mutate(
    course_code  = str_to_upper(str_squish(as.character(course_code))),
    course_title = str_squish(as.character(course_title)),
    department   = str_extract(course_code, "^[A-Z]+")
  ) |>
  filter(!is.na(student_id), !is.na(term), !is.na(course_code), !is.na(department), !is.na(course_title)) |>
  distinct(student_id, department, term, course_code, course_title)


intro <- enroll |>
  filter(str_detect(str_to_lower(course_title), "intro|introduc"))


intro_with_continue <- intro |>
  left_join(enroll, by = c("student_id", "department"), suffix = c("_intro", "_any")) |>
  group_by(student_id, department, course_code_intro, course_title_intro, term_intro) |>
  summarise(
    continued = any(term_any > term_intro),
    .groups = "drop"
  )


intro_course_retention <- intro_with_continue |>
  group_by(department,
           intro_course_code = course_code_intro,
           intro_course_title = course_title_intro) |>
  summarise(
    n_students = n_distinct(student_id),
    n_continue = sum(continued),
    retention_rate = n_continue / n_students,
    .groups = "drop"
  ) |>
  filter(n_students >= 5) |>
  arrange(desc(retention_rate))





