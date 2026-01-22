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