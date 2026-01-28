library(tidyverse)
library(plotly)
library(bslib)
library(scales)
library(extrafont)

registrar <- read_csv("data/raw/cleaned_registrar_data.csv")

#registrar_clean <- registrar |>
  #mutate(race_count = str_count(person_per_races, ",") + 1) |>
  #separate_longer_delim(students_stu_majors , delim = ',') |>
  #separate_longer_delim(person_per_races , delim = ',') |> 
  #mutate(students_stu_majors  = trimws(students_stu_majors )) |>
  #mutate(person_per_races = trimws(person_per_races))
  #group_by(stc_person, term_reporting_year) |>
  #mutate(major = first(students_stu_majors)) |> 
  #mutate(student_weight = 1 / race_count)

library(tidyverse)

#registrar_weighted <- registrar |>
 # mutate(race_count = str_count(person_per_races, ",") + 1) |>
  #separate_longer_delim(person_per_races, delim = ",") |>
  #mutate(person_per_races = trimws(person_per_races)) |>
  #mutate(student_weight = 1 / race_count) |> 
  #group_by(stc_person, term_reporting_year)

registrar_weighted <- registrar |>
  # This removes the duplicate rows caused by taking multiple classes
  distinct(stc_person, term_reporting_year, .keep_all = TRUE) |> 

  mutate(race_count = str_count(person_per_races, ",") + 1) |>
  separate_longer_delim(person_per_races, delim = ",") |>
  mutate(person_per_races = trimws(person_per_races)) |>
  mutate(student_weight = 1 / race_count) |> 
  group_by(stc_person, term_reporting_year)

major_CSI <- registrar_weighted |>
  filter(students_xstu_grad_app_major == "CSCI")


ggplot(data = major_CSI, aes(x = term_reporting_year, y = student_weight)) +
  geom_col() +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Nova Proxima", face = "bold"),
    axis.title = element_text(family = "Roboto Slab")) +
  labs(
    title = "Student Enrollment Per Year by Major",
    x = "Reporting Year",
    y = "Count"
  ) +
  scale_y_continuous(labels = label_comma())
  
  
