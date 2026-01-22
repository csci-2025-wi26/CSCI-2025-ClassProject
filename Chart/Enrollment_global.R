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

registrar_weighted <- registrar |>
  mutate(race_count = str_count(person_per_races, ",") + 1) |>
  separate_longer_delim(person_per_races, delim = ",") |>
  mutate(person_per_races = trimws(person_per_races)) |>
  mutate(student_weight = 1 / race_count) |> 
  group_by(stc_person, term_reporting_year)

race_labels <- c(
  "AN" = "American Indian", 
  "AS" = "Asian",
  "BL" = "Black", 
  "HP" = "Hawaiian or Pacific Islander",
  "ME" = "Mexican",
  "WH" = "White",
  "NA" = "Not Listed"
)

race_map <- c(
  "WH" = "#6A5ACD",
  "ME" = "#000080",
  "AS" = "#00BFFF",
  "BL" = "#E2725B",
  "AN" = "#228B22",
  "HP" = "#008080",
  "NA" = "#FF7F50"
)

ggplot(data = registrar_weighted, aes(x = term_reporting_year, y = student_weight, fill = fct_rev(fct_infreq((person_per_races))))) +
  geom_col() +
  scale_fill_manual(
  values = race_map,
  labels = race_labels) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Nova Proxima", face = "bold"),
    axis.title = element_text(family = "Roboto Slab")) +
  labs(
    title = "Student Enrollment Per Year by Major",
    x = "Reporting Year",
    y = "Count",
    fill = "Race"
  ) +
  scale_y_continuous(labels = label_comma())
  
  
