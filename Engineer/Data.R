library(tidyverse)
library(dplyr)

registrar <- read_csv("data/raw/cleaned_registrar_data.csv")

registrar_weighted <- registrar |>
  distinct(stc_person, term_reporting_year, .keep_all = TRUE) |> 
  mutate(race_count = str_count(person_per_races, ",") + 1) |>
  separate_longer_delim(person_per_races, delim = ",") |>
  mutate(person_per_races = trimws(person_per_races)) |>
  mutate(student_weight = 1 / race_count) |> 
  group_by(stc_person, term_reporting_year)

glimpse(registrar_weighted)




