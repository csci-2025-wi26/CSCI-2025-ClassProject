library(tidyverse)
library(dplyr)

registrar_data <- read_csv('data/raw/registrar_data.csv')

clean_data <- registrar_data |>
  group_by(stc_person, term_reporting_year) |>
  summarise(
    major = first(stc_depts),
    .groups = "drop"
  )

glimpse(clean_data)


#clean_data <- registrar_data |>
#  separate(stc_depts, into = c('first_major', 'second_major'), sep=',')

#glimpse(clean_data)


