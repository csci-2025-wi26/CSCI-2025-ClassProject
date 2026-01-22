library(tidyverse)
library(dplyr)

registrar_data <- read_csv('data/raw/registrar_data.csv')

clean_data <- registrar_data |>
  separate_longer_delim(stc_depts, delim = ',') |>
  mutate(stc_depts = trimws(stc_depts)) |>
  group_by(stc_person, term_reporting_year) |>
  mutate(major = first(stc_depts))

glimpse(clean_data)


#clean_data <- registrar_data |>
#  separate(stc_depts, into = c('first_major', 'second_major'), sep=',')

#glimpse(clean_data)


