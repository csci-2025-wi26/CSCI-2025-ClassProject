library(tidyverse)
library(dplyr)

registrar_data <- read_csv('data/raw/registrar_data.csv')

clean_data <- registrar_data |>
  group_by(stc_person, term_reporting_year) |>
  mutate(major = first(stc_depts)) |>
  ungroup()

glimpse(clean_data)


#clean_data <- registrar_data |>
#  separate(stc_depts, into = c('first_major', 'second_major'), sep=',')

#glimpse(clean_data)


