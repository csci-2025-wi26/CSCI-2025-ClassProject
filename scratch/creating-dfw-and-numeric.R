library(tidyverse)
library(dplyr)

raw_data <- read_csv('data/raw/registrar_data.csv')


#cleaning
clean_data <- raw_data |> 
  mutate(dfw = case_when(
    xstc_verified_lettr_grade %in% c("A+", "A", "A-", "B+", "B", "B-", "C+", "C", "C-", "P") ~ 1, 
    xstc_verified_lettr_grade %in% c('W', "F", "D-", "D", "D+")  ~ 0
  ))

#test -- should be empty.
clean_data |> 
  select(xstc_verified_lettr_grade, dfw) |>  
  filter(dfw != 0 & dfw != 1 | is.na(dfw)) |> 
  filter(!is.na(xstc_verified_lettr_grade) & xstc_verified_lettr_grade != "AU") |> 
  glimpse()

#look at only the letter grade and dfw
clean_data |> 
  select(xstc_verified_lettr_grade, dfw) |> 
  glimpse()


#__________add numeric gpa_______________

updated_clean_data <- clean_data |> 
  mutate(grade_numeric = case_when(
    xstc_verified_lettr_grade == "A" ~ 4.0,
    xstc_verified_lettr_grade == "A-" ~ 3.70,
    xstc_verified_lettr_grade == "B+" ~ 3.30,
    xstc_verified_lettr_grade == "B" ~ 3.00,
    xstc_verified_lettr_grade == "B-" ~ 2.70,
    xstc_verified_lettr_grade == "C+" ~ 2.30,
    xstc_verified_lettr_grade == "C" ~ 2.00,
    xstc_verified_lettr_grade == "C-" ~1.70,
    xstc_verified_lettr_grade == "D+" ~ 1.30,
    xstc_verified_lettr_grade == "D" ~ 1.00,
    xstc_verified_lettr_grade == "D-" ~ 0.70,
    xstc_verified_lettr_grade == "F" ~ 0.00 ,
  )) 

glimpse(updated_clean_data)

