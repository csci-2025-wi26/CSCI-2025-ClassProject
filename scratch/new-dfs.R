library(arrow)
library(tidyverse)
library(dplyr)
cleaned_data <- read_parquet("data/processed/cleaned_data.parquet")

gradeAverageDepartment <- cleaned_data |> 
  select(stc_person, xstc_verified_lettr_grade, dfw, grade_numeric, stc_depts) |> 
  group_by(stc_depts) |> 
  mutate(mean_department = mean(grade_numeric, na.rm = TRUE)) |>
  distinct(stc_depts, mean_department)

#print(gradeAverageDepartment, n = 26)


# creating df's with dfw rates for diff groups. gender, dept. etc.
dfw_rates_gender <- clean_data |> 
  group_by(person_gender) |> 
  summarise(mean(dfw, na.rm = TRUE))

dfw_rates_gender
