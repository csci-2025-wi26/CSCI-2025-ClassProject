library(tidyverse)
library(dplyr)

raw_data <- read_csv('data/raw/registrar_data.csv')

#adding dfw column
clean_data <- raw_data |> 
  mutate(dfw = case_when(
    xstc_verified_lettr_grade %in% c("A+", "A", "A-", "B+", "B", "B-", "C+", "C", "C-", "P") ~ 1, 
    xstc_verified_lettr_grade %in% c('W', "F", "D-", "D", "D+")  ~ 0
  ))

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
    xstc_verified_lettr_grade == "F" ~ 0.00 
  )) 

#make AU be NA so you can drop them all.

updated_clean_data |> 
  mutate(xstc_verified_lettr_grade = na_if(xstc_verified_lettr_grade, "AU")) 


#####


#install.packages('arrow')
library(arrow)
arrow::write_parquet(updated_clean_data, "data/processed/cleaned_data.parquet")

# Run this command to bring the dataframe into your script:
# cleaned_data <- read_parquet("data/processed/cleaned_data.parquet")


######










#creating df's with dfw rates
# dfw_rates_gender <- updated_clean_data |> 
#   group_by(person_gender) |> 
#   summarise(mean(dfw, na.rm = TRUE))

# dfw_rates_departments 

