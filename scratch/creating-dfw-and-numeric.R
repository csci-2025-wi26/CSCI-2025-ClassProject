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

#turn all the AU grades in letter grades column into NA's so they can be handled.
updated_clean_data <- updated_clean_data |> 
  mutate(xstc_verified_lettr_grade = na_if(xstc_verified_lettr_grade, "AU")) 

#this makes upper and lower division column. tested, includes no NA's. assumes that '1's are '100'
updated_clean_data <- updated_clean_data |> 
  mutate(upper_lower_div = case_when(str_detect(crs_no, "^1.*") | str_detect(crs_no, "^2.*") ~ "lower",
        str_detect(crs_no, "^3.*") | str_detect(crs_no, "^4.*") | str_detect(crs_no, "5.*") | str_detect(crs_no, "6.*") ~ "upper") 
  ) 

#make a column that specifies the course level so that users can eventually be able to filter/select by course level. Assumed 3-digit format still. Tested and confirmed no NA's in this column.
#note that this will work for BOTH the 3-digit format, and the new 4-digit format of course numbers.
updated_clean_data <- updated_clean_data |> 
  mutate(crs_level = case_when(str_detect(crs_no, "^1.*") ~ "1XXX",
                              str_detect(crs_no, "^2.*") ~ "2XXX",
                              str_detect(crs_no, "^3.*")~ "3XXX", 
                              str_detect(crs_no, "^4.*") ~ "4XXX", 
                              str_detect(crs_no, "^5.*") ~ "5XXX", 
                              str_detect(crs_no, "^6.*") ~ "6XXX"
                            )
                          )


#strip the commas from the students_stu_class
updated_clean_data <- updated_clean_data |> 
  mutate(students_stu_class = str_remove_all(students_stu_class, ","))

#remove numbers and extra whitespace from re
updated_clean_data <- updated_clean_data |> 
  mutate(re = str_replace(re, "[^a-zA-Z]{2,}", ""))

#some expected graduation years in xstu_grad_acad_year contain two values. 
#in order to not lose information we will keep these values, but add a comma for clarity.
#also switch to character instead of dbl since this is more of a category.
updated_clean_data <- updated_clean_data |> 
  mutate(students_xstu_grad_acad_year = as.character(students_xstu_grad_acad_year)) |> 
  mutate(
    students_xstu_grad_acad_year = str_replace(students_xstu_grad_acad_year, "^(\\d{4})(\\d+)", "\\1,\\2")
  )



write_csv(updated_clean_data, "data/processed/cleaned_data.csv")



# Run this command to bring the dataframe into your script:
#  cleaned_data <- read_csv("data/processed/cleaned_data.csv", 
#                        col_types = cols(
#                               students_xstu_grad_acad_year = col_character(), 
#                               term_numeric = col_character()
#                               stc_person = col_character(),
#                               term_reporting_year = col_character()
#                         )
#                   )



