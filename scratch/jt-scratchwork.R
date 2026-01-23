library(tidyverse)
library(dplyr)
cleaned_data <- read_csv("data/processed/cleaned_data.csv")

glimpse(cleaned_data)

cleaned_data |> 
  select(majors_maj_desc, students_stu_active_majors) |> 
  filter(is.na(students_stu_active_majors)) |> 
  glimpse()

cleaned_data |> 
  count(minors_minors_desc)

cleaned_data |> 
  count(students_xstu_grad_acad_year) |> 
  print(n=27)

cleaned_data |> 
  mutate(
    students_xstu_grad_acad_year = str_replace(students_xstu_grad_acad_year, "^(\\d{4})(\\d+)", "\\1,\\2")
  ) |> 
  count(students_xstu_grad_acad_year) |>  print(n=27)


  # arrange(n) |> 
  # filter(!str_detect(students_stu_active_majors, "^.{3,}$") & !str_detect(students_stu_active_majors, ",")) |> 
  # glimpse()


# cleaned_data |> 
#   filter(str_detect(stc_depts, ",")|str_detect(stc_depts, " ")) |> 
#   count(stc_depts)

# cleaned_data |> 
#   select(stc_course_name, crs_no, student_course_sec_stc_title, crs_level, upper_lower_div) |>
#   filter(crs_no == "1")



# #column E looks problematic: commas in double majors. if someone is selecting majors we want 
# #... double major "John" to be detected by major A or major B

# #the code would look like data |> filter(major == MATH)
# # We have mathph, no MAT, no PHY

# cleaned_data |> 
#   count(stc_depts) |> 
#   filter(stc_depts == "PHY")

# #go over first 3 cols

# cleaned_data |> 
#   count(stc_person) |> 
#   filter(stc_person > 0) |> 
#   arrange(stc_person)

# cleaned_data |> 
#   count(stu_acad_programs) |> 
#   arrange(n)










# #------------------------------
# cleaned_data |> 
#   select(stc_course_name, 
#     stc_sec_name, 
#     student_course_sec_stc_title, 
#     crs_no) |> 
#   glimpse()

# #course name nested in sec name.
# #number is just course name without the prefix
# #full course title is just a string, doesn't need to be messed with.




# #this makes upper and lower division column. tested, includes no NA's. assumes that '1's are '100'
# cleaned_data |> 
#   mutate(division = case_when(str_detect(crs_no, "^1.*") | str_detect(crs_no, "^2.*") ~ "lower",
#         str_detect(crs_no, "^3.*") | str_detect(crs_no, "^4.*") | str_detect(crs_no, "5.*") | str_detect(crs_no, "6.*") ~ "upper") 
#   ) |> 
#   # select(crs_no, division) |> 
#   # filter(is.na(division)) |> 
#   glimpse()

# cleaned_data |> 
#   mutate(crs_no_numeric = as.numeric(crs_no)) |> 


# #there are no NA course numbers. it's the XX's and "L's" that are causing me problems above.
# cleaned_data |> 
#   count(crs_no) |> arrange(n)







# cleaned_data |> 
#   select(stc_course_name, 
#     stc_sec_name, 
#     student_course_sec_stc_title, 
#     crs_no) |> 
#   filter(str_detect(crs_no, "^/d{4}$")) |> 
#   glimpse()


# cleaned_data |> 
#   mutate(crs_level = case_when(str_detect(crs_no, "^1.*") ~ "1XX",
#                               str_detect(crs_no, "^2.*") ~ "2XX",
#                               str_detect(crs_no, "^3.*")~ "3XX", 
#                               str_detect(crs_no, "^4.*") ~ "4XX", 
#                               str_detect(crs_no, "^5.*") ~ "5XX", 
#                               str_detect(crs_no, "^6.*") ~ "6XX"
#                             )
#                           ) |> 
#   filter(!str_detect(crs_level, "^\\dXX$")) |>     #test - returns nothing, so nothing left out.
#   glimpse()




# glimpse(cleaned_data)

# cleaned_data |> 
#   count(students_stu_class)

# cleaned_data |> 
#   mutate(students_stu_class = str_remove_all(students_stu_class, ",")) |> 
#   count(students_stu_class) |> 
#   # filter(is.na(students_stu_class)) |> 
#   # filter(stc_acad_level != "GR") |> 
#   glimpse()

# #^confirms all the na's in students_stu_class are from the Graduate students

# cleaned_data |> 
#   select(stc_acad_level, students_stu_class) |> 
#   filter(stc_acad_level == "GR") |> 
#   glimpse()

# cleaned_data |> 
#   count(stc_acad_level)



