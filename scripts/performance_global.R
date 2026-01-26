#contains all cleaning and objects that will eventually be needed for plots and app.


######------------------Cleaning------------------#########

library(tidyverse)
library(dplyr)

#See full cleaning script in creating-dfw-and-numeric.R
# Run this command to bring the dataframe into your script:
 cleaned_data <- read_csv("data/processed/cleaned_data.csv", 
                       col_types = cols(
                              students_xstu_grad_acad_year = col_character(), 
                              term_numeric = col_character(),
                              stc_person = col_character(),
                              term_reporting_year = col_character()
                        )
                  )



############------------------END OF CLEANING---------------#########



################------------STUFF FROM CHARTS---------------########

gradeAverageDepartment <- cleaned_data |> 
  select(stc_person, xstc_verified_lettr_grade, dfw, grade_numeric, stc_depts) |> 
  group_by(stc_depts) |> 
  mutate(mean_department = mean(grade_numeric, na.rm = TRUE)) |>
  mutate(stc_depts = fct_inorder(stc_depts)) |>
  arrange(mean_department) |>
  distinct(stc_depts, mean_department)

print(gradeAverageDepartment, n = 26)


##############-----------END OF STUFF FROM CHARTS----------########


#############-----------STUFF FROM EXTRACTING SEM DATA -----#######
extracting_sem_data <- cleaned_data |> 
  separate(student_course_sec_sec_term, into = c("year_digit", "term"), sep = "/") |> 
  mutate(
    year = 2000 + as.integer(year_digit),
    term_order = case_when(
      term == "SP" ~ 1,
      term == "SU" ~ 2,
      term == "FA" ~ 3,
      TRUE ~ NA_real_
    ),
    semester_index = year * 10 + term_order    #This line is creating a numeric column to sort by
  ) 

first_semester_grades_summary <- extracting_sem_data |> 
  group_by(stc_person) |> 
  filter(semester_index == min(semester_index, na.rm = TRUE)) |> #select the first semester; min of semester_index
  summarise(
    first_semester_mean_grade = mean(grade_numeric, na.rm = TRUE),
    first_semester_dfw = mean(dfw, na.rm = TRUE)
  )

first_two_semesters <- extracting_sem_data |> 
  group_by(stc_person) |> 
  arrange(semester_index) |> 
  filter(row_number() <= 2) |> 
  ungroup() |> 
  head()

#############----------END OF STUFF FROM EXTRACTING SEM DATA-----#####

############-----------NEW DF'S STUFF --------------------------#########

# 5 similar examples of creating df's with dfw rates for different groups. 
# Note how only the dataframe name and group changes - easy to put in an app with user selecting the group.
dfw_rates_gender <- cleaned_data |> 
  group_by(person_gender) |> 
  summarise(mean(dfw, na.rm = TRUE))

dfw_rates_dept <- cleaned_data |> 
  group_by(stc_depts) |> 
  summarize(mean(dfw, na.rm = TRUE)) |> 
  glimpse()

dfw_rates_crs_level <- cleaned_data |> 
  group_by(crs_level) |> 
  summarize(mean(dfw, na.rm = TRUE)) |> 
  glimpse()

dfw_rates_course <- cleaned_data |> 
  group_by(stc_course_name) |> 
  summarize(mean(dfw, na.rm = TRUE)) |> 
  glimpse()

dfw_rates_standing <- cleaned_data |> 
  group_by(students_stu_class) |> 
  summarize(mean(dfw, na.rm = TRUE)) |> 
  glimpse()


#same idea for numeric gpa. like with dfw, just change the group_by to get the avg gpa by group for any group you want.

grades_gender <- cleaned_data |> 
  group_by(person_gender) |> 
  summarise(mean(grade_numeric, na.rm = TRUE)) |> 
  glimpse()






#like above, the following 3 examples are highly adaptable.
#tracking changes over time -- longitudinal data.

progression_summary_re <- cleaned_data |> 
  group_by(re, students_stu_class) |> 
  summarise(
    avg_gpa = mean(grade_numeric, na.rm = TRUE),    # Tracks performance level
    gpa_variance = var(grade_numeric, na.rm = TRUE), # Tracks "spread" (inequality within group)
    .groups = "drop"
  ) |> 
  glimpse()

#then can visualize as a line plot where each race is a different colored line, so ...
#... we can see how the avg and the variance PROGRESSES for each group

#NOTE: could change the first group, but would want to keep the students_stu_class to still track longitudinal changes over time.

#same example but with upper/lower div instead of class standing (would result in plot that would allow visualization of upper/lower performances across different groups.)

upper_lower_re <- cleaned_data |> 
  group_by(re, upper_lower_div) |> 
  summarise(
    avg_gpa = mean(grade_numeric, na.rm = TRUE),    # Tracks performance level
    gpa_variance = var(grade_numeric, na.rm = TRUE), # Tracks "spread" (inequality within group)
    .groups = "drop"
  ) |> 
  glimpse()


#another example: progression by gender (instead of race). also showing that you can use dfw instead of gpa

progression_summary_gender <- cleaned_data |> 
  group_by(person_gender, students_stu_class) |> 
  summarise(
    dfwrate = mean(dfw, na.rm = TRUE),    # Tracks performance level
    dfw_variance = var(dfw, na.rm = TRUE), # Tracks "spread" (inequality within group)
    .groups = "drop"
  ) |> 
  glimpse()


#########----------END OF NEW DF'S STUFF--------------############
