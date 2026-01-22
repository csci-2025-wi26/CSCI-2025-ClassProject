library(arrow)
library(tidyverse)
library(dplyr)
cleaned_data <- read_csv("data/processed/cleaned_data.csv")

gradeAverageDepartment <- cleaned_data |> 
  select(stc_person, xstc_verified_lettr_grade, dfw, grade_numeric, stc_depts) |> 
  group_by(stc_depts) |> 
  mutate(mean_department = mean(grade_numeric, na.rm = TRUE)) |>
  distinct(stc_depts, mean_department)

#print(gradeAverageDepartment, n = 26)


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



