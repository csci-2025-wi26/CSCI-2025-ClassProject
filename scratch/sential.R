library(tidyverse)
library(arrow)
cleaned_data <- read_csv("data/processed/cleaned_data.csv")

plot <- cleaned_data |> 
  select(stc_person, xstc_verified_lettr_grade, dfw, grade_numeric, stc_depts, stc_course_name) |> 
  group_by(stc_depts) |> 
  mutate(mean_department = mean(grade_numeric, na.rm = TRUE)) |>
  distinct(stc_depts, mean_department) |> 
  ggplot(aes(y = stc_depts, x = mean_department))+
  geom_col()+
  labs(
  title = "Average Course Grade by Department",
  x = "Mean Numeric Grade",
  y = "Department",
  subtitle = "Mean of numeric course grades, aggregated by department",
  caption = "Grades averaged within each department; missing values excluded"
)

print(gradeAverageDepartment, n = 26)
