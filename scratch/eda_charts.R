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

gradeAverageDepartment <- updated_clean_data |> 
  select(stc_person, xstc_verified_lettr_grade, dfw, grade_numeric, stc_depts) |> 
  group_by(stc_depts) |> 
  mutate(mean_department = mean(grade_numeric, na.rm = TRUE)) |>
  mutate(stc_depts = fct_inorder(stc_depts)) |>
  arrange(mean_department) |>
  distinct(stc_depts, mean_department)

print(gradeAverageDepartment, n = 26)

mean_scores <- gradeAverageDepartment |>
  ggplot(aes(x = reorder(stc_depts, mean_department), y = mean_department, fill = mean_department >= 3.30)) +
  geom_col() +
  coord_cartesian(ylim = c(3, NA)) +
  scale_fill_manual(
    values = c("FALSE" = "#533860", "TRUE" = "#228B22"),
    labels = c("Below B+", "B+ or higher")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    legend.text  = element_text(size = 12),
    legend.title  = element_text(size = 15),
    legend.position = "top"
  ) +
  labs(fill = "Grade",
    x = "Departments",
    y = "Mean Score")

ggsave("charts/mean_scores.png", plot = mean_scores, width = 12, height = 6)
