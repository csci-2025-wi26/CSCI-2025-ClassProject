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
    legend.position = "top",
    title = element_text(size = 18, family = "Proxima Nova")
  ) +
  labs(fill = "Grade",
    x = "Department",
    y = "Mean Score",
    title = "Mean Grade Score by Department",
    )

mean_scores

ggsave("plots/mean_scores.png", plot = mean_scores, width = 12, height = 6)

#____________________

dfwStanding <- cleaned_data |>
  filter(!is.na(students_stu_class)) |>
  ggplot(aes(x = students_stu_class, y = dfw, na.rm = TRUE)) +
  geom_col(fill = "#533860") +
  labs(
    x = "Standing",
    y = "Students with a D, F, or W",
    title = "DFW by Standing"
  ) +
  theme_minimal() +
  theme(
    title = element_text(size = 18, family = "Proxima Nova"),
    axis.text.x = element_text(size = 12),
  )

ggsave("plots/dfw_by_standing.png", plot = dfwStanding, width = 12, height = 6)

#________________

cleaned_data$class_level <- str_extract(cleaned_data$stc_course_name, "(?<=-)\\d") |> as.factor()

dfwDeptMath <- cleaned_data |>
  filter(!is.na(students_stu_class),
          stc_depts %in% "MATPH") |>
  ggplot(aes(x = interaction(class_level, stc_course_name, lex.order = TRUE), y = dfw, na.rm = TRUE, fill = class_level)) +
  geom_col() +
  scale_fill_manual(
    values = c(
    "1" = "#533860",
    "2" = "#228B22",
    "3" = "#00BFFF",
    "4" = "#E2725B"
  )) +
  scale_x_discrete(labels = cleaned_data$stc_course_name) +
  labs(
    x = "Classes (Math Department)",
    y = "Students with a D, F, or W",
    title = "DFW by Class (Math Department)"
  ) +
  theme_minimal() +
  theme(
    title = element_text(size = 18, family = "Proxima Nova"),
    axis.text.x = element_text(size = 12, )
    
  ) +
  coord_flip()

ggsave("plots/dfw_by_dept_math.png", plot = dfwDeptMath, width = 10, height = 16)

#________

dfw_rates_gender <- cleaned_data |> 
  group_by(person_gender) |> 
  summarise(mean(dfw, na.rm = TRUE))

dfw_rates_dept <- cleaned_data |> 
  group_by(stc_depts) |> 
  summarize(mean(dfw, na.rm = TRUE))

dfw_rates_gender_bp <- cleaned_data |> 
  group_by(person_gender) |> 
  reframe(dfw, na.rm = TRUE)

grades_gender <- cleaned_data |> 
  group_by(person_gender) |> 
  reframe(grade_numeric, na.rm = TRUE)


dfw_rates_gender |>
  ggplot(aes(x = person_gender, y = `mean(dfw, na.rm = TRUE)`)) + 
  geom_col()+
  coord_cartesian(ylim = c(0.95, NA)) +
  labs(
  ) +
  theme_yote()

dfw_rates_gender |>
  ggplot(aes(x = person_gender, y = `mean(dfw, na.rm = TRUE)`)) + 
  geom_col()+
  coord_cartesian(ylim = c(0.95, NA)) +
  labs(
  ) +
  theme_yote()

#___________________________
#Probably the most important  plot.

cleaned_data <- read_csv('data/processed/cleaned_data.csv')

dfwDeptMathPro <- cleaned_data |>
  filter(!is.na(students_stu_class), !is.na(dfw),
          stc_depts == "MATPH") |>
  ggplot(aes(x = reorder(stc_course_name, dfw), fill = factor(dfw))) +
  geom_bar(position = "fill") +
  geom_hline(
    yintercept = c(0.25, 0.5, 0.75),
    linetype = "dotted"
  ) +
  scale_fill_manual(
    values = c(
    "0" = "#533860",
    "1" = "#228B22",
    "2" = "#00BFFF",
    "3" = "#E2725B"
  ),
    labels = c("Students with a DFW", "Students who passed")) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Classes (math department)",
    y = "Students pass rate",
    title = "DFW by Class (Math Department)",
    fill = "DFW"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12, family = "Roboto Slab"),
    title = element_text(size = 18, family = "Proxima Nova"),
    axis.text.x = element_text(size = 12, )
    
  ) +
  coord_flip()

dfwDeptMathPro

ggsave("plots/dfw_by_dept_math_proportional.png", plot = dfwDeptMathPro, width = 10, height = 16)
