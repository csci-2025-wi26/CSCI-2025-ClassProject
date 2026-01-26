glimpse(progression_summary_re)
library(forcats)

progression_summary_re |> 
    filter(!is.na(students_stu_class)) |> 
    mutate(students_stu_class = factor(students_stu_class, 
           levels = c("FR", "SO", "JR", "SR"))) |>  # <--- This forces the order
    group_by(re) |> 
    ggplot(aes(x = students_stu_class, y = avg_gpa, group = re, color = re)) +
    geom_line()
