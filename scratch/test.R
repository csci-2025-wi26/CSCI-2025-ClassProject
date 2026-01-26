dfw_by_level_plot <- dfw_by_level |>
  ggplot(aes(x = crs_level, y = dfw_rate)) +
  geom_col() +
  labs(
    title = "DFW Rate by Course Level",
    x = "Course Level",
    y = "DFW Rate"
  ) +
  theme_minimal()

dfw_by_level_plot