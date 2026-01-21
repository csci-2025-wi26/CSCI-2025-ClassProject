library(tidyverse)
registrar_data <- read_csv("data/raw/registrar_data.csv")

registrar_data_bus <- registrar_data |>
  filter(stc_depts == "BUS")


  ggplot(aes(x=, y=freq))

plot_data <- registrar_data |>
  filter(stc_depts == "MATPH") |>
  count(stc_person) |>
  count(n, name = "freq")

ggplot(plot_data, aes(x = factor(n), y = freq)) +
  geom_col(fill = "steelblue")
