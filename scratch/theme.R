library(tidyverse)

theme_yote <- function() {
  theme_minimal(
    base_family = "Roboto Slab"
  ) +
  theme(
    axis.text.x = element_text(size = 12),
    legend.text  = element_text(size = 12),
    legend.title  = element_text(size = 15),
    legend.position = "top",
    title = element_text(size = 18, family = "Proxima Nova")
  )
}