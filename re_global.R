library(tidyverse)
library(stringr)

raw_data <- read_csv("data/raw/registrar_data.csv")

major_map <- c(
  "Psychology" = "Psych.",
  "Biology" = "Bio.",
  "Computer Science" = "Comp. Sci.",
  "Exercise Science" = "Ex. Sci.",
  "Business Administration" = "Bus. Admin.",
  "Mathematics" = "Math.",
  "Accounting" = "Acct.",
  "Education" = "Ed.",
  "Environmental Studies" = "Env. Studies",
  "History" = "Hist.",
  "Anthropology" = "Anthro.",
  "Sociology" = "Soc."
)

process_journey <- function(major_string) {
  if (is.na(major_string)) return(NULL)
  
  parts <- str_split(major_string, ",")[[1]] |> str_trim()
  
  parts <- rev(parts)
  
  parts <- parts[!parts %in% c("OPEN", "NON", "NONGR")]
  
  if (length(parts) == 0) return(NULL)
  
  cleaned <- parts[1]
  if (length(parts) > 1) {
    for (i in 2:length(parts)) {
      if (parts[i] != parts[i-1]) {
        cleaned <- c(cleaned, parts[i])
      }
    }
  }
  return(cleaned)
}

retention_data <- raw_data |>
  group_by(stc_person) |>
  filter(term_numeric == max(term_numeric)) |>
  slice(1) |>
  ungroup() |>
  mutate(journey = map(students_stu_majors, process_journey)) |>
  filter(!map_lgl(journey, is.null)) |>
  mutate(transitions = map(journey, ~ {
    n <- length(.x)
    tibble(
      major = .x,
      did_shift = c(rep(TRUE, n - 1), FALSE)
    )
  })) |>
  select(stc_person, transitions) |>
  unnest(transitions) |>
  group_by(major) |>
  summarise(
    total_students = n(),
    shifters = sum(did_shift),
    .groups = 'drop'
  ) |>
  mutate(
    retention_rate = (1 - (shifters / total_students)) * 100
  ) |>
  filter(total_students >= 5) |>
  arrange(desc(retention_rate))