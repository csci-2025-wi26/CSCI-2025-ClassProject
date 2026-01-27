library(tidyverse)
library(stringr)

raw_data     <- read_csv("data/raw/registrar_data.csv", show_col_types = FALSE)
mapping_raw  <- read_csv("data/raw/major-department_correlation.csv", show_col_types = FALSE)
dept_list    <- read_csv("data/raw/departments.csv", show_col_types = FALSE)

latest_term <- max(raw_data$term_numeric, na.rm = TRUE)

valid_depts <- dept_list |> 
  select(Dept_Code = `Department Code`, Dept_Name = `College of Idaho Department`) |> 
  mutate(Dept_Code = str_squish(Dept_Code)) |> 
  distinct()

major_map <- mapping_raw |>
  separate_rows(Code, sep = "/") |>
  mutate(
    Code = str_squish(Code),
    Department = str_squish(Department) 
  ) |>
  select(Code, Department)

major_overrides <- tribble(
  ~Code,    ~Department,
  "ACCTS",  "ACCT",
  "BIOCH",  "CHE",
  "COMSC",  "COMM",
  "REL",    "PHIL",
  "RELST",  "PHIL",
  "MUSED",  "MUS",
  "MUSPF",  "MUS"
)

final_major_map <- bind_rows(major_map, major_overrides) |>
  filter(!Code %in% c("APCHE", "BUMDM", "CRW", "EED", "ELIT", "EXPH", 
                      "HEA", "HPER", "PHARM", "PHE", "PREC", "ROM")) |>
  semi_join(valid_depts, by = c("Department" = "Dept_Code")) |>
  deframe()

prefix_overrides <- tribble(
  ~Prefix,  ~Department,
  "ATH",    "ANSC",
  "COM",    "COMM",
  "ENVI",   "ENVST",
  "PHI",    "PHIL",
  "POLEC",  "POE",
  "REL",    "PHIL"
)

get_dept <- function(code) {
  code <- str_squish(code)
  
  if (code %in% names(final_major_map)) return(final_major_map[[code]])
  
  override <- prefix_overrides |> filter(Prefix == code) |> pull(Department)
  if (length(override) > 0) return(override)
  
  if (code %in% valid_depts$Dept_Code) return(code)
  
  return(NA_character_)
}

extract_depts <- function(major_str) {
  if (is.na(major_str)) return(character(0))
  codes <- str_split(major_str, ",")[[1]] |> str_squish()
  depts <- map_chr(codes, get_dept)
  unique(depts[!is.na(depts)])
}

student_snapshot <- raw_data |>
  group_by(stc_person) |>
  filter(term_numeric == max(term_numeric)) |> 
  slice(1) |>
  ungroup() |>
  select(stc_person, term_numeric, students_stu_majors, students_xstu_grad_app_major, students_xstu_grad_acad_year)

processed_students <- student_snapshot |>
  mutate(
    grad_year_raw = str_remove(as.character(students_xstu_grad_acad_year), "\\.0$"),
    grad_year     = as.numeric(str_sub(grad_year_raw, -4)),
    
    history_depts = map(students_stu_majors, extract_depts),
    grad_depts = map(students_xstu_grad_app_major, extract_depts),
    
    has_grad_year = !is.na(grad_year)
  )

get_bubble_data <- function(min_year, max_year) {
  
  valid_grads <- processed_students |>
    filter(has_grad_year, grad_year >= min_year, grad_year <= max_year)
  
  valid_depts |>
    rename(dept = Dept_Code) |>
    mutate(stats = map(dept, function(d) {
      
      ever_declared <- processed_students |> filter(map_lgl(history_depts, ~ d %in% .x))
      denom <- nrow(ever_declared)
      
      if (denom == 0) return(tibble(graduated = 0, total = 0, rate = 0))
      
      graduated_with <- valid_grads |>
        filter(map_lgl(grad_depts, ~ d %in% .x)) |>
        nrow()
      
      return(tibble(graduated = graduated_with, total = denom, rate = (graduated_with / denom) * 100))
    })) |>
    unnest(stats)
}

get_major_flow <- function(target_dept, min_year, max_year) {
  
  target_dept <- str_squish(target_dept) 
  
  starters <- processed_students |>
    filter(map_lgl(history_depts, ~ target_dept %in% .x))
  
  if (nrow(starters) == 0) return(NULL)
  
  starters |>
    mutate(
      primary_grad = map_chr(grad_depts, function(x) if(length(x)>0) x[1] else NA_character_),
      
      primary_grad_clean = str_squish(primary_grad),
      
      outcome = case_when(
        !has_grad_year & term_numeric >= latest_term ~ "Still Enrolled",
        !has_grad_year ~ "Dropped Out",
        
        grad_year < min_year | grad_year > max_year ~ "Graduated (Other Year)",
        
        map_lgl(grad_depts, ~ target_dept %in% .x) ~ paste("Retained:", target_dept),
        
        is.na(primary_grad) ~ "Unknown Grad Major",
        
        primary_grad_clean == target_dept ~ paste("Retained:", target_dept),
        
        TRUE ~ paste("Switched to:", primary_grad)
      )
    ) |>
    count(outcome) |>
    arrange(desc(n))
}