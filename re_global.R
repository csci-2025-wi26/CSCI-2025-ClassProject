library(tidyverse)
library(stringr)


raw_data     <- read_csv("data/raw/registrar_data.csv", show_col_types = FALSE)
mapping_raw  <- read_csv("data/raw/major-department_correlation.csv", show_col_types = FALSE)
dept_list    <- read_csv("data/raw/departments.csv", show_col_types = FALSE)


valid_depts <- dept_list |> 
  select(Dept_Code = `Department Code(s)`, Dept_Name = `College of Idaho Department`) |> 
  distinct()


major_map <- mapping_raw |>
  separate_rows(Code, sep = "/") |>
  mutate(Code = str_trim(Code)) |>
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

prefix_map <- c(
  "ATH"   = "ANSC",
  "COM"   = "COMM",
  "ENVI"  = "ENVST",
  "PHI"   = "PHIL",
  "POLEC" = "POE",
  "REL"   = "PHIL"
)

get_dept <- function(code) {
  code <- str_trim(code)
  
  if (code %in% names(final_major_map)) return(final_major_map[[code]])
  
  if (code %in% names(prefix_map)) return(prefix_map[[code]])
  
  return(NA_character_)
}


# Graph 1


student_snapshot <- raw_data |>
  group_by(stc_person) |>
  filter(term_numeric == max(term_numeric)) |> 
  slice(1) |>
  ungroup() |>
  select(stc_person, students_stu_majors, students_xstu_grad_app_major, students_xstu_grad_acad_year)

extract_depts <- function(major_str) {
  if (is.na(major_str)) return(character(0))
  codes <- str_split(major_str, ",")[[1]] |> str_trim()
  depts <- map_chr(codes, get_dept)
  unique(depts[!is.na(depts)])
}

graduates_data <- student_snapshot |>
  filter(!is.na(students_xstu_grad_acad_year)) |> 
  mutate(
    history_depts = map(students_stu_majors, extract_depts),
    grad_depts    = map(students_xstu_grad_app_major, extract_depts)
  )

retention_stats <- valid_depts |>
  rename(dept = Dept_Code) |>
  mutate(
    stats = map(dept, function(d) {
      ever_declared <- graduates_data |> 
        filter(map_lgl(history_depts, ~ d %in% .x))
      
      denom <- nrow(ever_declared)
      
      if (denom == 0) return(tibble(graduated = 0, total = 0, rate = 0))
      
      graduated_with <- ever_declared |>
        filter(map_lgl(grad_depts, ~ d %in% .x)) |>
        nrow()
      
      return(tibble(graduated = graduated_with, total = denom, rate = (graduated_with / denom) * 100))
    })
  ) |>
  unnest(stats)



# Graph 2

intro_raw <- raw_data |>
  select(stc_person, stc_course_name, stc_depts, students_xstu_grad_acad_year, students_xstu_grad_app_major) |>
  mutate(
    course_num = as.numeric(str_extract(stc_course_name, "\\d+")),
    prefix = str_trim(stc_depts) 
  ) |>
  filter(course_num >= 100 & course_num <= 199)

get_intro_targets <- function(p) {
  if (p == "BUACC") return(c("ACCT", "BUS"))
  if (p == "MATPH") return(c("MATH", "PHY"))
  
  val <- get_dept(p)
  if (is.na(val)) return(NULL)
  return(val)
}

intro_mapped <- intro_raw |>
  mutate(dept_target = map(prefix, get_intro_targets)) |>
  unnest(dept_target) 

intro_stats <- valid_depts |>
  rename(dept = Dept_Code) |>
  mutate(
    stats = map(dept, function(d) {
      students_took_intro <- intro_mapped |>
        filter(dept_target == d) |>
        pull(stc_person) |>
        unique()
      
      denom <- length(students_took_intro)
      
      if (denom == 0) return(tibble(graduated = 0, total = 0, rate = 0))
      
      grads_in_major <- graduates_data |>
        filter(stc_person %in% students_took_intro) |>
        filter(map_lgl(grad_depts, ~ d %in% .x)) |>
        nrow()
      
      return(tibble(graduated = grads_in_major, total = denom, rate = (grads_in_major / denom) * 100))
    })
  ) |>
  unnest(stats)