registrar <- read_csv("./data/raw/cleaned_registrar_data.csv")

registrar_weighted <- registrar |>
  mutate(
    person_per_races = stri_enc_toutf8(as.character(person_per_races)),
    students_xstu_grad_app_major =
      stri_enc_toutf8(as.character(students_xstu_grad_app_major))
  ) |>
  distinct(stc_person, term_reporting_year, .keep_all = TRUE) |> 
  mutate(race_count = str_count(person_per_races, ",") + 1) |>
  separate_longer_delim(person_per_races, delim = ",") |>
  mutate(person_per_races = trimws(person_per_races)) |>
  mutate(student_weight = 1 / race_count) |> 
  group_by(stc_person, term_reporting_year) |>
  mutate(
    major = case_when(
      students_xstu_grad_app_major %in% c("MAT", "MATPH", "MATCS", "MATPHY", "PHMA.BA") ~ "MATH",
      students_xstu_grad_app_major %in% c("BIO", "BIO.BS", "BIOC.BS", "BIOCH", "BIOMD", "BIOMD.BA") ~ "BIO",
      students_xstu_grad_app_major %in% c("ACC", "ACC.BA", "ACCT") ~ "ACC",
      students_xstu_grad_app_major %in% c("HIS", "HIS.BA", "EXSCI,HIS") ~ "HIS",
      students_xstu_grad_app_major %in% c("EDIND", "ELIT", "CRW") ~ "ENG",
      students_xstu_grad_app_major == "THE" ~ "THE",
      students_xstu_grad_app_major %in% c("POE", "IPEC", "IPE", "INTAF.BA") ~ "POE",
      students_xstu_grad_app_major %in% c("EXSCI", "HPER", "EXPA", "EXSCI,BUMDM") ~ "EXSCI",
      students_xstu_grad_app_major == "MUS" ~ "MUS",
      students_xstu_grad_app_major %in% c("EDCUR", "EDINTE", "MUSED") ~ "EDU",
      students_xstu_grad_app_major == "RELST" ~ "RELST",
      students_xstu_grad_app_major %in% c("PSY", "PSY,EDIND") ~ "PSY",
      students_xstu_grad_app_major %in% c("ATH", "ATH.BA", "DMM") ~ "ATH",
      students_xstu_grad_app_major == "CHE" ~ "CHE",
      students_xstu_grad_app_major %in% c("BUS", "BUS.BA", "BUSMDM", "BUMDM", "BUS,ATH") ~ "BUS",
      students_xstu_grad_app_major %in% c("HEA", "LTCW", "LTCW.BA") ~ "HEA",
      students_xstu_grad_app_major %in% c("COM", "COM.BA") ~ "COM",
      students_xstu_grad_app_major %in% c("ENVST", "ENVSTU", "EVNST.BA") ~ "ENV",
      students_xstu_grad_app_major %in% c("CSC", "CSMA.BA", "CSMA.BS", "CSMAS") ~ "CSC",
      students_xstu_grad_app_major %in% c("ART.BA", "ARTDES", "ARTDS") ~ "ART",
      students_xstu_grad_app_major %in% c("PHI", "PHI.BA") ~ "PHI",
      students_xstu_grad_app_major == "SPA" ~ "SPA",
      students_xstu_grad_app_major == "FINC.BS" ~ "FINC",
      students_xstu_grad_app_major == "CRIM.BA" ~ "CRIM",
      TRUE ~ "OTHER"
    )
  )
write_csv(registrar_weighted, "cleaned_weighted_registrar_data.csv")