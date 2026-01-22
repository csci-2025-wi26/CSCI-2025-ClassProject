library(tidyverse)
registrar_data <- read_csv("data/raw/registrar_data.csv")

registrar_data |> 
  select("term_reporting_year")
 

  #ggplot(aes(x = stc_depts, y = person_xper_chosen_last_name)) +
  #geom_col()

# we have a count of how many classes a student has taken in a particular department 
# We need to look at the school as a whole along with grades 

#Is these a specific class that students do particularly bad in 
# Is there a specific course that students fail 
# Do students go to take more courses after a specific course? 
# Western Civ and FYS - what grade do students get and will they continue? 

# Look at intro classes 
# If students take an intro class how likely are they to take more classes in the department 
registrar_data |> 
  separate_wider_delim(
    cols = students_stu_majors,
    delim = ",",
    names = c("final_major", "major_1", "major_2", "major_3", "major_4"),
    too_few = "align_start",
    too_many = "merge"
  ) |> 
  select(c("final_major", "major_1", "major_2", "major_3", "major_4")) |> 
  filter(major_2 == "OPEN")

