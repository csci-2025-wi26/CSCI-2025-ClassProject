library(tidyverse)
library(dplyr)

raw_data <- vroom::vroom("C:\\Users\\logan\\CSCI-2025-ClassProject\\data\\raw\\registrar_data.csv")

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

#turn all the AU grades in letter grades column into NA's so they can be handled.
updated_clean_data <- updated_clean_data |> 
  mutate(xstc_verified_lettr_grade = na_if(xstc_verified_lettr_grade, "AU")) 

#this makes upper and lower division column. tested, includes no NA's. assumes that '1's are '100'
updated_clean_data <- updated_clean_data |> 
  mutate(upper_lower_div = case_when(str_detect(crs_no, "^1.*") | str_detect(crs_no, "^2.*") ~ "lower",
        str_detect(crs_no, "^3.*") | str_detect(crs_no, "^4.*") | str_detect(crs_no, "5.*") | str_detect(crs_no, "6.*") ~ "upper") 
  ) 

#make a column that specifies the course level so that users can eventually be able to filter/select by course level. Assumed 3-digit format still. Tested and confirmed no NA's in this column.
updated_clean_data <- updated_clean_data |> 
  mutate(crs_level = case_when(str_detect(crs_no, "^1.*") ~ "1XX",
                              str_detect(crs_no, "^2.*") ~ "2XX",
                              str_detect(crs_no, "^3.*")~ "3XX", 
                              str_detect(crs_no, "^4.*") ~ "4XX", 
                              str_detect(crs_no, "^5.*") ~ "5XX", 
                              str_detect(crs_no, "^6.*") ~ "6XX"
                            )
                          )


#strip the commas from the students_stu_class
updated_clean_data <- updated_clean_data |> 
  mutate(students_stu_class = str_remove_all(students_stu_class, ","))

#remove numbers and extra whitespace from re
updated_clean_data <- updated_clean_data |> 
  mutate(re = str_replace(re, "[^a-zA-Z]{2,}", ""))




#choosing to leave upper_lower_div and crs_level as strings for now.
#glimpse(updated_clean_data)




#write_csv(updated_clean_data, "data/processed/cleaned_data.csv")


# Run this command to bring the dataframe into your script:
# cleaned_data <- read_csv("data/processed/cleaned_data.csv")



# _____Shiny stuff_____

# source('en_global.R')

# main page with a title that has C of I branding
# left side panel page that has 'Home', 'Enrollment', 'Retention', 
# 'Outcomes', 'Performance'
# mini tabs with each group (plot per tab)
# download button for each plot
# filters across groups: department (selectInput), class standing (selectInput)
# add prefixes for variables (like 'pe_' for performance)
# inputs above plots

library(shiny)
library(bslib)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Q1",
      fluidRow(
        column(12,
          selectInput("opt1", "DFW or GPA?", choices = c("dfw", "grade_numeric")),
          radioButtons("opt2", "Select Demographic", choices = c("person_gender", "re", "pell"))
        )
      ),
      fluidRow(
        column(12,
          tableOutput("plotq1")
        )
      )
    ),
    tabPanel("Q2",
      fluidRow(
        column(12
        )
      ),
      fluidRow(
        column(12,
          plotOutput("plotq2")
        )
      )
    ),
    tabPanel("Q3",
      fluidRow(
        column(12
        )
      ),
      fluidRow(
        column(12,
          plotOutput("plotq3")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  output$plotq1 <- renderPlot({
    ggplot(updated_clean_data, aes(x = students_stu_class, y = .data[[input$opt1]],
      fill = .data[[input$opt2]])) + 
      geom_col(stat = "identity")
  })
  
  output$plotq2 <- renderPlot({
    ggplot(data.frame(x = c(10:6), y = c(6:10)), aes(x = x, y = y)) +
      geom_point()
  })

  output$plotq3 <- renderPlot({
    ggplot(data.frame(x = c(1:5), y = c(5:1)), aes(x = x, y = y)) +
      geom_point()
  })
}

shinyApp(ui, server)