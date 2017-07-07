library(shiny)
library(dplyr)
library(leaflet)
library(shinythemes)
library(ggplot2)
library(plotly)
library(data.table)
library(stringr)
DT = fread("/Users/bendenisshaffer/Dropbox/myKlovr/CollegeFinder/Data/Interim/collegeFinderDataInjected.csv")


shinyUI(navbarPage(title = "College Finder App",theme = shinytheme("united"),
        
        tabPanel("School Type",
                 sidebarPanel(
                         selectInput(inputId = "region", label = "Region",
                                     choices = unique(DT$Region), selected = "N"),
                         selectInput(inputId = "state", label = "Chose State", 
                                     choices = unique(DT$state), multiple = T, selected = c("MA","NY")),
                         sliderInput(inputId = "ugrad_total_pop", label = "School Size", 
                                     min = 0, max = 70000, step = 20, value = 40000),
                         checkboxInput(inputId = "four_year", label = "4 Year"),
                         checkboxInput(inputId = "two_year", label = "2 Year"),
                         checkboxInput(inputId = "private", label = "Private"),
                         checkboxInput(inputId = "public", label = "Public"),
                         radioButtons(inputId = "coed", label = "Co-Educational:",
                                      choices = c("COED" = 3,
                                                  "All Female" = 1,
                                                  "All Male" = 2)),
                         checkboxInput(inputId = "religious", label = "Has Religious Affiliation"),
                         checkboxInput(inputId = "specialized", label = "Specialized School"),
                         selectInput(inputId = "major_selecotr", label = "Choose Majors", 
                                     choices = str_trim(unique(unlist(str_split(DT$subjects_offered, pattern = ","))))[-1],
                                     multiple = T, selected = "Mathematics & Statistics")
                 ),
                 mainPanel(
                         leafletOutput("college_map"),
                         dataTableOutput("school_size")
                 )
                 
                 ),
        
        navbarMenu(title = "Academic",   
                   tabPanel("Standardized Tests",
                            sidebarPanel(
                                    sliderInput(inputId = "SAT_avg_composite_up", label = "Average SAT Upper Bound", 
                                                min = 0, max = 1600, value = 1500, step = 10),
                                    sliderInput(inputId = "SAT_avg_composite_lw", label = "Average SAT Lower Bound", 
                                                min = 0, max = 1600, value = 400, step = 10),
                                    sliderInput(inputId = "SAT_avg_math_up", label = "Average SAT MATH Upper Bound", 
                                                min = 0, max = 800, value = 700, step = 10),
                                    sliderInput(inputId = "SAT_avg_math_lw", label = "Average SAT MATH Lower Bound", 
                                                min = 0, max = 800, value = 500, step = 10),
                                    sliderInput(inputId = "SAT_avg_reading_up", label = "Average SAT READING Upper Bound", 
                                                min = 0, max = 800, value = 700, step = 10),
                                    sliderInput(inputId = "SAT_avg_reading_lw", label = "Average SAT READING Lower Bound", 
                                                min = 0, max = 800, value = 500, step = 10),
                                    sliderInput(inputId = "SAT_avg_writing_up", label = "Average SAT WRITING Upper Bound", 
                                                min = 0, max = 2200, value = 2100, step = 10),
                                    sliderInput(inputId = "SAT_avg_writing_lw", label = "Average SAT WRITING Lower Bound", 
                                                min = 0, max = 2200, value = 400, step = 10),
                                    sliderInput(inputId = "ACT_avg_composite_up", label = "Average ACT Upper Bound",
                                                min = 0, max = 36, value = 30, step = 1),
                                    sliderInput(inputId = "ACT_avg_composite_lw", label = "Average ACT Lower Bound",
                                                min = 0, max = 36, value = 0, step = 1),
                                    sliderInput(inputId = "ACT_avg_math_up", label = "Average ACT MATH Upper Bound",
                                                min = 0, max = 36, value = 30, step = 1),
                                    sliderInput(inputId = "ACT_avg_math_lw", label = "Average ACT MATH Lower Bound",
                                                min = 0, max = 36, value = 0, step = 1),
                                    sliderInput(inputId = "ACT_avg_eng_up", label = "Average ACT ENGLISH Upper Bound",
                                                min = 0, max = 36, value = 30, step = 1),
                                    sliderInput(inputId = "ACT_avg_eng_lw", label = "Average ACT ENGLISH Lower Bound",
                                                min = 0, max = 36, value = 0, step = 1),
                                    sliderInput(inputId = "ACT_avg_writing_up", label = "Average ACT WRITING Upper Bound",
                                                min = 0, max = 36, value = 30, step = 1),
                                    sliderInput(inputId = "ACT_avg_writing_lw", label = "Average ACT WRITING Lower Bound",
                                                min = 0, max = 36, value = 0, step = 1)
                            ),
                            
                            mainPanel(
                                    dataTableOutput("college_list")
                            )
                   ),
                   
                   tabPanel("GPA Distributions",
                            selectInput(inputId = "school_name_gpa", label = "Type School Name", choices = unique(DT$school_name)),
                            plotOutput("gpa_density"))
                   
        ),

        navbarMenu("Campus Life",
                tabPanel(title = "Club Sports",
                         selectInput(inputId = "athletic_school_name", 
                                     selected = "University of Michigan -- Ann Arbor", 
                                     label = "Type School Name", 
                                     choices = DT$school_name),
                         
                         column(6,tableOutput("men_club_sports")),
                         column(6,tableOutput("women_club_sports"))
                         
                         ),

                tabPanel(title = "Student Organizations"
                         
                         )
        )
        
)
)

