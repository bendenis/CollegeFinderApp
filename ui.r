library(shiny)
library(dplyr)
library(leaflet)
library(shinythemes)
library(ggplot2)
library(plotly)
library(data.table)
library(stringr)
DT = fread("Data/D8.csv")


shinyUI(navbarPage(title = "College Finder App",theme = shinytheme("united"),
        
        tabPanel("School Type",
                 sidebarPanel(
                         h1("Location"),
                         selectInput(inputId = "region", label = "Region",
                                     choices = unique(DT$Region), selected = "N"),
                         selectInput(inputId = "state", label = "Chose State", 
                                     choices = unique(DT$State), multiple = T, selected = c("NY")),
                         h1("School Type"),
                         sliderInput(inputId = "ugrad_total_pop", label = "School Size", 
                                     min = 0, max = 70000, step = 20, value = 40000),
                         checkboxInput(inputId = "four_year", label = "4 Year"),
                         checkboxInput(inputId = "two_year", label = "2 Year"),
                         checkboxInput(inputId = "private", label = "Private"),
                         checkboxInput(inputId = "public", label = "Public"),
                         checkboxInput(inputId = "religious", label = "Has Religious Affiliation"),
                         checkboxInput(inputId = "specialized", label = "Specialized School"),
                         radioButtons(inputId = "coed", label = "Co-Educational:",
                                      choices = c("COED" = 3,
                                                  "All Female" = 1,
                                                  "All Male" = 2))
                 ),
                 
                 mainPanel(
                         leafletOutput("college_map"),
                         dataTableOutput("list1")
                 )
                 
                 ),
        
        navbarMenu(title = "Academic",   
                   tabPanel("Majors",
                            sidebarPanel(
                                    h1("Majors"),
                                    p("Select multiple majors that you are interested in."),
                                    selectInput(inputId = "major_selecotr", label = "Choose Majors", 
                                                choices = str_trim(unique(unlist(str_split(DT$subjects_offered, pattern = ","))))[-1],
                                                multiple = T, selected = "Mathematics & Statistics")
                            ),
                            
                            mainPanel(
                                    leafletOutput("college_map2"),
                                    dataTableOutput("list2")
                            )
                   ),
                   
                   tabPanel("SAT",
                            sidebarPanel(
                                    sliderInput(inputId = "SAT_avg_composite_up", label = "Average SAT Upper Bound", 
                                                min = 0, max = 1600, value = 1600, step = 10),
                                    sliderInput(inputId = "SAT_avg_composite_lw", label = "Average SAT Lower Bound", 
                                                min = 0, max = 1600, value = 0, step = 10),
                                    sliderInput(inputId = "SAT_avg_math_up", label = "Average SAT MATH Upper Bound", 
                                                min = 0, max = 800, value = 800, step = 10),
                                    sliderInput(inputId = "SAT_avg_math_lw", label = "Average SAT MATH Lower Bound", 
                                                min = 0, max = 800, value = 0, step = 10),
                                    sliderInput(inputId = "SAT_avg_reading_up", label = "Average SAT READING Upper Bound", 
                                                min = 0, max = 800, value = 800, step = 10),
                                    sliderInput(inputId = "SAT_avg_reading_lw", label = "Average SAT READING Lower Bound", 
                                                min = 0, max = 800, value = 0, step = 10),
                                    sliderInput(inputId = "SAT_avg_writing_up", label = "Average SAT WRITING Upper Bound", 
                                                min = 0, max = 2200, value = 2200, step = 10),
                                    sliderInput(inputId = "SAT_avg_writing_lw", label = "Average SAT WRITING Lower Bound", 
                                                min = 0, max = 2200, value = 0, step = 10)
                                    ),
                            
                            mainPanel(
                                    dataTableOutput("list2sat")
                            )
                            
                            ),
                   
                   tabPanel("ACT",
                            sidebarPanel(
                                    sliderInput(inputId = "ACT_avg_composite_up", label = "Average ACT Upper Bound",
                                                min = 0, max = 36, value = 36, step = 1),
                                    sliderInput(inputId = "ACT_avg_composite_lw", label = "Average ACT Lower Bound",
                                                min = 0, max = 36, value = 0, step = 1),
                                    sliderInput(inputId = "ACT_avg_math_up", label = "Average ACT MATH Upper Bound",
                                                min = 0, max = 36, value = 36, step = 1),
                                    sliderInput(inputId = "ACT_avg_math_lw", label = "Average ACT MATH Lower Bound",
                                                min = 0, max = 36, value = 0, step = 1),
                                    sliderInput(inputId = "ACT_avg_eng_up", label = "Average ACT ENGLISH Upper Bound",
                                                min = 0, max = 36, value = 36, step = 1),
                                    sliderInput(inputId = "ACT_avg_eng_lw", label = "Average ACT ENGLISH Lower Bound",
                                                min = 0, max = 36, value = 0, step = 1),
                                    sliderInput(inputId = "ACT_avg_writing_up", label = "Average ACT WRITING Upper Bound",
                                                min = 0, max = 36, value = 36, step = 1),
                                    sliderInput(inputId = "ACT_avg_writing_lw", label = "Average ACT WRITING Lower Bound",
                                                min = 0, max = 36, value = 0, step = 1)
                                    ),
                            
                            mainPanel(
                                    dataTableOutput("list2act")
                            )
                            
                            ),
                   
                   
                   tabPanel("GPA Distributions",
                            selectInput(inputId = "school_name_gpa", label = "Type School Name", choices = unique(DT$SchoolName),
                                        selected = "University of Michigan -- Ann Arbor"),
                            plotOutput("gpa_density"))
                   
        ),
        
        tabPanel(title = "Financial",
                 sidebarPanel(
                        h1("Tuition"),
                        sliderInput(inputId = "tuition", label = "Maximum Tuition",
                             min = 0, max = 100000, value = 100000),
                        h1("Scholarships"),
                        checkboxInput(inputId = "nb_scholarship", label = "Need Based Scholarships"),
                        checkboxInput(inputId = "pell_grant_scholarship", label = "Pell Grant"),
                        checkboxInput(inputId = "SEOG_scholarship", label = "SEOG"),
                        checkboxInput(inputId = "state_scholarship", label = "State Scholarships"),
                        checkboxInput(inputId = "college_scholarship", label = "College Scholarships"),
                        checkboxInput(inputId = "private_scholarship", label = "Private Scholarships"),
                        checkboxInput(inputId = "nursing_scholarship", label = "Nursing Scholarships"),
                        checkboxInput(inputId = "united_negro_scholarship", label = "United Negro Scholarships"),
                        
                        h1("Loans"),
                        checkboxInput(inputId = "subsudised_stafford_loan", label = "Subsudised Stafford Loan"),
                        checkboxInput(inputId = "un_subsudised_stafford_loan", label = "Unsubsudised Stafford Loan"),
                        checkboxInput(inputId = "plus_loan", label = "PLUS Loan"),
                        checkboxInput(inputId = "perkins_loan", label = "Perkins Loan"),
                        checkboxInput(inputId = "nursing_loan", label = "Nursing Loan"),
                        checkboxInput(inputId = "state_loan", label = "State Loan"),
                        checkboxInput(inputId = "college_loan", label = "College Loan")
                        ),
                 
                 
                        mainPanel(
                                leafletOutput("college_map3"),
                                dataTableOutput("list3")
                        )
                 
                 ),
        
        navbarMenu(title = "Univesity Life",
                   
                   tabPanel(title = "Campus Accessibility",
                            sidebarPanel(
                                    h1("Disabilities"),
                                    checkboxInput(inputId = "handicapped_housing", label = "Handicapped Housing"),
                                    checkboxInput(inputId = "handicapped_notetaking", label = "Notetaking for Handicapped Students"),
                                    checkboxInput(inputId = "handicapped_taprecorder", label = "Taperecoding for Handicapped Students"),
                                    checkboxInput(inputId = "handicapped_tutor", label = "Tutoring for Handicapped Students"),
                                    checkboxInput(inputId = "handicapped_reader", label = "Reader for Handicapped Students"),
                                    checkboxInput(inputId = "handicapped_interpeter", label = "Interpreter for Handicapped Students"),
                                    checkboxInput(inputId = "handicapped_transport", label = "Transportation for Handicapped Students"),
                                    checkboxInput(inputId = "handicapped_taperecorder", label = "Taperecoding for Handicapped Students"),
                                    checkboxInput(inputId = "handicapped_equipment", label = "Equipment for Handicapped Students"),
                                    checkboxInput(inputId = "handicapped_braille", label = "Braille Code on Campus"),
                                    checkboxInput(inputId = "handicapped_talkbook", label = "Talkbook for Handicapped Students")
                            ),
                            
                            mainPanel(
                                    dataTableOutput("list4")
                            ),
                            
                            leafletOutput("college_map4"),
                            
                            selectInput(inputId = "athletic_school_name", 
                                        selected = "University of Michigan -- Ann Arbor", 
                                        label = "Type School Name", 
                                        choices = DT$SchoolName),
                            
                            fluidRow(
                                    h1("Club Sports"),
                                    column(4,tableOutput("men_club_sports")),
                                    column(4,tableOutput("women_club_sports")),
                                    h1("Notable Alumni"),
                                    column(4,tableOutput("alumni_list"))
                            ),
                            
                            fluidRow(
                                    h1("After Graduating"),
                                    column(6, tableOutput("firm_list")),
                                    column(6, tableOutput("grad_list"))
                            ),
                            
                            fluidRow(
                                    h1("Disability Services"),
                                    column(6, tableOutput("handicapped_services"))
                            ),
                            
                            fluidRow(
                                    h1("Diversity"),
                                    column(10,plotlyOutput("diversity_plot"))
                            )
                            
                        ),
                   
                   tabPanel(title = "Housing",
                           sidebarPanel(
                                   h1("Housing"),
                                   checkboxInput(inputId = "offered_housing", label = "Housing is Offered"),
                                   checkboxInput(inputId = "coed_housing", label = "COED Housing"),
                                   checkboxInput(inputId = "women_only_housing", label = "Women Only Housing"),
                                   checkboxInput(inputId = "men_only_housing", label = "Men Only Housing"),
                                   checkboxInput(inputId = "sorority_housing", label = "Sorority Housing"),
                                   checkboxInput(inputId = "frat_housing", label = "Fraternity Housing"),
                                   checkboxInput(inputId = "single_student_appartment", label = "Single Student Appartment"),
                                   checkboxInput(inputId = "married_student_appartment", label = "Married Student Appartment"),
                                   checkboxInput(inputId = "disability_housing", label = "Disability Housing"),
                                   checkboxInput(inputId = "international_student_housing", label = "International Student Housing"),
                                   checkboxInput(inputId = "cooperative_housing", label = "Cooperative Housing")
                           ),
                           
                           mainPanel(
                                   dataTableOutput("list5")
                           )
                           
                        )
                   )
        
)
)

