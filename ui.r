library(shiny)
library(dplyr)
library(leaflet)
library(shinythemes)
library(ggplot2)
library(plotly)
library(data.table)
library(stringr)
DT = fread("Data/collegeFinderDataInjected.csv")


shinyUI(navbarPage(title = "College Finder App",theme = shinytheme("united"),
        
        tabPanel("School Type",
                 sidebarPanel(
                         h1("Location"),
                         selectInput(inputId = "region", label = "Region",
                                     choices = unique(DT$Region), selected = "N"),
                         selectInput(inputId = "state", label = "Chose State", 
                                     choices = unique(DT$state), multiple = T, selected = c("NY")),
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
                                    )
                            )#,
                   
                   
                   # tabPanel("GPA Distributions",
                   #          selectInput(inputId = "school_name_gpa", label = "Type School Name", choices = unique(DT$school_name),
                   #                      selected = "University of Michigan -- Ann Arbor"),
                   #          plotOutput("gpa_density"))
                   
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
                        checkboxInput(inputId = "united_negro_scholarship", label = "United Negro Scholarships")
                        ),
                 
                        mainPanel(
                                leafletOutput("college_map3"),
                                dataTableOutput("list3")
                        )
                 
                 ),

        tabPanel(title = "University Life",
                selectInput(inputId = "athletic_school_name", 
                            selected = "University of Michigan -- Ann Arbor", 
                            label = "Type School Name", 
                            choices = DT$school_name),
                         
                fluidRow(
                        column(4,tableOutput("men_club_sports")),
                        column(4,tableOutput("women_club_sports")),
                        column(4,tableOutput("alumni_list"))
                        ),
                         
                fluidRow(
                        column(6, tableOutput("firm_list")),
                        column(6, tableOutput("grad_list"))
                         ),
                         
                fluidRow(
                        column(10,plotlyOutput("diversity_plot"))
                         )
                         
        )
        
)
)

