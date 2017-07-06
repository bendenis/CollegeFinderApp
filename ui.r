library(shiny)
#library(wohdMyKlovr)
library(dplyr)
DT = data.table::fread("/Users/bendenisshaffer/Dropbox/myKlovr/CollegeFinder/Data/Interim/collegeFinderDataInjected.csv")


shinyUI(navbarPage(title = "College Finder App",
        
        tabPanel("Academic",
                sidebarPanel(
                        selectInput(inputId = "state", label = "Chose State", choices = unique(DT$state)),
                        sliderInput(inputId = "SAT_up", label = "SAT upper", min = 0, max = 800, value = 700, step = 10),
                        sliderInput(inputId = "SAT_lw", label = "SAT lower", min = 0, max = 800, value = 500, step = 10),
                        checkboxInput(inputId = "four_year", label = "4 Year"),
                        checkboxInput(inputId = "two_year", label = "2 Year"),
                        checkboxInput(inputId = "private", label = "Private"),
                        checkboxInput(inputId = "public", label = "Public")
                ),

                mainPanel(
                        tableOutput("college_list"),
                        plotOutput("map")
                )
        ),

        navbarMenu("Campus Life",
                tabPanel(title = "Club Sports",
                         selectInput(inputId = "athletic_school_name", 
                                     selected = "Harvard University", 
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

