library(shiny)
library(dplyr)
library(leaflet)
library(shinythemes)
DT = data.table::fread("/Users/bendenisshaffer/Dropbox/myKlovr/CollegeFinder/Data/Interim/collegeFinderDataInjected.csv")


shinyUI(navbarPage(title = "College Finder App",theme = shinytheme("cyborg"),
        
        tabPanel("Academic",
                sidebarPanel(
                        selectInput(inputId = "state", label = "Chose State", choices = unique(DT$state), multiple = T, selected = c("MA","NY")),
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
                                     selected = "University of Michigan", 
                                     label = "Type School Name", 
                                     choices = DT$school_name),
                         
                         column(6,tableOutput("men_club_sports")),
                         column(6,tableOutput("women_club_sports"))
                         
                         ),

                tabPanel(title = "Student Organizations"
                         
                         )
        ),
        
        tabPanel(title = "Map",
                 leafletOutput("college_map")
                 )
        
)
)

