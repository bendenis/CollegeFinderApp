library(shiny)
library(dplyr)
library(leaflet)
library(shinythemes)
library(ggplot2)
library(plotly)
library(data.table)
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
                         selectInput(inputId = "school_pop_to_plot", label = "Choose School",  
                                     choices = DT$school_name, selected = "University of Michigan -- Ann Arbor"),
                         checkboxInput(inputId = "four_year", label = "4 Year"),
                         checkboxInput(inputId = "two_year", label = "2 Year"),
                         checkboxInput(inputId = "private", label = "Private"),
                         checkboxInput(inputId = "public", label = "Public"),
                         radioButtons(inputId = "coed", label = "Co-Educational:",
                                      choices = c("COED" = 3,
                                                  "Female" = 1,
                                                  "Male" = 2))
                 ),
                 mainPanel(
                         dataTableOutput("school_size")
                 )
                 
                 ),
                   
        tabPanel(title = "Map",
                 leafletOutput("college_map")
        ),      
                   
        tabPanel("Academic",
                sidebarPanel(
                        selectInput(inputId = "state", label = "Chose State", choices = unique(DT$state), multiple = T, selected = c("MA","NY")),
                        sliderInput(inputId = "SAT_up", label = "SAT upper", min = 0, max = 800, value = 700, step = 10),
                        sliderInput(inputId = "SAT_lw", label = "SAT lower", min = 0, max = 800, value = 500, step = 10)
                ),

                mainPanel(
                        tableOutput("college_list")
                )
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

