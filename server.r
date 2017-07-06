library(shiny)


shinyServer(function(input,output) {
        
        dt = reactive({
                DT %>% filter(state == input$state, 
                              SAT_avg_math <= input$SAT_up, 
                              SAT_avg_math >= input$SAT_lw,
                              four_year == input$four_year | two_year == input$two_year,
                              public_or_private == input$private | public_or_private != input$public) %>% 
                        arrange(uni_ranking) %>%
                        select(school_name, state, city, uni_ranking, Longitude, Latitude)
        })
        
        output$map = renderPlot({
                map_it(variable = dt()$uni_ranking, state = input$state, data = dt())
                })

        output$college_list = renderTable({
                head(dt(),50)
        })
        
        athletic_school_name = reactive({input$athletic_school_name})

        output$men_club_sports = renderTable({
                DT %>% filter(school_name == athletic_school_name()) %>%
                        select(men_club_sports)
        })

        output$women_club_sports = renderTable({
                DT %>% filter(school_name == athletic_school_name()) %>%
                        select(women_club_sports)
        })
}
)