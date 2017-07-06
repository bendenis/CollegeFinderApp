library(shiny)


shinyServer(function(input,output) {
        
        dt1 = reactive({
                DT %>% filter(Region == input$region, state == input$state,
                              four_year == input$four_year | two_year == input$two_year,
                              public_or_private == input$private | public_or_private != input$public,
                              ugrad_total_pop <= input$ugrad_total_pop, 
                              coed == input$coed) %>% 
                        arrange(uni_ranking)
        })
        
        output$school_size = renderDataTable({
                dt1() %>% select(school_name, city, ugrad_total_pop, uni_ranking, college_ranking)
        })
        
        output$college_map = renderLeaflet({
                leaflet(data = dt1()) %>% 
                        setView(lng = mean(dt1()$Longitude, na.rm = T), lat = mean(dt1()$Latitude, na.rm = T), zoom = 7) %>% 
                        addTiles() %>%
                        addCircleMarkers(lng = ~Longitude, 
                                         lat = ~Latitude, 
                                         label = ~as.character(dt1()$school_name),
                                         clusterOptions = markerClusterOptions(zIndexOffset = 5))
        })
        
        dt2 = reactive({
                DT %>% filter(Region == input$region, state == input$state, 
                              SAT_avg_math <= input$SAT_up, 
                              SAT_avg_math >= input$SAT_lw,
                              four_year == input$four_year | two_year == input$two_year,
                              public_or_private == input$private | public_or_private != input$public,
                              ugrad_total_pop <= input$ugrad_total_pop) %>% 
                        arrange(uni_ranking) %>%
                        select(school_name, state, city, uni_ranking, college_ranking, Longitude, Latitude)
        })
        

        output$college_list = renderTable({
                head(dt2(),50)
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