library(shiny)
library(htmltools)
library(stringr)
library(data.table)


shinyServer(function(input,output) {
        
        dt1 = reactive({
                tb = DT %>% filter(Region == input$region, state == input$state,
                              four_year == input$four_year | two_year == input$two_year,
                              public_or_private == input$private | public_or_private != input$public,
                              ugrad_total_pop <= input$ugrad_total_pop, 
                              coed == input$coed,
                              religious_affiliation == input$religious,
                              specialized == input$specialized) %>% 
                        arrange(uni_ranking) %>% as.data.frame()
                tb[sapply(tb$subjects_offered, function(x) str_detect(x, input$major_selecotr)), ]
        })
        
        output$school_size = renderDataTable({
                dt1() %>% select(school_name, city, ugrad_total_pop, uni_ranking)
        },
        options = list(pageLength = 10))
        
        output$college_map = renderLeaflet({
                
                popup = str_c(sep = "<br/>",
                              "<b><a href='http://www.myklovr.com'> MyKlovr </a></b>",
                              "1350 6th Ave.",
                              "New York, NY 98138")
                
                leaflet(data = dt1()) %>% 
                        setView(lng = mean(dt1()$Longitude, na.rm = T), lat = mean(dt1()$Latitude, na.rm = T), zoom = 7) %>% 
                        addTiles() %>%
                        addCircleMarkers(lng = ~Longitude, 
                                         lat = ~Latitude, 
                                         label = ~as.character(dt1()$school_name),
                                         clusterOptions = markerClusterOptions(zIndexOffset = 5),
                                         popup = popup)
        })
        
        dt2 = reactive({
                DT %>% filter(SAT_avg_composite <= input$SAT_avg_composite_up,
                              SAT_avg_composite >= input$SAT_avg_composite_lw,
                              SAT_avg_math <= input$SAT_avg_math_up, 
                              SAT_avg_math >= input$SAT_avg_math_lw,
                              SAT_avg_reading <= input$SAT_avg_reading_up,
                              SAT_avg_reading >= input$SAT_avg_reading_lw,
                              SAT_avg_writing <= input$SAT_avg_writing_up,
                              SAT_avg_writing >= input$SAT_avg_writing_lw,
                              ACT_avg_composite <= input$ACT_avg_composite_up,
                              ACT_avg_composite >= input$ACT_avg_composite_lw,
                              ACT_avg_math <= input$ACT_avg_math_up,
                              ACT_avg_math >= input$ACT_avg_math_lw,
                              ACT_avg_eng <= input$ACT_avg_eng_up,
                              ACT_avg_eng >= input$ACT_avg_eng_lw,
                              ACT_avg_writing <= input$ACT_avg_writing_up,
                              ACT_avg_writing >= input$ACT_avg_writing_lw) %>% 
                        arrange(uni_ranking) %>%
                        select(school_name, state, city,
                               SAT_range_upper, SAT_range_lower, 
                               ACT_range_upper, ACT_range_lower)
        })
        

        output$college_list = renderDataTable({
                dt2()
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
        
        output$firm_list = renderTable({
                DT %>% filter(school_name == athletic_school_name()) %>%
                                                 select(firm_list)
        })
        
        output$grad_list = renderTable({
                DT %>% filter(school_name == athletic_school_name()) %>%
                        select(grad_list)
        })
        
        output$gpa_density = renderPlot({
                gpa_dt = DT %>% select(school_name,
                                       GPA_3.75_higher, GPA_3.50_3.74, 
                                       GPA_3.25_3.49, GPA_3_3.24, GPA_3_3.24,
                                       GPA_2.50_2.99, GPA_2.0_2.49, GPA_1.0_1.99, GPA_below_1)
                sp = c(3.875,3.625,3.37,3.12, 2.745, 2.25, 1.495, 0.5)
                pr = as.numeric(gpa_dt[school_name == input$school_name_gpa,-1]/100)
                dn = sample(sp, size = 10000, prob = pr, replace = T)
                g = ggplot(data.frame(dn), aes(x = dn)) + 
                        stat_density(bw = 0.2, alpha = 0.5) + 
                        xlim(2,4)
                g
        })
        
        output$diversity_plot = renderPlotly({
                diversity = DT[DT$school_name == input$athletic_school_name,c(81:83,164:172)]
                dv = melt(diversity)
                dv$tp = c(rep("M/F",2),"Total",rep("ByRace",9))
                ggplot(dv, aes(x = tp, y = value, fill = variable)) +
                        geom_bar(stat = "identity")
        })
        
}
)