library(shiny)
library(htmltools)
library(stringr)
library(data.table)


shinyServer(function(input,output) {
        
        #### PAGE 1
        
        dt1 = reactive({
                tb = DT %>% filter(Region == input$region, state == input$state,
                              four_year == input$four_year | two_year == input$two_year,
                              public_or_private == input$private | public_or_private != input$public,
                              ugrad_total_pop <= input$ugrad_total_pop, 
                              coed == input$coed) %>% 
                        arrange(uni_ranking) %>% as.data.frame()
                
                if(input$religious == 1){
                        tb = tb[tb$religious_affiliation == 1,]
                }
                
                if(input$specialized == 1){
                        tb = tb[tb$specialized == 1,]
                }
                
                #tb[sapply(tb$subjects_offered, function(x) str_detect(x, input$major_selecotr)), ]
                tb
        })
        
        output$list1 = renderDataTable({
                dt1() %>% select(school_name, city, uni_ranking, selectivity_rank)
        },
        options = list(pageLength = 20))
        
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
        
        
        #### PAGE 2 ACADEMIC
        
        dt2 = reactive({
                dt = dt1() %>% filter(SAT_avg_composite <= input$SAT_avg_composite_up,
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
                        arrange(uni_ranking)
                
                dt[sapply(dt$subjects_offered, function(x) str_detect(x, input$major_selecotr)), ]
        })

        output$list2 = renderDataTable({
                dt2() %>% select(school_name, state, city,
                               SAT_range_upper, SAT_range_lower, 
                               ACT_range_upper, ACT_range_lower,
                               selectivity_rank)
        },
        options = list(pageLength = 20))
        
        output$list2sat = renderDataTable({
                dt2() %>% select(school_name, state, city,
                                 SAT_range_upper, SAT_range_lower,
                                 selectivity_rank)
        },
        options = list(pageLength = 20))
        output$list2act = renderDataTable({
                dt2() %>% select(school_name, state, city,
                                 ACT_range_upper, ACT_range_lower,
                                 selectivity_rank)
        },
        options = list(pageLength = 20))
        
        output$college_map2 = renderLeaflet({
                
                popup = str_c(sep = "<br/>",
                              "<b><a href='http://www.myklovr.com'> MyKlovr </a></b>",
                              "1350 6th Ave.",
                              "New York, NY 98138")
                
                leaflet(data = dt2()) %>% 
                        setView(lng = mean(dt2()$Longitude, na.rm = T), lat = mean(dt2()$Latitude, na.rm = T), zoom = 7) %>% 
                        addTiles() %>%
                        addCircleMarkers(lng = ~Longitude, 
                                         lat = ~Latitude, 
                                         label = ~as.character(dt2()$school_name),
                                         clusterOptions = markerClusterOptions(zIndexOffset = 5),
                                         popup = popup)
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

        #### PAGE 3
        
        dt3 = reactive({
                tb = dt2() %>% filter(tuition_instate <= input$tuition)
                
                if(input$nb_scholarship == 1){
                        tb = tb[tb$nb_scholarship == 1,]
                }
                if(input$SEOG_scholarship == 1){
                        tb = tb[tb$SEOG_scholarship == 1,]
                }
                if(input$state_scholarship == 1){
                        tb = tb[tb$state_scholarship == 1,]
                }
                if(input$college_scholarship == 1){
                        tb = tb[tb$college_scholarship == 1,]
                }
                if(input$private_scholarship == 1){
                        tb = tb[tb$private_scholarship == 1,]
                }
                if(input$nursing_scholarship == 1){
                        tb = tb[tb$SEOG_scholarship == 1,]
                }
                if(input$nursing_scholarship == 1){
                        tb = tb[tb$SEOG_scholarship == 1,]
                }
                if(input$united_negro_scholarship == 1){
                        tb = tb[tb$united_negro_scholarship == 1,]
                }
                tb
                
        })
        
        output$list3 = renderDataTable({
                dt3() %>% select(school_name, city, uni_ranking, tuition_instate, tuition_outstate, selectivity_rank)
                
        },
        options = list(pageLength = 20))
        
        output$college_map3 = renderLeaflet({
                
                popup = str_c(sep = "<br/>",
                              "<b><a href='http://www.myklovr.com'> MyKlovr </a></b>",
                              "1350 6th Ave.",
                              "New York, NY 98138")
                
                leaflet(data = dt3()) %>% 
                        setView(lng = mean(dt3()$Longitude, na.rm = T), lat = mean(dt3()$Latitude, na.rm = T), zoom = 7) %>% 
                        addTiles() %>%
                        addCircleMarkers(lng = ~Longitude, 
                                         lat = ~Latitude, 
                                         label = ~as.character(dt3()$school_name),
                                         clusterOptions = markerClusterOptions(zIndexOffset = 5),
                                         popup = popup)
        })
        
        #### PAGE 4
        
        dt4 = reactive({
                tb = dt3()
                
                if(input$handicapped_braille == 1){
                        tb = tb[tb$handicapped_braille == 1,]
                }
                if(input$handicapped_equipment == 1){
                        tb = tb[tb$handicapped_equipment == 1,]
                }
                if(input$handicapped_housing == 1){
                        tb = tb[tb$handicapped_housing == 1,]
                }
                if(input$handicapped_interpeter == 1){
                        tb = tb[tb$handicapped_interpeter == 1,]
                }
                if(input$handicapped_notetaking == 1){
                        tb = tb[tb$handicapped_notetaking == 1,]
                }
                if(input$handicapped_reader == 1){
                        tb = tb[tb$handicapped_reader == 1,]
                }
                if(input$handicapped_talkbook == 1){
                        tb = tb[tb$handicapped_talkbook == 1,]
                }
                if(input$handicapped_taperecorder == 1){
                        tb = tb[tb$handicapped_taperecorder == 1,]
                }
                tb
                
        })
        
        output$college_map4 = renderLeaflet({
                
                popup = str_c(sep = "<br/>",
                              "<b><a href='http://www.myklovr.com'> MyKlovr </a></b>",
                              "1350 6th Ave.",
                              "New York, NY 98138")
                
                leaflet(data = dt4()) %>% 
                        setView(lng = mean(dt3()$Longitude, na.rm = T), lat = mean(dt4()$Latitude, na.rm = T), zoom = 7) %>% 
                        addTiles() %>%
                        addCircleMarkers(lng = ~Longitude, 
                                         lat = ~Latitude, 
                                         label = ~as.character(dt4()$school_name),
                                         clusterOptions = markerClusterOptions(zIndexOffset = 5),
                                         popup = popup)
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
        
        output$alumni_list = renderTable({
                DT %>% filter(school_name == athletic_school_name()) %>%
                        select(alum_list)
        })
        
        output$diversity_plot = renderPlotly({
                diversity = DT[DT$school_name == input$athletic_school_name,c(81:83,164:172)]
                dv = melt(diversity)
                dv$tp = c(rep("M/F",2),"Total",rep("ByRace",9))
                ggplot(dv, aes(x = tp, y = value, fill = variable)) +
                        geom_bar(stat = "identity")
        })
        
        output$handicapped_services = renderTable({
                
                ix = which(DT$school_name == athletic_school_name())
                ls = names(DT[ix,c(55:65)])[which(DT[ix,c(55:65)] == 1)]
                ls
        })
        
        output$list4 =  renderDataTable({
                dt4() %>% select(school_name, city, uni_ranking, tuition_instate, tuition_outstate, selectivity_rank)
                
        },
        options = list(pageLength = 10))
        
}
)