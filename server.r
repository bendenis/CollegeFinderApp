library(shiny)
library(htmltools)
library(stringr)
library(data.table)
library(dplyr)
DT = fread("Data/D8.csv")

shinyServer(function(input,output) {
        
        #### PAGE 1
        
        dt1 = reactive({
                tb = DT %>% filter(Region == input$region, State == input$state,
                              FourYear == input$four_year | TwoYear == input$two_year,
                              PublicPrivate == input$private | PublicPrivate != input$public,
                              UgradTotalPop <= input$ugrad_total_pop, 
                              COED == input$coed) %>% 
                        arrange(rank_overall) %>% as.data.frame()
                
                if(input$religious == 1){
                        tb = tb[tb$ReligiousAffiliation == 1,]
                }
                
                if(input$specialized == 1){
                        tb = tb[tb$Specialized == 1,]
                }
                
                tb
        })
        
        output$list1 = renderDataTable({
                dt1() %>% select(SchoolName, City, rank_overall, SelectivityRank)
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
                                         label = ~as.character(dt1()$SchoolName),
                                         clusterOptions = markerClusterOptions(zIndexOffset = 5),
                                         popup = popup)
        })
        
        
        #### PAGE 2 ACADEMIC
        
        dt2 = reactive({
                dt = dt1() %>% filter(SATAverageComposite <= input$SAT_avg_composite_up,
                                      SATAverageComposite >= input$SAT_avg_composite_lw,
                                      SATAverageMath <= input$SAT_avg_math_up, 
                                      SATAverageMath >= input$SAT_avg_math_lw,
                                      SATAverageReading <= input$SAT_avg_reading_up,
                                      SATAverageReading >= input$SAT_avg_reading_lw,
                                      SATAverageWriting <= input$SAT_avg_writing_up,
                                      SATAverageWriting >= input$SAT_avg_writing_lw,
                                      ACTAverageComposite <= input$ACT_avg_composite_up,
                                      ACTAverageComposite >= input$ACT_avg_composite_lw,
                                      ACTAverageMath <= input$ACT_avg_math_up,
                                      ACTAverageMath >= input$ACT_avg_math_lw,
                                      ACTAverageEnglish <= input$ACT_avg_eng_up,
                                      ACTAverageEnglish >= input$ACT_avg_eng_lw,
                                      ACTAverageWriting <= input$ACT_avg_writing_up,
                                      ACTAverageWriting >= input$ACT_avg_writing_lw) %>% 
                        arrange(rank_overall)
                
                dt[sapply(dt$subjects_offered, function(x) str_detect(x, input$major_selecotr)), ]
        })

        output$list2 = renderDataTable({
                dt2() %>% select(SchoolName, State, City,
                                 SATRangeUpper, SATRangeLower, 
                                 ACTRangeUpper, ACTRangeLower,
                                 SelectivityRank)
        },
        options = list(pageLength = 20))
        
        output$list2sat = renderDataTable({
                dt2() %>% select(SchoolName, State, City,
                                 SATRangeUpper, SATRangeLower,
                                 SelectivityRank)
        },
        options = list(pageLength = 20))
        output$list2act = renderDataTable({
                dt2() %>% select(SchoolName, State, City,
                                 ACTRangeUpper, ACTRangeLower,
                                 SelectivityRank)
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
                                         label = ~as.character(dt2()$SchoolName),
                                         clusterOptions = markerClusterOptions(zIndexOffset = 5),
                                         popup = popup)
        })
        
        output$gpa_density = renderPlot({
                gpa_dt = DT %>% select(SchoolName,
                                       GPA_3.75_higher, GPA_3.50_3.74, 
                                       GPA_3.25_3.49, GPA_3_3.24, GPA_3_3.24,
                                       GPA_2.50_2.99, GPA_2.0_2.49, GPA_1.0_1.99, GPA_below_1)
                sp = c(3.875,3.625,3.37,3.12, 2.745, 2.25, 1.495, 0.5)
                pr = as.numeric(gpa_dt[SchoolName == input$school_name_gpa,-1]/100)
                dn = sample(sp, size = 10000, prob = pr, replace = T)
                g = ggplot(data.frame(dn), aes(x = dn)) + 
                        stat_density(bw = 0.2, alpha = 0.5) + 
                        xlim(2,4)
                g
        })

        #### PAGE 3
        
        dt3 = reactive({
                tb = dt2() %>% filter(InstateTuition <= input$tuition)
                
                if(input$nb_scholarship == 1){
                        tb = tb[tb$NeedBasedScholarship == 1,]
                }
                if(input$SEOG_scholarship == 1){
                        tb = tb[tb$SEOGScholarship == 1,]
                }
                if(input$state_scholarship == 1){
                        tb = tb[tb$StateScholarship == 1,]
                }
                if(input$college_scholarship == 1){
                        tb = tb[tb$CollegeScholarship == 1,]
                }
                if(input$private_scholarship == 1){
                        tb = tb[tb$PrivateScholarship == 1,]
                }
                if(input$nursing_scholarship == 1){
                        tb = tb[tb$NursingScholarship == 1,]
                }
                if(input$pell_grant_scholarship == 1){
                        tb = tb[tb$PellGrantScholarship == 1,]
                }
                if(input$united_negro_scholarship == 1){
                        tb = tb[tb$UnitedNegroScholarship == 1,]
                }
                tb
                
        })
        
        output$list3 = renderDataTable({
                dt3() %>% select(SchoolName, City, rank_overall, InstateTuition, OutstateTuition, SelectivityRank)
                
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
                                         label = ~as.character(dt3()$SchoolName),
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
                                         label = ~as.character(dt4()$SchoolName),
                                         clusterOptions = markerClusterOptions(zIndexOffset = 5),
                                         popup = popup)
        })
                
        athletic_school_name = reactive({input$athletic_school_name})

        # output$men_club_sports = renderTable({
        #         DT %>% filter(SchoolName == athletic_school_name()) %>%
        #                 select(MenClubSports)
        # })
        # 
        # output$women_club_sports = renderTable({
        #         DT %>% filter(SchoolName == athletic_school_name()) %>%
        #                 select(WomenClubSports)
        # })
        # 
        # output$firm_list = renderTable({
        #         DT %>% filter(SchoolName == athletic_school_name()) %>%
        #                                          select(FirmList)
        # })
        # 
        # output$grad_list = renderTable({
        #         DT %>% filter(SchoolName == athletic_school_name()) %>%
        #                 select(GradSchoolList)
        # })
        # 
        # output$alumni_list = renderTable({
        #         DT %>% filter(SchoolName == athletic_school_name()) %>%
        #                 select(NotableAlumni)
        # })
        # 
        # output$diversity_plot = renderPlotly({
        #         diversity = DT[DT$SchoolName == input$athletic_school_name,c(81:83,164:172)]
        #         dv = melt(diversity)
        #         dv$tp = c(rep("M/F",2),"Total",rep("ByRace",9))
        #         ggplot(dv, aes(x = tp, y = value, fill = variable)) +
        #                 geom_bar(stat = "identity")
        # })
        # 
        # output$handicapped_services = renderTable({
        #         
        #         ix = which(DT$SchoolName == athletic_school_name())
        #         ls = names(DT[ix,c(55:65)])[which(DT[ix,c(55:65)] == 1)]
        #         ls
        # })
        
        output$list4 =  renderDataTable({
                dt4() %>% select(SchoolName, City, rank_overall, InstateTuition, OutstateTuition, SelectivityRank)
                
        },
        options = list(pageLength = 10))
        
        
        #### PAGE 5 HOUSING
        
        dt5 = reactive({
                tb = dt4()
                
                if(input$offered_housing == 1){
                        tb = tb[tb$HousingOffered == 1,]
                }
                if(input$coed_housing == 1){
                        tb = tb[tb$COEDHousing == 1,]
                }
                if(input$men_only_housing == 1){
                        tb = tb[tb$MenOnlyHousing == 1,]
                }
                if(input$women_only_housing == 1){
                        tb = tb[tb$WomenOnlyHousing == 1,]
                }
                if(input$frat_housing == 1){
                        tb = tb[tb$FratHousing == 1,]
                }
                if(input$sorority_housing == 1){
                        tb = tb[tb$SororityHousing == 1,]
                }
                if(input$disability_housing == 1){
                        tb = tb[tb$DisabilityHousing == 1,]
                }
                if(input$international_student_housing == 1){
                        tb = tb[tb$InternationalStudentHousing == 1,]
                }
                if(input$cooperative_housing == 1){
                        tb = tb[tb$CooperativeHousing == 1,]
                }
                if(input$single_student_appartment == 1){
                        tb = tb[tb$SingleStudentAppartment == 1,]
                }
                if(input$married_student_appartment == 1){
                        tb = tb[tb$MarriedStudentAppartment == 1,]
                }
                tb
                
        })
        
        output$list5 =  renderDataTable({
                dt5() %>% select(SchoolName, City, rank_overall, InstateTuition, OutstateTuition, SelectivityRank)},
        options = list(pageLength = 10))
        
        
}
)