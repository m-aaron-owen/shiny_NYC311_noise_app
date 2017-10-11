library(ggplot2)

function(input, output, session) {
    

#===================================
# City Map Tab
#===================================    

#------------------
# Base Map
#------------------
    
    output$mymap = renderLeaflet({
    leaflet() %>%
        addProviderTiles("CartoDB.DarkMatter") %>%
        setView(lng = -73.92838, lat = 40.73009, zoom = 11)
    })

#------------------
# Map updating with user inputs
#------------------
        
    observeEvent(c(input$year1, input$month, input$complaint1, input$slider), {
        
        if (input$year1 == "All") {
            if (input$complaint1 == "All") {
                x = noise2 %>% 
                    filter(start_month == input$month) %>% 
                    na.omit()
            } else {
                x = noise2 %>% 
                    filter(start_month == input$month, Complaint.Type2 == input$complaint1) %>% 
                    na.omit()
            }
        } else {
            if (input$complaint1 == "All") {
                x = noise2 %>% 
                    filter(start_year == input$year1, start_month == input$month) %>% 
                    na.omit()
            } else {
                x = noise2 %>% 
                    filter(start_year == input$year1, start_month == input$month, Complaint.Type2 == input$complaint1) %>% 
                    na.omit()
            }
        }
        
        leafletProxy("mymap", data = x) %>%
            clearWebGLHeatmap() %>%
            addWebGLHeatmap(~Longitude, ~Latitude, size = input$slider, units = "p", opacity = 0.6)
    })    

        
#---------
# total complaints plot
#----------
        
    output$bar2 = renderPlot({
        if (input$year1 == "All") {
            if (input$complaint1 == "All") {
                x = noise2 %>%
                    group_by(Borough) %>%
                    summarise(count = n())
            } else {
                x = noise2 %>% 
                    filter(Complaint.Type2 == input$complaint1) %>%
                    group_by(Borough) %>%
                    summarise(count = n())
            }
        } else {
            if (input$complaint1 == "All") {
                x = noise2 %>%
                    filter(start_year == input$year1) %>%
                    group_by(Borough) %>%
                    summarise(count = n())
            } else {
                x = noise2 %>% 
                    filter(Complaint.Type2 == input$complaint1, start_year == input$year1) %>%
                    group_by(Borough) %>%
                    summarise(count = n())
            }
        }
            
        ggplot(x, aes(x = Borough, y = count)) +
            geom_bar(aes(fill = Borough), stat = "identity", show.legend = F) +
            labs(y = "Total Complaints") + theme(axis.title.x = element_blank()) +
            scale_x_discrete(labels = c("BNX", "BKN", "MAN", "QNS", "STI")) +
            scale_y_continuous(expand = c(0, 0), labels = scales::comma) +
            theme_bw() +
            theme(panel.border = element_blank(), panel.grid = element_blank(),
                  axis.line = element_line(color = "black"))
            
    }, height = 200, width = 200)  
        
#-----------
# complaints per square mile plot
#-----------

    output$bar = renderPlot({
        if (input$year1 == "All") {
            if (input$complaint1 == "All") {
                x = noise2 %>%
                    group_by(Borough, sq_miles) %>%
                    summarise(count = n()) %>%
                    summarise(comp_per_mile = count/sq_miles)
            } else {
                x = noise2 %>% 
                    filter(Complaint.Type2 == input$complaint1) %>%
                    group_by(Borough, sq_miles) %>%
                    summarise(count = n()) %>%
                    summarise(comp_per_mile = count/sq_miles)
            }
        } else {
            if (input$complaint1 == "All") {
                x = noise2 %>%
                    filter(start_year == input$year1) %>%
                    group_by(Borough, sq_miles) %>%
                    summarise(count = n()) %>%
                    summarise(comp_per_mile = count/sq_miles)
            } else {
                x = noise2 %>% 
                    filter(Complaint.Type2 == input$complaint1, start_year == input$year1) %>%
                    group_by(Borough, sq_miles) %>%
                    summarise(count = n()) %>%
                    summarise(comp_per_mile = count/sq_miles)
            }
        }

        ggplot(x, aes(x = Borough, y = comp_per_mile)) + 
            geom_bar(aes(fill = Borough), stat = "identity", show.legend = F) +
            labs(y = "Complaints per Square Mile") + theme(axis.title.x = element_blank()) + 
            scale_x_discrete(labels = c("BNX", "BKN", "MAN", "QNS", "STI")) + 
            scale_y_continuous(expand = c(0, 0), labels = scales::comma) +
            theme_bw() +
            theme(panel.border = element_blank(), panel.grid = element_blank(),
                  axis.line = element_line(color = "black"))
        
    }, height = 200, width = 200)
 
#----------
# complaints per capita plot
#----------

    output$bar3 = renderPlot({
        if (input$year1 == "All") {
            if (input$complaint1 == "All") {
                x = noise2 %>%
                    group_by(Borough, pop) %>%
                    summarise(count = n()) %>%
                    summarise(comp_per_capita = count/pop)
            } else {
                x = noise2 %>% 
                    filter(Complaint.Type2 == input$complaint1) %>%
                    group_by(Borough, pop) %>%
                    summarise(count = n()) %>%
                    summarise(comp_per_capita = count/pop)
            }
        } else {
            if (input$complaint1 == "All") {
                x = noise2 %>%
                    filter(start_year == input$year1) %>%
                    group_by(Borough, pop) %>%
                    summarise(count = n()) %>%
                    summarise(comp_per_capita = count/pop)
            } else {
                x = noise2 %>% 
                    filter(Complaint.Type2 == input$complaint1, start_year == input$year1) %>%
                    group_by(Borough, pop) %>%
                    summarise(count = n()) %>%
                    summarise(comp_per_capita = count/pop)
            }
        }
        
        ggplot(x, aes(x = Borough, y = comp_per_capita)) + 
            geom_bar(aes(fill = Borough), stat = "identity", show.legend = F) +
            labs(y = "Complaints per Capita") + theme(axis.title.x = element_blank()) + 
            scale_x_discrete(labels = c("BNX", "BKN", "MAN", "QNS", "STI")) + 
            scale_y_continuous(expand = c(0, 0), labels = scales::comma) +
            theme_bw() +
            theme(panel.border = element_blank(), panel.grid = element_blank(),
                  axis.line = element_line(color = "black"))
        
    }, height = 200, width = 200)
    
#===================================
# Complaint Trends Tab
#===================================
    
#---------
# complaints by month/hour plot
#---------
    
    output$barPlot = renderPlot({
        if (input$year2 == "All") {
            if (input$boro == "All") {
                if (input$complaint2 == "All") {
                    bar = noise2 %>%
                        group_by_(input$grouped) %>% 
                        summarise(count = n())
                } else {
                    bar = noise2 %>% 
                        filter(Complaint.Type2 == input$complaint2) %>% 
                        group_by_(input$grouped) %>% 
                        summarise(count = n())
                }
            } else {
                if (input$complaint2 == "All") {
                    bar = noise2 %>% 
                        filter(Borough == input$boro) %>% 
                        group_by_(input$grouped) %>% 
                        summarise(count = n())
                } else {
                    bar = noise2 %>% 
                        filter(Borough == input$boro, Complaint.Type2 == input$complaint2) %>% 
                        group_by_(input$grouped) %>% 
                        summarise(count = n())
                }
            }
        } else {
            if (input$boro == "All") {
                if (input$complaint2 == "All") {
                    bar = noise2 %>% 
                        filter(start_year == input$year2) %>% 
                        group_by_(input$grouped) %>% 
                        summarise(count = n())
                } else {
                    bar = noise2 %>% 
                        filter(start_year == input$year2, Complaint.Type2 == input$complaint2) %>% 
                        group_by_(input$grouped) %>% 
                        summarise(count = n())
                }
            } else {
                if (input$complaint2 == "All") {
                    bar = noise2 %>% 
                        filter(start_year == input$year2, Borough == input$boro) %>% 
                        group_by_(input$grouped) %>% 
                        summarise(count = n())
                } else {
                    bar = noise2 %>% 
                        filter(start_year == input$year2, Borough == input$boro, Complaint.Type2 == input$complaint2) %>% 
                        group_by_(input$grouped) %>% 
                        summarise(count = n())
                }
            }
        }

        
        # calculating expected probabilities for chi square test
        prob = rep(1/(nrow(bar)), nrow(bar))
        # performing chi square test to determine expected value
        res = chisq.test(bar$count, p = prob)
        
        if (input$grouped == "calendar_") {
            limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
            annotate_x = 6.5
            lab = "Month Complaint Filed"
        } else {
            limits = c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", 
                       "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23")
            annotate_x = 12.5
            lab = "Hour Complaint Filed"
        }
        
        ggplot(bar, aes_string(x = input$grouped, y = "count")) +
            geom_bar(aes_string(fill = input$grouped), stat = "identity", position = "dodge", show.legend = F) +
            geom_hline(yintercept = res$expected) +
            annotate("text", x = annotate_x, y = 1.05 * res$expected, 
                     label = paste("Goodness of Fit Expectation", round(res$expected))) +
            scale_x_discrete(limits = limits) +
            labs(x = lab, y = "Volume of Complaints") + 
            scale_y_continuous(expand = c(0, 0), labels = scales::comma) + 
            theme_bw() +
            theme(panel.border = element_blank(), panel.grid = element_blank(),
                  axis.line = element_line(color = "black"), axis.text = element_text(size = 12),
                  axis.title = element_text(size = 14, face = "bold"))
    })

#---------
# complaints grouped by year plot
#---------
    
    output$barPlot2 = renderPlot({
        if (input$boro == "All") {
            if (input$complaint2 == "All") {
                bar = noise2 %>%
                    group_by(start_year, Borough) %>%
                    summarise(count = n())
                
                ggplot(bar, aes(x = start_year, y = count)) +
                    geom_bar(aes(fill = Borough), stat = "identity", position = "stack") + 
                    xlab("Year Complaint Filed") +
                    scale_y_continuous(expand = c(0, 0), labels = scales::comma) + 
                    theme_bw() +
                    theme(panel.border = element_blank(), panel.grid = element_blank(),
                          axis.line = element_line(color = "black"), axis.text = element_text(size = 12),
                          axis.title.x = element_text(size = 14, face = "bold"), axis.title.y = element_blank()) +
                    scale_fill_brewer(type = "qual", palette = 7)
            } else {
                bar = noise2 %>%
                    filter(Complaint.Type2 == input$complaint2) %>%
                    group_by(start_year, Borough) %>%
                    summarise(count = n())
                
                ggplot(bar, aes(x = start_year, y = count)) +
                    geom_bar(aes(fill = Borough), stat = "identity", position = "stack") + 
                    xlab("Year Complaint Filed") +
                    scale_y_continuous(expand = c(0, 0), labels = scales::comma) + 
                    theme_bw() +
                    theme(panel.border = element_blank(), panel.grid = element_blank(),
                          axis.line = element_line(color = "black"), axis.text = element_text(size = 12),
                          axis.title.x = element_text(size = 14, face = "bold"), axis.title.y = element_blank()) +
                    scale_fill_brewer(type = "qual", palette = 7)
            }
        } else {
            if (input$complaint2 == "All") {
                bar = noise2 %>%
                    filter(Borough == input$boro) %>%
                    group_by(start_year) %>%
                    summarise(count = n())
                
                ggplot(bar, aes(x = start_year, y = count)) +
                    geom_bar(aes(fill = start_year), stat = "identity", position = "dodge", show.legend = F) + 
                    xlab("Year Complaint Filed") +
                    scale_y_continuous(expand = c(0, 0), labels = scales::comma) + 
                    theme_bw() +
                    theme(panel.border = element_blank(), panel.grid = element_blank(),
                          axis.line = element_line(color = "black"), axis.text = element_text(size = 12),
                          axis.title.x = element_text(size = 14, face = "bold"), axis.title.y = element_blank()) +
                    scale_fill_brewer(type = "qual", palette = 7)
                
            } else {
                bar = noise2 %>%
                    filter(Borough == input$boro, Complaint.Type2 == input$complaint2) %>%
                    group_by(start_year) %>%
                    summarise(count = n())
                
                ggplot(bar, aes(x = start_year, y = count)) +
                    geom_bar(aes(fill = start_year), stat = "identity", position = "dodge", show.legend = F) + 
                    xlab("Year Complaint Filed") +
                    scale_y_continuous(expand = c(0, 0), labels = scales::comma) + 
                    theme_bw() +
                    theme(panel.border = element_blank(), panel.grid = element_blank(),
                          axis.line = element_line(color = "black"), axis.text = element_text(size = 12),
                          axis.title.x = element_text(size = 14, face = "bold"), axis.title.y = element_blank()) +
                    scale_fill_brewer(type = "qual", palette = 7)
            }
        }
    })
    
#---------
# Horizontal bar plot
#---------
    output$horizontal_bar = renderPlot({
        if (input$year2 == "All") {
            if (input$boro == "All") {
                x = noise2 %>% 
                    group_by(Complaint.Type2) %>% 
                    summarise(count = n())
            } else {
                x = noise2 %>% 
                    filter(Borough == input$boro) %>% 
                    group_by(Complaint.Type2) %>% 
                    summarise(count = n())
            }
        } else {
            if (input$boro == "All") {
                x = noise2 %>% 
                    filter(start_year == input$year2) %>% 
                    group_by(Complaint.Type2) %>% 
                    summarise(count = n())
            } else {
                x = noise2 %>% 
                    filter(start_year == input$year2, Borough == input$boro) %>% 
                    group_by(Complaint.Type2) %>%
                    summarise(count = n())
            }
        }
        
        x = transform(x, Complaint.Type2 = reorder(Complaint.Type2, count))
        
        ggplot(x, aes(x = Complaint.Type2, y = count, fill = Complaint.Type2)) + 
            geom_bar(stat = "identity", show.legend = F) +
            coord_flip() + scale_y_continuous(labels = scales::comma) +
            labs(x = "Complaint Type", y = "Volume of Complaints") +
            scale_y_continuous(expand = c(0, 0), labels = scales::comma) + 
            theme_bw() +
            theme(panel.border = element_blank(), panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
                  axis.line = element_line(color = "black"), axis.text = element_text(size = 12),
                  axis.title.x = element_text(size = 14, face = "bold"), axis.title.y = element_blank()) +
            scale_fill_brewer(palette = "Spectral")
    })
    
    
#===================================
# Resolution Times Tab
#===================================
    
#----------
# boxplot by boro
#----------

    output$boxPlot_boro = renderPlot({
        if (input$year3 == "All") {
            if (input$complaint3 == "All") {
                x = noise2 %>% filter(Complaint.Type != "Survey") %>% group_by(Borough)
            } else {
                x = noise2 %>% filter(Complaint.Type != "Survey", Complaint.Type2 == input$complaint3) %>% group_by(Borough)
            }
        } else {
            if (input$complaint3 == "All") {
                x = noise2 %>% 
                    filter(Complaint.Type != "Survey", start_year == input$year3) %>% 
                    group_by(Borough)
            } else {
                x = noise2 %>% 
                    filter(Complaint.Type != "Survey", start_year == input$year3, Complaint.Type2 == input$complaint3) %>% 
                    group_by(Borough)
            }
        }

        ggplot(x, aes(x = Borough, y = time_diff/60/60)) +
            geom_boxplot(aes(col = Borough), width = 0.3, show.legend = F, outlier.shape = 21) +
            scale_y_log10(labels = scales::comma) + 
            labs(y = "Log Time for\nComplaint Resolution in Hours", x = "Borough") +
            theme_bw() +
            scale_x_discrete(labels = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")) +
            theme(panel.border = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_line(color = "gray"), panel.grid.minor.y = element_line(color = "gray"),
                  axis.line = element_line(color = "black"), axis.text = element_text(size = 12),
                  axis.title= element_text(size = 14, face = "bold"))
        
    }, height = 275)
    
#---------- 
# box plot by complaint
#----------
    
    output$boxPlot_complaint = renderPlot({
        if (input$year3 == "All") {
            if(input$boro2 == "All") {
                x = noise2 %>% filter(Complaint.Type != "Survey")
            } else {
                x = noise2 %>% filter(Complaint.Type != "Survey", Borough == input$boro2)
            }
        } else {
            if (input$boro2 == "All") {
                x = noise2 %>% filter(Complaint.Type != "Survey", start_year == input$year3)
            } else {
                x = noise2 %>% filter(Complaint.Type != "Survey", Borough == input$boro2, start_year == input$year3)
            }
        }
        
        ggplot(x, aes(x = Complaint.Type2, y = (time_diff/60/60))) + 
            geom_boxplot(aes(col = Complaint.Type2), show.legend = F) +
            geom_boxplot(aes(fill = Complaint.Type2), show.legend = F, outlier.shape = 21) +
            scale_y_log10(labels = scales::comma) + 
            labs(y = "Log Time for\nComplaint Resolution in Hours", x = "Complaint Type") +
            theme_bw() +
            theme(panel.border = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_line(color = "gray"), panel.grid.minor.y = element_line(color = "gray"),
                  axis.line = element_line(color = "black"), axis.text = element_text(size = 12),
                  axis.text.x = element_text(angle = -45, vjust = 0.9, hjust = 0.1),
                  axis.title = element_text(size = 14, face = "bold")) +
            scale_fill_brewer(palette = "Spectral")
        
    }, height = 350)
}