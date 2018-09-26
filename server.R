###################
# server.R
# 
# For all your server needs 
###################

server <- function (input, output, session) {
  
  # Setting up User Token
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['userToken']])) {
      updateTextInput(session, "userToken", value = query[['userToken']])
    }
  })
  
  # Introduction
  output$Introduction.intro <- renderUI({ createIntroduction('Salutation!','Introduction Paragraph 1','Thankyou.') })
  
  # Simple Histogram 
  output$simpleHist.intro <- renderUI({ HTML("Introduction - Simple Histogram") })
  
  output$simpleHist.desc  <- renderUI({ HTML("Description - Simple Histogram") })
  
  output$simpleHist.plot  <- renderPlot({ createHist(userToken = input$userToken, data = toydata$cont1, color = "#007ba7", xlabel = "Histogram x label") })
  
  # Multi Histogram
  output$multiHist.intro <- renderUI({ HTML("Introduction - Multi Histrogram") })
  
  output$multiHist.desc1 <- renderUI({ HTML("Description - Multi Histrogram for Dimension 1") })
  output$multiHist.desc2 <- renderUI({ HTML("Description - Multi Histrogram for Dimension 2") })
  output$multiHist.desc3 <- renderUI({ HTML("Description - Multi Histrogram for Dimension 3") })
  
  output$multiHist.plot1 <- renderPlot({ createHist(userToken = input$userToken, data = toydata$cont1, color = "#007ba7", xlabel = "Scores in comparison group") })
  output$multiHist.plot2 <- renderPlot({ createHist(userToken = input$userToken, data = toydata$cont2, color = "#007ba7", xlabel = "Scores in comparison group") })
  output$multiHist.plot3 <- renderPlot({ createHist(userToken = input$userToken, data = toydata$cont3, color = "#007ba7", xlabel = "Scores in comparison group") })
 
  # Simple Bar
  output$simpleBar.intro <- renderUI({ HTML("Introduction - Simple Bar") })
  
  output$simpleBar.desc  <- renderUI({ HTML("Description - Simple Bar") })
  
  output$simpleBar.plot  <- renderPlot({ createBar(userToken = input$userToken, data = toydata[,c("cont1","cont1_mean", "cont1_comp")], uniform = TRUE, color = "#355C7D", xlabel = "Simple Bar - x label") })
  
  # Multi Bar
  output$multiBar.intro <- renderUI({ HTML("Introduction - Multi Bar") })
  
  output$multiBar.desc1 <- renderUI({ HTML("Description - Multi Bar for Dimension 1") })
  output$multiBar.desc2 <- renderUI({ HTML("Description - Multi Bar for Dimension 2") })
  output$multiBar.desc3 <- renderUI({ HTML("Description - Multi Bar for Dimension 3") })
  
  output$multiBar.plot1 <- renderPlot({ createBar(userToken = input$userToken, data = toydata[,c("cont1","cont1_mean", "cont1_comp")], uniform = FALSE, color = c("#355C7D","#6C5B7B","#C06C84"), xlabel = "Scores in comparison group") })
  output$multiBar.plot2 <- renderPlot({ createBar(userToken = input$userToken, data = toydata[,c("cont2","cont2_mean", "cont2_comp")], uniform = FALSE, xlabel = "Scores in comparison group") })
  output$multiBar.plot3 <- renderPlot({ createBar(userToken = input$userToken, data = toydata[,c("cont3","cont3_mean", "cont3_comp")], uniform = TRUE, xlabel = "Scores in comparison group") })

  # Simple Radar
  output$simpleRadar.intro <- renderUI({ HTML("Introduction - Simple Radar") })
  
  output$simpleRadar.desc  <- renderUI({ HTML("Description - Simple Radar") })
  
  output$simpleRadar.plot  <- renderPlot({ createRadar(userToken = input$userToken, data = toydata[,c("cont1","cont2","cont3", "cont4")]) })

  # Simple Radar W/ Comparison
  output$simpleRadarComp.intro <- renderUI({ HTML("Introduction - Simple Radar with Comparison") }) 
  
  output$simpleRadarComp.desc  <- renderUI({ HTML("Description - Simple Radar with Comparison") })
  
  output$simpleRadarComp.plot  <- renderPlot({ createRadarComp(userToken = input$userToken, data = toydata[,c("cont1","cont2","cont3", "cont4")], dataComp = toydata[,c("cont1_mean","cont2_mean","cont3_mean", "cont4_mean")], color = "#007ba7") })

  # Fancy Radar W/ Comparison
  output$fancyRadarComp.intro <- renderUI({ HTML("Introduction - Fancy Radar with Comparison")})
  
  #output$fancyRadarComp.desc1 <- renderUI({ input$fancyRadarCompTB})
  output$fancyRadarComp.desc1 <- renderUI({ HTML("Description - Fancy Radar with Comparison for Dimension 1") })
  output$fancyRadarComp.desc2 <- renderUI({ HTML("Description - Fancy Radar with Comparison for Dimension 2") })
  output$fancyRadarComp.desc3 <- renderUI({ HTML("Description - Fancy Radar with Comparison for Dimension 3") })
  output$fancyRadarComp.desc4 <- renderUI({ HTML("Description - Fancy Radar with Comparison for Dimension 4") })
  
  output$fancyRadarComp.plot1 <- renderPlot({ createRadarComp(userToken = input$userToken, data = toydata[,c("cont1","cont2","cont3", "cont4")], dataComp = toydata[,c("cont1_mean","cont2_mean","cont3_mean", "cont4_mean")], color = "#007ba7", dim = "cont1") })
  output$fancyRadarComp.plot2 <- renderPlot({ createRadarComp(userToken = input$userToken, data = toydata[,c("cont1","cont2","cont3", "cont4")], dataComp = toydata[,c("cont1_mean","cont2_mean","cont3_mean", "cont4_mean")], color = "#007ba7", dim = "cont2") })
  output$fancyRadarComp.plot3 <- renderPlot({ createRadarComp(userToken = input$userToken, data = toydata[,c("cont1","cont2","cont3", "cont4")], dataComp = toydata[,c("cont1_mean","cont2_mean","cont3_mean", "cont4_mean")], color = "#007ba7", dim = "cont3") })
  output$fancyRadarComp.plot4 <- renderPlot({ createRadarComp(userToken = input$userToken, data = toydata[,c("cont1","cont2","cont3", "cont4")], dataComp = toydata[,c("cont1_mean","cont2_mean","cont3_mean", "cont4_mean")], color = "#007ba7", dim = "cont4") })

  # Simple Line
  output$simpleLine.intro <- renderUI({ HTML("Introduction - Simple Line") })
  
  output$simpleLine.desc  <- renderUI({ HTML("Description - Simple Line") })
  
  output$simpleLine.plot  <- renderPlot({ createLine(userToken = input$userToken, data = toydata[,c("ts_var1_V1_1","ts_var1_V1_2","ts_var1_V1_3", "ts_var1_V1_4", "ts_var1_V1_5",
                                                                                                    "ts_var1_V2_1","ts_var1_V2_2","ts_var1_V2_3", "ts_var1_V2_4", "ts_var1_V2_5",
                                                                                                    "ts_var2_V1_1","ts_var2_V1_2","ts_var2_V1_3", "ts_var2_V1_4", "ts_var2_V1_5",
                                                                                                    "ts_var2_V2_1","ts_var2_V2_2","ts_var2_V2_3", "ts_var2_V2_4", "ts_var2_V2_5")], xlength = 5) })
  # Fancy Line
  output$fancyLine.intro <- renderUI({ HTML("Introduction - Fancy Line")})
  
  output$fancyLine.desc1  <- renderUI({ HTML("Description - Fancy Line for Dimension 1") })
  output$fancyLine.desc2  <- renderUI({ HTML("Description - Fancy Line for Dimension 2") })
  output$fancyLine.desc3  <- renderUI({ HTML("Description - Fancy Line for Dimension 3") })
  output$fancyLine.desc4  <- renderUI({ HTML("Description - Fancy Line for Dimension 4") })
  
  output$fancyLine.plot1  <- renderPlot({ createLine(userToken = input$userToken, data = toydata[,c("ts_var1_V1_1","ts_var1_V1_2","ts_var1_V1_3", "ts_var1_V1_4", "ts_var1_V1_5",
                                                                                                    "ts_var1_V2_1","ts_var1_V2_2","ts_var1_V2_3", "ts_var1_V2_4", "ts_var1_V2_5",
                                                                                                    "ts_var2_V1_1","ts_var2_V1_2","ts_var2_V1_3", "ts_var2_V1_4", "ts_var2_V1_5",
                                                                                                    "ts_var2_V2_1","ts_var2_V2_2","ts_var2_V2_3", "ts_var2_V2_4", "ts_var2_V2_5")], xlength = 5, dim = "line1") })
  output$fancyLine.plot2  <- renderPlot({ createLine(userToken = input$userToken, data = toydata[,c("ts_var1_V1_1","ts_var1_V1_2","ts_var1_V1_3", "ts_var1_V1_4", "ts_var1_V1_5",
                                                                                                    "ts_var1_V2_1","ts_var1_V2_2","ts_var1_V2_3", "ts_var1_V2_4", "ts_var1_V2_5",
                                                                                                    "ts_var2_V1_1","ts_var2_V1_2","ts_var2_V1_3", "ts_var2_V1_4", "ts_var2_V1_5",
                                                                                                    "ts_var2_V2_1","ts_var2_V2_2","ts_var2_V2_3", "ts_var2_V2_4", "ts_var2_V2_5")], xlength = 5, dim = "line2") })
  output$fancyLine.plot3  <- renderPlot({ createLine(userToken = input$userToken, data = toydata[,c("ts_var1_V1_1","ts_var1_V1_2","ts_var1_V1_3", "ts_var1_V1_4", "ts_var1_V1_5",
                                                                                                    "ts_var1_V2_1","ts_var1_V2_2","ts_var1_V2_3", "ts_var1_V2_4", "ts_var1_V2_5",
                                                                                                    "ts_var2_V1_1","ts_var2_V1_2","ts_var2_V1_3", "ts_var2_V1_4", "ts_var2_V1_5",
                                                                                                    "ts_var2_V2_1","ts_var2_V2_2","ts_var2_V2_3", "ts_var2_V2_4", "ts_var2_V2_5")], xlength = 5, dim = "line3") })
  output$fancyLine.plot4  <- renderPlot({ createLine(userToken = input$userToken, data = toydata[,c("ts_var1_V1_1","ts_var1_V1_2","ts_var1_V1_3", "ts_var1_V1_4", "ts_var1_V1_5",
                                                                                                    "ts_var1_V2_1","ts_var1_V2_2","ts_var1_V2_3", "ts_var1_V2_4", "ts_var1_V2_5",
                                                                                                    "ts_var2_V1_1","ts_var2_V1_2","ts_var2_V1_3", "ts_var2_V1_4", "ts_var2_V1_5",
                                                                                                    "ts_var2_V2_1","ts_var2_V2_2","ts_var2_V2_3", "ts_var2_V2_4", "ts_var2_V2_5")], xlength = 5, dim = "line4") })
  
  
}

