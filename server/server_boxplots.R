# boxplots for selected indicators
output$plotBoxplotBrushed <- renderPlot({
  
  brush <- input$plot_brush
  pointsBrushed <- brushedPoints(.tSNE_plot_filter(input$colRegion,input$colPeriod,input$colCountry,
                                                   input$explore_variables), brush)
  #     click <- input$plot_click
  #     point <- nearPoints(.tSNE_plot_filter(input$colRegion,input$colPeriod,input$colCountry,
  #                                          input$explore_variables), click, threshold = 3, 
  #                        maxpoints = 1, addDist = TRUE)
  
  boxplotBrushed <- .boxPlots(pointsBrushed,input$colRegion,input$colPeriod,input$colCountry,
                              input$explore_variables)#,point$Country,point$Period)
  
  return(boxplotBrushed)
  
})