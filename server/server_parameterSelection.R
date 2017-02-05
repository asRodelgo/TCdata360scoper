# update country selector with region selector
observe({
  
  if (!((input$colRegion=="All") || (is.null(input$colRegion)))){
    region <- input$colRegion
    updateSelectizeInput(session, "colCountry",
                         choices=sort(unique(filter(data_tsne_sample, Region %in% region)$Country)), 
                         selected=NULL)
  }
  
})  