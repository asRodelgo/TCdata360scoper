###### update country selector with region selector --------------
observe({
  
  if (!((input$colRegion=="All") || (is.null(input$colRegion)))){
    region <- input$colRegion
    updateSelectizeInput(session, "colCountry",
                         choices=sort(unique(filter(data_tsne_sample, Region %in% region)$Country)), 
                         selected=NULL)
  }
  
})  

######## Show PM picture ----------------------------------------

output$staff_photo <- renderUI({
  
  photo_link <- get_operational_profile_photo(input$colCountry)[1,1] 
  
  #tags$img(src = photo_link, width = '100%', height = '100%',position = 'absolute')
  tags$img(src = photo_link, display= 'block',margin= '0 auto', height= 'auto',width= '100%', verticalAlign='middle')
  #tags$img(src = photo_link,position = 'absolute',top = '-9999px',left= '-9999px',right= '-9999px',bottom= '-9999px',margin= 'auto')
  #return(dwwcirc(readJPEG(tags$img(src = photo_link))))
  #return(dwwcirc(photo_link))
})