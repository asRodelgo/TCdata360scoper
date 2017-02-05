# ------ Tables 

.brushTable <- function(brushPoints,selected_indicators){
  
  if (!is.null(selected_indicators)){  
    # map indicator labels to codes
    selected_indicators <- paste0("X",filter(indicators_1_2, name %in% selected_indicators)$id)
    #
    # brushed points
    #brushPoints <- filter(tsne_points_filter, Country %in% c("Bolivia","Colombia"))
    brushPoints <- dplyr::select(brushPoints,Country, iso3, Period, one_of(selected_indicators))
    #names(brushPoints) <- c("Country","Period",indicator_selection_plots_short)
    # actual data filter
    selected_datascope_data <- .filter_datascope_data()
    # merge
    brushPoints_actual <- merge(selected_datascope_data,brushPoints[,c("Country","Period")], 
                                by.x = c("Country","Period"), by.y = c("Country","Period"))
    brushPoints_actual <- brushPoints_actual %>%
      dplyr::select(Country, iso3,Period, one_of(selected_indicators)) %>%
      mutate(Country = paste0('<a href=',country_url,iso3,' target="_blank" >',Country,'</a>')) %>%
      dplyr::select(-iso3)
    
    require(stringr) # to wrap label text
    names(brushPoints_actual) <- gsub("_"," ",names(brushPoints_actual))
    names(brushPoints_actual) <- str_wrap(names(brushPoints_actual), width = 25)  
    
    #names(brushPoints_actual) <- c("Country","Period",indicator_selection_plots_short)
    brushPoints_actual[,c(3:ncol(brushPoints_actual))] <- round(brushPoints_actual[,c(3:ncol(brushPoints_actual))],2)
    brushPoints_actual[is.na(brushPoints_actual)] <- "..."
    
    ## Testing hrefs for indicators
    #     count_rows <- nrow(brushPoints_actual)
    #     thisRow <- 1
    #     while (thisRow <= count_rows){
    #       brushPoints_actual[thisRow,3] <- paste0('<a href=',indicator_url,'corr.scr',' target="_blank" >',brushPoints_actual[thisRow,3],'</a>')
    #       thisRow <- thisRow + 1
    #     }
    ## end of testing
    
    #return(str(brushPoints))
    brushPoints <- brushPoints_actual
  } else {
    brushPoints <- dplyr::select(brushPoints,Country, Period)
  }  
  return(brushPoints)  
}

.compare10_click <- function(colPeriod,colCountry){
  
  if (colCountry=="All" || is.null(colCountry)) colCountry <- countries_list
  if (colPeriod=="All" || is.null(colPeriod)) colPeriod <- periods_list
  
  if (length(tsne_ready)>0){ # if data do stuff
    
    # coordenates x,y of clicked point
    distCouPerX <- filter(tsne_ready,Country %in% colCountry, Period %in% colPeriod)$x
    distCouPerY <- filter(tsne_ready,Country %in% colCountry, Period %in% colPeriod)$y
    
    tsne_points_filter <- tsne_ready %>%
      select(Period,Country,x,y) %>%
      mutate(dist = sqrt((x-distCouPerX)^2+(y-distCouPerY)^2)) %>%
      arrange(dist) %>%
      select(-x,-y)
    
  } else{ return()}
  
  return(tsne_points_filter)
}

.summary_brush <- function(brushPoints,selected_indicators){
  
  #brushPoints <- filter(tsne_ready, Country == "Spain")
  if (!is.null(selected_indicators)){  
    # map indicator labels to codes
    selected_indicators <- paste0("X",filter(indicators_1_2, name %in% selected_indicators)$id)
    # brushed points
    brushPoints <- select(brushPoints,Country, Period, one_of(selected_indicators)) %>%
      summarize_if(is.numeric,funs(mean(.,na.rm=TRUE)))
    
    
  } else {
    brushPoints <- dplyr::select(brushPoints,Country, Period)
  }  
  return(brushPoints)  
  
}