# --------- Filter and processing functions for plots

# Plot tsne chart ---------------------------------------------------------
.tSNE_plot_filter <- function(colRegion,colPeriod,colCountry,selected_indicators){
  #
  if (colCountry=="All" || is.null(colCountry)) colCountry <- countries_list
  if (colRegion=="All" || is.null(colRegion)) colRegion <- regions_list
  if (colPeriod=="All" || is.null(colPeriod)) colPeriod <- periods_list
  
  # transform indicators text into codes (for now, add/remove X)
  #selected_indicators <- paste0("X",selected_indicators)
  # 
  if (length(tsne_ready)>0){ # if data do stuff
    if (!is.null(selected_indicators)){  # if at least 1 selected indicator
      # map indicator labels to codes
      #selected_indicators <- paste0("X",filter(indicators_1_2, name %in% selected_indicators)$id)
      selected_indicators <- paste0("X",unique(filter(data_attributes, Series_Name %in% selected_indicators)$Series_Code))
      #
      tsne_ready_select <- tsne_ready %>%
        dplyr::select(main_object, Period, Region, 
                      IncomeLevel, Country, x, y, 
                      one_of(selected_indicators))
    } else { # no selected indicators    
      tsne_ready_select <- tsne_ready %>%
        dplyr::select(main_object, Period, Region, 
                      IncomeLevel, Country, x, y)
    }  
    # General Filters
    tsne_points_filter <- tsne_ready_select %>%
      filter(Country %in% colCountry & Region %in% colRegion & 
               Period %in% colPeriod) %>%
      group_by(Country,Period) %>%
      mutate(group = ifelse(length(colRegion)>2,
                            ifelse(length(colPeriod) == 2,
                                   ifelse(length(colCountry)>2,Period,paste0(Country," (",Period,")")),
                                   ifelse(length(colCountry)>2,Region,
                                          ifelse(length(colPeriod)==1,paste0(Country," (",Period,")"),Country))),
                            ifelse(length(colPeriod)>2,ifelse(length(colCountry)>2,Region,Country),
                                   ifelse(length(colCountry)>2,paste0(Region," (",Period,")"),paste0(Country," (",Period,")")))))
    tsne_points_filter <- as.data.frame(tsne_points_filter)
    tsne_points_filter_out <- tsne_ready_select %>%
      filter(!(Country %in% colCountry & Region %in% colRegion & Period %in% colPeriod))
    
    
  } else{ return()}
  #plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
  #graphics::text(1.5, 1,"Not enough data", col="red", cex=2)
  
  return(tsne_points_filter)
}

# Filters for hover over tooltips ---------------------------------------------------------
.tSNE_plot_filter_hover <- function(colRegion,colPeriod,colCountry,selected_indicators){
  #
  if (colCountry=="All" || is.null(colCountry)) colCountry <- as.character(countries_list)
  if (colRegion=="All" || is.null(colRegion)) colRegion <- regions_list
  if (colPeriod=="All" || is.null(colPeriod)) colPeriod <- periods_list

  if (length(tsne_ready)>0){ # if data do stuff
    #datascope_filter <- .filter_datascope()
    tsne_points_filter <- inner_join(tsne_ready[,c("main_object","Period","Region", "Country","x","y","missing_values")],mutate(data_filter, Period = as.character(Period)), by=c("main_object","Period")) %>%
      filter(Country %in% colCountry & Region %in% colRegion & Period %in% colPeriod) %>%
      dplyr::select(indicatorID,Period,Observation,Country,x,y) %>%
      distinct(Country,Period,indicatorID, .keep_all=TRUE)
    
    
    tsne_points_filter$indicatorID <- paste0("X",tsne_points_filter$indicatorID)
    #these_indicators <- paste0("X",filter(indicators_1_2, name %in% selected_indicators)$id)
    these_indicators <- paste0("X",unique(filter(data_attributes, Series_Name %in% selected_indicators)$Series_Code))
    
    tsne_points_filter <- tsne_points_filter %>%
      #group_by(Country,Period) %>%
      spread(indicatorID,Observation) %>%
      select(Country,Period,one_of(these_indicators),x,y) %>%
      mutate_at(vars(num_range("X",1:5000)), funs(round(.,2))) %>%
      #select(-Indicator) %>%
      distinct(Country,Period,.keep_all=TRUE) %>%
      as.data.frame()
  #write.csv(tsne_points_filter, "data/hover_data.csv", row.names=FALSE)
  } else{ return()}

  return(tsne_points_filter)
}

# Filters for brush over tooltips ---------------------------------------------------------
.tSNE_plot_filter_brush <- function(colRegion,colPeriod,colCountry){
  #
  if (colCountry=="All" || is.null(colCountry)) colCountry <- countries_list
  if (colRegion=="All" || is.null(colRegion)) colRegion <- regions_list
  if (colPeriod=="All" || is.null(colPeriod)) colPeriod <- periods_list
  
  if (length(tsne_ready)>0){ # if data do stuff
    #datascope_filter <- .filter_datascope()
    tsne_points_filter <- inner_join(tsne_ready[,c("main_object","Period","x","y","missing_values")],mutate(data_filter, Period = as.character(Period)), by=c("main_object","Period")) %>%
      filter(Country %in% colCountry & Region %in% colRegion & Period %in% colPeriod) %>%
      dplyr::select(indicatorID,Period,Observation,Country,x,y) %>%
      distinct(Country,Period,indicatorID, .keep_all=TRUE)
    
     tsne_points_filter$indicatorID <- paste0("X",tsne_points_filter$indicatorID)
    
    #write.csv(tsne_points_filter, "data/hover_data.csv", row.names=FALSE)
  } else{ return()}
  
  return(tsne_points_filter)
}

# filter datascope original data
.filter_datascope <- function(data, isCountry = TRUE){
  
  data_filter <- data %>%
    gather(Period,Observation,-main_object,-indicatorID) %>%
    #inner_join(indicators_1_2, by="id") %>%
    dplyr::select(main_object,indicatorID,Period,Observation) %>%
    distinct(main_object, Period, indicatorID, .keep_all=TRUE) %>%
    mutate(Period = as.character(gsub("X","",Period))) 
  
  if (isCountry){
    data_filter <- data_filter %>%
      inner_join(select(countries,iso3,Country=name,Region=region,IncomeLevel=incomeLevel),
                 by=c("main_object"="iso3"))
  }
  
 
  return(data_filter) 
  
}  


