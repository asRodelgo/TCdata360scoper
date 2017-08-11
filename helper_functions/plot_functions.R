# -------------- Plot side charts

# Density plots
.densityPlots <- function(colRegion,colPeriod,colCountry,colIndicator,clickCountry,clickPeriod,selected_indicators){
  
  tsne_points_filter <- .tSNE_plot_filter(colRegion,colPeriod,colCountry,colIndicator)
  tsne_points_filter <- gather(tsne_points_filter, indicator, value, -iso3,-Country,-IncomeLevel,-Region,-Period,-x,-y)
  tsne_ready_gather <- gather(tsne_ready, indicator, value, -iso3,-Country,-IncomeLevel,-Region,-Period,-x,-y)
  
  tsne_points_filter <- filter(tsne_points_filter, indicator %in% selected_indicators)
  tsne_ready_gather <- filter(tsne_ready_gather, indicator %in% selected_indicators)
  
  if (is.null(clickCountry)){
    
    ggplot(data=tsne_ready_gather,aes(value)) + 
      geom_density(data=tsne_ready_gather,aes(y=..density..),alpha=.8, fill="grey") +  
      geom_histogram(data=tsne_points_filter,aes(y=..density..),alpha=.6, fill="lightblue") +  
      facet_wrap(~indicator, nrow=1, scales="free_x") +
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(lineheight=.5),
            #axis.text.x = element_blank(),
            #axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()
            #axis.ticks = element_blank()
      )
    
  } else {
    
    verticalLine <- tsne_points_filter %>%
    filter(Country == clickCountry, Period == clickPeriod) %>%
      dplyr::select(indicator, value)
    
    ggplot(data=tsne_ready_gather,aes(value)) + 
      geom_density(data=tsne_ready_gather,aes(y=..density..),alpha=.8, fill="grey") +  
      geom_histogram(data=tsne_points_filter,aes(y=..density..),alpha=.6, fill="lightblue") +  
      facet_wrap(~indicator, nrow=1, scales="free_x") +
      geom_vline(data=verticalLine, aes(xintercept = value), colour="red", size = 1) +
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(lineheight=.5),
            #axis.text.x = element_blank(),
            #axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()
            #axis.ticks = element_blank()
      )
    
  }
  
}

.radarPlot <- function(brushPoints,selected_indicators){
  
  tsne_radar <- tsne_ready %>%
    dplyr::select(one_of(selected_indicators), Country, Period) %>%
    mutate_at(selected_indicators, funs(max,mean)) %>%
    #filter(Season == colPeriod) %>%
    dplyr::select(-Period)
  
  #brushPoints <- filter(tsne_ready, Tm == "CHI")
  brushPoints <- as.data.frame(brushPoints)
  
  if (nrow(brushPoints)>0){
    #brushPoints <- merge(tsne_ready, brushPoints, by = c("Player","Season"))
    tsne_mean <- brushPoints %>%
      dplyr::select(one_of(selected_indicators), Country, Period) %>%
      mutate_at(selected_indicators, funs(mean)) %>%
      #dplyr::select(ends_with("_mean")) %>%
      mutate(Country = "mean of selected") %>%
      distinct(.keep_all=TRUE) %>%
      dplyr::select(Country, everything())
    
    names(tsne_mean) <- gsub("_mean","",names(tsne_mean))
    
  } else {
    tsne_mean <- tsne_radar %>%
      dplyr::select(ends_with("_mean")) %>%
      distinct(.keep_all=TRUE) %>%
      mutate(Country = "mean of selected") %>%
      dplyr::select(Country, everything())
    
    names(tsne_mean) <- gsub("_mean","",names(tsne_mean))
  }
  
  tsne_max <- tsne_radar %>%
    dplyr::select(ends_with("_max")) %>%
    distinct(.keep_all=TRUE) %>%
    mutate(Country = "max") %>%
    dplyr::select(Country, everything())
  
  names(tsne_max) <- gsub("_max","",names(tsne_max))
  
  tsne_radar <- bind_rows(tsne_mean,tsne_max)
  tsne_radar <- dplyr::select(tsne_radar, Country, one_of(selected_indicators))
  # shorter names to display
  names(tsne_radar) <- c("Country",indicator_selection_plots_short)
  #ez.radarmap(df, "model", stats="mean", lwd=1, angle=0, fontsize=0.6, facet=T, facetfontsize=1, color=id, linetype=NULL)
  ez.radarmap(tsne_radar, "Country", stats="none", lwd=1, angle=0, fontsize=1.5, facet=F, facetfontsize=1, color=id, linetype=NULL) +
    theme(legend.key=element_blank(),
          legend.title=element_blank(),
          legend.position = "bottom",
          #panel.border = element_blank(),
          #panel.background = element_blank(),
          plot.title = element_text(lineheight=.5),
          #axis.text.x = element_blank(),
          #axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
          #axis.ticks = element_blank()
    )  
  
}

.radarPlot_base <- function(brushPoints,selected_indicators){
  
  tsne_ready_select <- as.data.frame(.tSNE_plot_filter(colRegion,colPeriod,colCountry,selected_indicators))
  
  if (length(selected_indicators)>1){
    tsne_radar <- tsne_ready_select %>%
      group_by(group) %>%
      dplyr::select(one_of(selected_indicators), Country, Period,group) %>%
      mutate_at(selected_indicators, funs(max,mean)) %>%
      #filter(Season == colPeriod) %>%
      dplyr::select(-Period)
  } else { # introduce fictitious variable x to keep the _mean, _max structure when 
    # only 1 indicator is selected
    tsne_radar <- tsne_ready_select %>%
      dplyr::select(one_of(selected_indicators),x, Country, Period,group) %>%
      mutate_at(c(selected_indicators,"x"), funs(mean)) #%>%
    #filter(Season == colPeriod) %>%
    dplyr::select(-Period)
  }
  #brushPoints <- filter(tsne_ready, iso3 == "ALB")
  brushPoints <- as.data.frame(brushPoints)
  
  if (nrow(brushPoints)>0){
    #brushPoints <- merge(tsne_ready, brushPoints, by = c("Player","Season"))
    tsne_mean <- brushPoints %>%
      dplyr::select(one_of(selected_indicators), Country, Period,Region) %>%
      mutate_at(selected_indicators, funs(mean)) %>%
      #dplyr::select(ends_with("_mean")) %>%
      mutate(group = "mean of selected") %>%
      distinct(group, .keep_all=TRUE) %>%
      dplyr::select(group, everything())
    
    names(tsne_mean) <- gsub("_mean","",names(tsne_mean))
    
  } else {
    
    tsne_mean <- tsne_radar %>%
      group_by(group) %>%
      dplyr::select(ends_with("_mean"),group) %>%
      distinct(.keep_all=TRUE) %>%
      #mutate(Country = "mean of selected") %>%
      dplyr::select(group, everything())
    
    names(tsne_mean) <- gsub("_mean","",names(tsne_mean))
    
  }
  
  tsne_max <- tsne_ready %>%
    #group_by(group) %>%
    dplyr::select(one_of(selected_indicators)) %>%
    mutate_at(selected_indicators, funs(max)) %>%
    #dplyr::select(-Period) %>%
    #dplyr::select(ends_with("_max")) %>%
    mutate(group = "max") %>%
    distinct(.keep_all=TRUE) %>%
    dplyr::select(group,everything())
  
  #names(tsne_max) <- gsub("_max","",names(tsne_max))
  
  tsne_radar <- bind_rows(tsne_mean,tsne_max)
  tsne_radar <- dplyr::select(tsne_radar, group, one_of(selected_indicators))
  
  require(stringr) # to wrap label text
  names(tsne_radar) <- gsub("_"," ",names(tsne_radar))
  names(tsne_radar) <- str_wrap(names(tsne_radar), width = 20)  
  
  # add the min column and transpose
  tsne_radar <- t(tsne_radar)
  tsne_radar_labels <- row.names(tsne_radar)[-1]
  tsne_radar_groups <- tsne_radar[1,]
  tsne_radar <- as.data.frame(tsne_radar)
  tsne_radar <- mutate_all(tsne_radar, funs(as.numeric(as.character(.))))
  tsne_radar <- tsne_radar %>%
    mutate(Observation = V1*10, min = 1, max = 10) %>%
    dplyr::select(max, min, Observation)
  tsne_radar <- as.data.frame(t(tsne_radar))
  # plot
  radarchart(tsne_radar, axistype=1, caxislabels=c(" "," ",".5"," "," "), centerzero = FALSE,seg=4,
             plty=c(1),plwd=c(5),pcol=c("green"),pdensity=c(0),
             cglwd=2,axislabcol="red", vlabels=tsne_radar_labels, cex.main=1,cex=2.5)  
  
}

# Bar chart plot
.bar_chart <- function(brushPoints,colRegion,colPeriod,colCountry,selected_indicators){      
  
  if (!(is.null(selected_indicators))){
    
    tsne_ready_select <- as.data.frame(.tSNE_plot_filter(colRegion,colPeriod,colCountry,selected_indicators))
    
    if (length(selected_indicators)>1){
      tsne_radar <- tsne_ready_select %>%
        group_by(group) %>%
        dplyr::select(one_of(selected_indicators), Country, Period,group) %>%
        mutate_at(selected_indicators, funs(max,mean)) %>%
        #filter(Season == colPeriod) %>%
        dplyr::select(-Period)
    } else { # introduce fictitious variable x to keep the _mean, _max structure when 
      # only 1 indicator is selected
      tsne_radar <- tsne_ready_select %>%
        dplyr::select(one_of(selected_indicators),x, Country, Period,group) %>%
        mutate_at(c(selected_indicators,"x"), funs(mean)) #%>%
      #filter(Season == colPeriod) %>%
      dplyr::select(-Period)
    }
    #brushPoints <- filter(tsne_ready, iso3 == "ALB")
    brushPoints <- as.data.frame(brushPoints)
    
    if (nrow(brushPoints)>0){
      #brushPoints <- merge(tsne_ready, brushPoints, by = c("Player","Season"))
      tsne_mean <- brushPoints %>%
        dplyr::select(one_of(selected_indicators), Country, Period,Region) %>%
        mutate_at(selected_indicators, funs(mean)) %>%
        #dplyr::select(ends_with("_mean")) %>%
        mutate(group = "mean of selected") %>%
        distinct(group, .keep_all=TRUE) %>%
        dplyr::select(group, everything())
      
      names(tsne_mean) <- gsub("_mean","",names(tsne_mean))
      
    } else {
      
      tsne_mean <- tsne_radar %>%
        group_by(group) %>%
        dplyr::select(ends_with("_mean"),group) %>%
        distinct(.keep_all=TRUE) %>%
        #mutate(Country = "mean of selected") %>%
        dplyr::select(group, everything())
      
      names(tsne_mean) <- gsub("_mean","",names(tsne_mean))
      
    }
    
    tsne_max <- tsne_ready %>%
      #group_by(group) %>%
      dplyr::select(one_of(selected_indicators)) %>%
      mutate_at(selected_indicators, funs(max)) %>%
      #dplyr::select(-Period) %>%
      #dplyr::select(ends_with("_max")) %>%
      mutate(group = "max") %>%
      distinct(.keep_all=TRUE) %>%
      dplyr::select(group,everything())
    
    #names(tsne_max) <- gsub("_max","",names(tsne_max))
    
    tsne_radar <- bind_rows(tsne_mean,tsne_max)
    tsne_radar <- dplyr::select(tsne_radar, group, one_of(selected_indicators))
    
    require(stringr) # to wrap label text
    names(tsne_radar) <- gsub("_"," ",names(tsne_radar))
    names(tsne_radar) <- str_wrap(names(tsne_radar), width = 20)  
    
    tsne_barchart <- gather(tsne_radar,Indicator,Observation,-group)
    
    data_color <- filter(tsne_barchart,!(group=="max"))
    data_color$group <- str_wrap(data_color$group,width=10)
    data_grey <- filter(tsne_barchart,group=="max")
    
    #data <- mutate(data, id = seq(1,nrow(data),1))
    ggplot(NULL,aes(x=Indicator,y=Observation)) +
      geom_bar(data=data_grey,color="#f1f3f3",fill = "#f1f3f3",stat="identity") +
      geom_bar(data=data_color,aes(color=group,fill=group),stat="identity",position = "dodge") +
      geom_text(data=data_color, aes(label=round(Observation,2),y=Observation + .05,group=group),
                size=4,color="darkblue",position = position_dodge(width = .9)) + 
      coord_flip()+
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            legend.position="top",
            legend.key.size = unit(0.5, "cm"),
            panel.border = element_blank(),
            panel.background = element_blank(),plot.title = element_text(lineheight=.5),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12)) + 
      labs(x="",y=""#,title="Top 5 constraints according to 2013 Enterprise Survey (in percent)"
      )
  } else{
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"No indicators selected", col="red", cex=1)
  }
}

# Box plots
.boxPlots <- function(brushPoints,colRegion,colPeriod,colCountry,selected_indicators,clickCountry=NULL,clickPeriod=NULL){      
  
  # map indicator labels to codes
  #selected_indicators <- paste0("X",filter(indicators_1_2, name %in% selected_indicators)$id)
  #
  #set.seed(123) # to fix the jitter across different plots
  # cloud of points on top of boxplots
  tsne_points_filter <- as.data.frame(.tSNE_plot_filter(colRegion,colPeriod,colCountry,selected_indicators))
  tsne_points_filter <- gather(tsne_points_filter, indicator, value, -main_object,-Country,
                               -IncomeLevel,-Region,-Period,-x,-y,-group) %>%
    distinct(Country, Period, indicator, .keep_all=TRUE)
  tsne_points_filter$indicator <- gsub("X","",tsne_points_filter$indicator)
  tsne_points_filter <- merge(tsne_points_filter,distinct(data_attributes[,c("Series_Code","Series_Name")]), by.x="indicator",by.y="Series_Code",all.x = TRUE) 
  tsne_points_filter <- tsne_points_filter %>%
    select(-indicator) %>%
    select(indicator = Series_Name, everything())
  tsne_points_filter$indicator <- gsub("_"," ",tsne_points_filter$indicator)
  tsne_points_filter$indicator <- str_wrap(tsne_points_filter$indicator, width = 20)  
  tsne_points_filter$group <- str_wrap(tsne_points_filter$group,width=12)
  
  # boxplots
  selected_indicators_codes <- paste0("X",unique(filter(data_attributes, Series_Name %in% selected_indicators)$Series_Code))
  tsne_ready_gather <- gather(tsne_ready, indicator, value, -main_object,-Country,
                              -IncomeLevel,-Region,-Period,-x,-y) %>%
    filter(indicator %in% selected_indicators_codes) %>%
    mutate(value = as.numeric(value)) %>%
    distinct(Country, Period, indicator, .keep_all=TRUE)
  
  tsne_ready_gather$indicator <- gsub("X","",tsne_ready_gather$indicator)
  tsne_ready_gather <- merge(tsne_ready_gather,distinct(data_attributes[,c("Series_Code","Series_Name")]), by.x="indicator",by.y="Series_Code",all.x = TRUE) 
  tsne_ready_gather <- tsne_ready_gather %>%
    select(-indicator) %>%
    select(indicator = Series_Name, everything())
  tsne_ready_gather$indicator <- gsub("_"," ",tsne_ready_gather$indicator)
  tsne_ready_gather$indicator <- str_wrap(tsne_ready_gather$indicator, width = 20)
  extremes_high <- tsne_ready_gather %>%
    group_by(indicator) %>%
    filter(value==max(value)) %>%
    distinct(value,.keep_all=TRUE)
  extremes_low <- tsne_ready_gather %>%
    group_by(indicator) %>%
    filter(value==min(value)) %>%
    distinct(value,.keep_all=TRUE)
  #tsne_ready_gather$group <- str_wrap(tsne_ready_gather$group,width=10)
  
  #plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
  ##graphics::text(1.5, 1,nrow(brushPoints), col="red", cex=1)
  #graphics::text(1.5, 0.8,paste(clickCountry,"Ubabu"), col="red", cex=1)
  
  if (paste0(clickPeriod," ")==" "){ #no click
    
    brushPoints <- as.data.frame(brushPoints)
    
    if (nrow(brushPoints)>0){ #brush
      
      brushPoints <- dplyr::select(brushPoints,group,one_of(selected_indicators_codes))
      brushPoints <- gather(brushPoints, indicator, value, -group)
      brushPoints$indicator <- gsub("X","",brushPoints$indicator)
      brushPoints <- merge(brushPoints,distinct(data_attributes[,c("Series_Code","Series_Name")]), by.x="indicator",by.y="Series_Code",all.x = TRUE) 
      brushPoints <- brushPoints %>%
        select(-indicator) %>%
        select(indicator = Series_Name, everything())
      brushPoints$indicator <- gsub("_"," ",brushPoints$indicator)
      brushPoints$indicator <- str_wrap(brushPoints$indicator, width = 20)
      brushPoints$group <- str_wrap(brushPoints$group,width=12)
      
      ggplot(data=tsne_ready_gather,aes(indicator,value)) + 
        geom_boxplot(color="darkgrey") +  
        geom_jitter(data=tsne_points_filter,aes(group=group,color=group),alpha=0.01,width=0.3) +  
        geom_jitter(data=brushPoints,aes(group=group,color=group),width=0.3) +  
        geom_text(data=extremes_high,aes(label=str_wrap(paste0(Country," (",Period,")"),width=12)),color="darkgrey",size=3,nudge_x = 0.35,nudge_y=-0.05,show.legend = FALSE) +
        geom_text(data=extremes_low,aes(label=str_wrap(paste0(Country," (",Period,")"),width=12)),color="darkgrey",size=3,nudge_x = 0.35,nudge_y=0.05,show.legend = FALSE) +           
        coord_flip() +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.text = element_text(size = 12),
              legend.position="top",
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.title = element_text(lineheight=.5),
              #axis.text.x = element_blank(),
              axis.text.y = element_text(size=13),
              axis.title.x = element_blank(),
              axis.title.y = element_blank()
              #axis.ticks = element_blank()
        )
    } else{ # no brush, no click
      
      ggplot(data=tsne_ready_gather,aes(indicator,value)) + 
        geom_boxplot(color="darkgrey") +  
        geom_jitter(data=tsne_points_filter,aes(group=group,color=group),width=0.3) +
        geom_text(data=extremes_high,aes(label=str_wrap(paste0(Country," (",Period,")"),width=12)),color="darkgrey",size=3,nudge_x = 0.35,nudge_y=-0.05,show.legend = FALSE) +
        geom_text(data=extremes_low,aes(label=str_wrap(paste0(Country," (",Period,")"),width=12)),color="darkgrey",size=3,nudge_x = 0.35,nudge_y=0.05,show.legend = FALSE) +      
        coord_flip() +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.text = element_text(size = 12),
              legend.position="top",
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.title = element_text(lineheight=.5),
              #axis.text.x = element_blank(),
              axis.text.y = element_text(size=13),
              axis.title.x = element_blank(),
              axis.title.y = element_blank()
              #axis.ticks = element_blank()
        )
      
    }  
    
  } else { #click
    
    selectedPoint <- tsne_points_filter %>%
      filter(Country == clickCountry, Period == clickPeriod) %>%
      dplyr::select(Country,Period,indicator, value)
    
    ggplot(data=tsne_ready_gather,aes(indicator,value)) + 
      geom_boxplot(color="darkgrey") +  
      geom_jitter(data=tsne_points_filter,aes(group=group,color=group),alpha=0.5,width=0.3) + 
      geom_point(data=selectedPoint,aes(fill=paste0(Country," (",Period,")")),color="blue",size=4) +
      geom_text(data=extremes_high,aes(label=str_wrap(paste0(Country," (",Period,")"),width=12)),color="darkgrey",size=3,nudge_x = 0.35,nudge_y=-0.05,show.legend = FALSE) +
      geom_text(data=extremes_low,aes(label=str_wrap(paste0(Country," (",Period,")"),width=12)),color="darkgrey",size=3,nudge_x = 0.35,nudge_y=0.05,show.legend = FALSE) +      
      coord_flip() +
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            legend.text = element_text(size = 12),
            legend.position="top",
            panel.border = element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(lineheight=.5),
            axis.text.y = element_text(size=13),
            #axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()
            #axis.ticks = element_blank()
      )
    
  }
  
  
}

# brush tooltip
.summary_brush <- function(brushedP,selected_indicators){
  
#   tsne_points_filter$id <- paste0("X",brushedP$id)
  #these_indicators <- paste0("X",filter(indicators_1_2, name %in% selected_indicators)$id)
  these_indicators <- paste0("X",unique(filter(data_attributes, Series_Name %in% selected_indicators)$Series_Code))
  #brushedP <- tsne_points_filter
  tsne_points_mean <- tsne_points_filter %>%
    filter(id %in% these_indicators) %>%
    group_by(id) %>%
    #spread(id,Observation) %>%
    #select(Country,Period,one_of(these_indicators),x,y) %>%
    mutate(Avg = mean(Observation,na.rm=TRUE), Std = sd(Observation,na.rm=TRUE)) %>%
    #select(-Indicator) %>%
    distinct(id,.keep_all=TRUE) %>%
    as.data.frame()
  
  return(tsne_points_mean)
}

# Staff Photo data
get_operational_profile_photo <- function(pm) {
  
  Talent <- RODBC::odbcDriverConnect('DRIVER={ODBC Driver 13 for SQL Server};
    SERVER=WBGMSSQLEOA001,5800;Database=WBG;uid=EFI_User;pwd=Efi2017go!')
  
  data <- sqlQuery(Talent, paste0("SELECT top 1 [Employee Photo] FROM Analytics.Employee_TRS_Project_Team Where [Employee Full Name]='",pm,"'"))
  
  names(data) <- gsub(" ","_",names(data))
  
  # copy image to www so I can render it later in LaTeX (PDF)
  #download.file(as.character(data[1,1]), paste0("www/",gsub(" ","_",pm),".jpg"), mode = "wb")
  
  return(data)
}