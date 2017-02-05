# ------- Prepare data for tSNE generation (writer) and display
# Data processing
.filter_datascope_data <- function(){
  
  data_filter <- datascope %>%
    gather(Period,Observation,-iso3,-id) %>%
    inner_join(indicators_1_2, by="id") %>%
    dplyr::select(iso3,id,Period,Observation,Indicator=name) %>%
    distinct(iso3, Period, Indicator, .keep_all=TRUE) %>%
    mutate(Period = gsub("X","",Period)) %>%
    inner_join(select(countries,iso3,Country=name,Region=region,IncomeLevel=incomeLevel),
               by="iso3")
  
  #   data_merge <- mutate(data_merge, IndicatorShort = gsub(" ","_",IndicatorShort))
  #   data_merge <- mutate(data_merge, IndicatorShort = gsub("(","",IndicatorShort,fixed = TRUE))
  #   data_merge <- mutate(data_merge, IndicatorShort = gsub(")","",IndicatorShort,fixed = TRUE))
  #   data_merge <- mutate(data_merge, IndicatorShort = gsub("%","perc",IndicatorShort,fixed = TRUE))
  #   data_merge <- mutate(data_merge, IndicatorShort = gsub(",","",IndicatorShort,fixed = TRUE))
  #   data_merge <- mutate(data_merge, IndicatorShort = gsub("$","Dollars",IndicatorShort,fixed = TRUE))
  #   data_merge <- mutate(data_merge, IndicatorShort = gsub(":","",IndicatorShort,fixed = TRUE))
  #   data_merge <- mutate(data_merge, IndicatorShort = gsub("=","_",IndicatorShort,fixed = TRUE))
  #   data_merge <- mutate(data_merge, IndicatorShort = gsub("&","and",IndicatorShort,fixed = TRUE))
  #   data_merge <- mutate(data_merge, IndicatorShort = gsub(".","",IndicatorShort,fixed = TRUE))
  #   data_merge <- mutate(data_merge, IndicatorShort = gsub("-","_",IndicatorShort,fixed = TRUE))
  #   data_merge <- mutate(data_merge, IndicatorShort = gsub("/","_",IndicatorShort,fixed = TRUE))
  #   data_merge <- distinct(data_merge, CountryCode, Period, IndicatorShort, .keep_all = TRUE)
  
  # BOTTLENECK ########
  data_spread <- spread(data_filter, id, Observation)
  # make id columns start with a non-numeric character
  names(data_spread)[!sapply(data_spread, is.character)] <- paste0("X",names(data_spread)[!sapply(data_spread, is.character)])
  # remove all NA rows
  data_tsne <- data_spread[rowSums(is.na(data_spread))<ncol(data_spread[, !sapply(data_spread, is.character)]),]
  # remove all NA columns
  data_tsne <- data_tsne[,colSums(is.na(data_tsne))<nrow(data_tsne[, !sapply(data_tsne, is.character)])]
  
  # BOTTLENECK ########
  data_tsne <- data_tsne %>%
    group_by(iso3,Period) %>%
    mutate_if(is.numeric, funs(sum(.,na.rm=TRUE))) %>%
    distinct(iso3,Period,.keep_all=TRUE) %>%
    mutate_if(is.numeric, funs(na_if(.,0)))
  data_tsne <- as.data.frame(data_tsne)
  
  return(data_tsne)
}

# Prepare data for tSNE algorithm
.prepare_data <- function(){
  
  data_tsne <- .filter_datascope_data()
  
  # calculate missing values by indicator
  num_col <- ncol(data_tsne[, !sapply(data_tsne, is.character)])
  data_missing <- data_tsne %>%
    mutate(missing_values = rowSums(is.na(.))/num_col) %>%
    dplyr::select(Country, Period, missing_values) %>%
    distinct(Country,Period,.keep_all=TRUE)
  
  # impute NAs by the global mean + jitter which proved to work better visually than other imputations
  data_tsne <- data_tsne %>%
    mutate_if(is.numeric, funs(replace(., which(is.na(.)), mean(., na.rm=TRUE) * rnorm(length(.),1,0.02))))
  data_tsne <- as.data.frame(data_tsne)
  
  # scale to [0,1] to improve tsne final shape
  maxs <- apply(data_tsne[,!sapply(data_tsne, is.character)], 2, function(x) { max(x,na.rm=TRUE)}) 
  mins <- apply(data_tsne[,!sapply(data_tsne, is.character)], 2, function(x) { min(x,na.rm=TRUE)})
  data_tsne[,!sapply(data_tsne, is.character)] <- as.data.frame(scale(data_tsne[,!sapply(data_tsne, is.character)], center = mins, scale = maxs - mins))
  # Remove NaN
  data_tsne <- data_tsne %>%
    mutate_if(is.numeric, funs(replace(., which(is.nan(.)), mean(., na.rm=TRUE) * rnorm(length(.),1,0.02))))
  data_tsne <- as.data.frame(data_tsne)
  
  data_tsne <- distinct(data_tsne, iso3, Period, .keep_all = TRUE)
  data_tsne <- merge(data_tsne, data_missing, by=c("Country","Period"))
  data_tsne$missing_values <- as.numeric(data_tsne$missing_values)
  data_tsne[,!sapply(data_tsne, is.character)] <- round(data_tsne[,!sapply(data_tsne, is.character)],3)
  
  return(data_tsne)
  
}
# Run it!
write.csv(data_tsne, "data/data_tsne.csv",row.names = FALSE)

#.prepare_data()

