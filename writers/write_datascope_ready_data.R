# Datascope writer ---------------------------------
library(jsonlite)
library(tidyverse)
# Query data based on ids of filtered indicators
# loop by country and indicator id. Bind it all in a data.frame
datascope <- data.frame()
specialchars <- paste(c("[-]","[.]"),collapse = "|")
#for (cou in c("BRA")){
for (cou in countries$id){
  for (ind in indicators_1_2$id){
    print(paste0("Processing...",cou," ",ind))
    thisQuery <- fromJSON(paste0("http://datascope-prod.amida-demo.com/api/v1/data?countries=",cou,
                                 "&indicators=",ind),
                          flatten = TRUE)
    if (length(thisQuery$data)>0){
      thisQuery <- flatten(thisQuery$data$indicators[[1]])
      if (!is.null(thisQuery$estimated)){
        thisQuery$estimated <- NULL
        thisQuery <- as.data.frame(thisQuery)
      }  
      thisQuery <- thisQuery %>%
        mutate(iso3 = cou)
      names(thisQuery) <- gsub("values.","",names(thisQuery),fixed=TRUE)
      names(thisQuery) <- ifelse(grepl(specialchars,names(thisQuery)),substr(names(thisQuery),1,4),names(thisQuery))
      # consolidate quarterly data by the 4th quarter
      names(thisQuery) <- gsub("Q4","",names(thisQuery))
      thisQuery <- select(thisQuery, -dplyr::contains("Q"))

      if (nrow(datascope)==0) {
        datascope <- thisQuery
      } else {
        datascope <- bind_rows(datascope,thisQuery)
      }
    }
  }
}
# remove Quarters
#datascope <- select(datascope, -dplyr::contains("Q"))
# remove estimated and product detail columns
# datascope <- datascope %>%
#   select(iso3,id, everything(), -products)
write.csv(datascope,"data/datascope.csv",row.names = FALSE)
# -----------------------------------------------------------

