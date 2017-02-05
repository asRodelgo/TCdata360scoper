# ----- Writer countries and indicators
library(jsonlite)
library(tidyverse)
# Query country metadata:
countries <- fromJSON("http://datascope-prod.amida-demo.com/api/v1/countries/?fields=id%2Ciso2%2Ciso3%2Cname%2Cregion%2CincomeLevel%2ClendingType%2CcapitalCity%2Cgeo",
                      flatten = TRUE)
write.csv(countries, "data/countries.csv",row.names = FALSE)
# Query indicators:
indicators <- fromJSON("http://datascope-prod.amida-demo.com/api/v1/indicators?fields=id%2Cname%2Cdataset%2CvalueType%2CdatasetId%2Cnotes%2Cproperties%2Crank%2Cdefinition",
                       flatten=TRUE)
write.csv(indicators, "data/indicators.csv",row.names = FALSE)
# Keep only indicators used in tSNE algorithm:
types_allowed <- c("value","percent_of_gdp","dtf","usd_millions","us_dollars","percent")

indicators_1_2 <- indicators %>%
  filter(rank < 3, valueType %in% types_allowed & !(datasetId==23)) %>% # avoid Services Restrict Index
  distinct(name,.keep_all=TRUE) %>%
  arrange(name)

write.csv(indicators_1_2, "data/indicators_1_2.csv",row.names = FALSE)
