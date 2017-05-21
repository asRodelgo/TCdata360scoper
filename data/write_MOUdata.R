# FIG data -----------

# 1. Read data
require(tidyverse)

# helper functions
writers <- file.path("writers", list.files("write", full.names = FALSE))
for (w in writers) source(w, local = TRUE)
helpers <- file.path("helper_functions", list.files("helper_functions", full.names = FALSE))
for (h in helpers) source(h, local = TRUE)

# read data from source and get it ready for the app
data <- read.csv("/Users/asanchez3/Desktop/Work/EFI_Dashboards/data/backups/EFI_MOU_Details.csv",
                 stringsAsFactors = FALSE)
# put attributes away for later
data_measures <- select_if(data, is.numeric)
data_objects <- select(data, main_object = WB_Project_ID, Period = MOU_Fiscal_Year, main_object_desc = WB_Project_Name)

data_attributes <- bind_cols(data_objects, data_measures)
data_attributes <- gather(data_attributes, Series_Code, Observation, -main_object, -Period, -main_object_desc) %>%
  select(Country_Code = main_object, Country_Name = main_object_desc, Series_Code, Period, Observation) %>% mutate(Series_Name = Series_Code)
  
write.csv(data_attributes, "data/data_attributes.csv", row.names = FALSE)

# reduce data to minimum required for tSNE
# main_object: variable to plot
# Period: time period
# indicatorID: dimensions to be reduced
data <- select(data_attributes, main_object = Country_Code, indicatorID = Series_Code, Period, Observation) %>%
  mutate(Period = paste0("X",Period)) %>% distinct(main_object, indicatorID, Period, .keep_all=TRUE) %>%
  spread(Period, Observation)
# to display attributes when mouse over the dots
data_filter <- .filter_datascope(data, isCountry = FALSE)
write.csv(data_filter, "data/data_filter.csv", row.names = FALSE)

# 2. Call: .prepare_data() (that calls: .filter_datascope_data()) and
# add attributes to main_object
data_tsne <- .prepare_data(data)

data_tsne <- left_join(data_tsne,countries[,c("iso3","region","name","incomeLevel")], by = c("main_object"="iso3")) %>%
  select(main_object, Period, Region = region, Country=name, IncomeLevel = incomeLevel, everything())

write.csv(data_tsne, "data/data_tsne.csv",row.names = FALSE)

# 3. Call: .generateTSNE() which calls .prepare_data() which calls: .filter_datascope_data()
data_points <- .generateTSNE(data, periodMin = "1900", periodMax = "2100",
                             num_iter = 400, max_num_neighbors = 50, num_epochs = 100)

write.csv(tsne_points, "data/tsne_points.csv", row.names = FALSE)


