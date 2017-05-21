# FIG data -----------

# 1. Read data
require(readxl)
require(tidyverse)

# helper functions
writers <- file.path("writers", list.files("write", full.names = FALSE))
for (w in writers) source(w, local = TRUE)
helpers <- file.path("helper_functions", list.files("helper_functions", full.names = FALSE))
for (h in helpers) source(h, local = TRUE)

# read data from source and get it ready for the app
data <- read_excel("data/Data_Extract_From_FIG.xlsx")
names(data) <- gsub("[0-9]* \\[[A-Z]*","X",names(data))
names(data) <- gsub("\\]","",names(data))
names(data) <- gsub(" ","_",names(data))
data <- data %>% mutate_at(vars(matches("[0-9]")), as.numeric)
# put attributes away for later
data_attributes <- dplyr::select(data, -starts_with("X")) %>% distinct(.keep_all=TRUE)
write.csv(data_attributes, "data/data_attributes.csv", row.names = FALSE)

# reduce data to minimum required for tSNE
# main_object: variable to plot
# Period: time period
# indicatorID: dimensions to be reduced
data <- select(data, main_object = Country_Code, indicatorID = Series_Code, starts_with("X"))
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


