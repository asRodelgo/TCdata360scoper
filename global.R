# load global packages ----------------------------------------------
library(dplyr) # manipulate data 
library(ggplot2) # charts
library(data.table) # fast operations
library(tidyr) # transform data
library(stringr) # manipulate strings
require(tsne) # t-SNE algorithm
library(DT) # customize dataTable javascript library
library(reshape2) # manipulate data
library(knitr) # generate LaTeX PDF report
library(jsonlite)

# global data and functions -----------------------------------------

# bookmark this app
enableBookmarking(store = "url")

# Read the global data available for the whole session. Will be loaded only once
source("data/read_data.R", local = TRUE)
# These functions are called before the actual server work. They will be loaded for the
# session
helpers <- file.path("helper_functions", list.files("helper_functions", full.names = FALSE))
for (h in helpers) source(h, local = TRUE)

# enhance datascope data with descriptors to main variables
datascope_filter <- .filter_datascope()

# ----------- tSNE data topology
data_tsne <- read.csv("data/data_tsne.csv",stringsAsFactors = FALSE)
data_tsne$Period <- as.character(data_tsne$Period) # to avoid continuous gradient color
data_tsne_sample <- filter(data_tsne, Period < "2017" & Period > "1995")
tsne_ready <- cbind(data_tsne_sample,tsne_points)
names(tsne_ready)[ncol(tsne_ready)-1] <- "x"
names(tsne_ready)[ncol(tsne_ready)] <- "y"
# Default selector choices for tsne -----------
countries_list <- sort(unique(data_tsne_sample$Country))
periods_list <- sort(unique(data_tsne_sample$Period))
regions_list <- sort(unique(data_tsne_sample$Region))
indicators_list <- names(data_tsne_sample)[!sapply(data_tsne_sample, is.character)]
indicators_list <- gsub("X","",indicators_list)
#
indicator_selection_plots <- filter(indicators_1_2, rank == 1)$id 
#("289","315","321","350","361","416")
indicator_selection_names <- filter(indicators_1_2, id %in% indicator_selection_plots)$name
# indicator_selection_plots_short <- c("Ease_DB","Corruption","Unemployed",
#                                      "LPI","MFN_Tariff","Remittances",
#                                      "Manufac",
#                                      "Export","Import","Income")
# filter data by selected indicators in order to show the actual values on the 
# topology tab
selected_datascope_data <- filter(datascope, id %in% indicator_selection_plots)
# ---------------
# TCdata360 URLs
country_url <- "http://tcdata360.worldbank.org/countries/"
indicator_url <- "http://tcdata360.worldbank.org/indicators/"
tsne_url <- "http://distill.pub/2016/misread-tsne/"

