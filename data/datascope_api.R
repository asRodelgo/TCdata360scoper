# -------- Read Datascope API
datascope <- read.csv("data/datascope.csv",stringsAsFactors = FALSE)
#
# Query country metadata:
countries <- read.csv("data/countries.csv", stringsAsFactors = FALSE)
# Query indicators:
indicators <- read.csv("data/indicators.csv",stringsAsFactors = FALSE)
# Top ranked and cleaned up indicators
indicators_1_2 <- read.csv("data/indicators_1_2.csv",stringsAsFactors = FALSE)


