# -------------- Read data at start up
#
source("data/datascope_api.R", local = TRUE)

# ---------------------------------
# tsne pre-calculated cloud of points
tsne_points <- read.csv("data/tsne_points.csv",stringsAsFactors = FALSE)

