# -------- Writers don't run when the app is called. They are used for pre-processing

# TSNE writer
.generateTSNE <- function(){
  
  datascope <- read.csv("data/datascope.csv", stringsAsFactors = FALSE)
  
  data_tsne <- .prepare_data()
  
  data_tsne_sample <- filter(data_tsne, Period > "1995" & Period < "2017")
  
  if (nrow(data_tsne)>0){
    num_iter <- 500
    max_num_neighbors <- 50
    set.seed(456) # reproducitility
    tsne_points <- tsne(data_tsne_sample[,!sapply(data_tsne, is.character)], 
                        max_iter=as.numeric(num_iter), 
                        perplexity=as.numeric(max_num_neighbors), 
                        epoch=100)
    # add jitter
    tsne_points <- tsne_points + runif(length(tsne_points),-1,1)
    #plot(tsne_points_jit)
    plot(tsne_points)
  } else {
    tsne_points <- c()
  }
  write.csv(tsne_points, "data/tsne_points.csv", row.names = FALSE)
}

# Run it!
.generateTSNE()
