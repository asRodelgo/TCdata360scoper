# FIG data -----------

# 1. Read data
require(readxl)

data <- read_excel("data/Data_Extract_From_FIG.xlsx")
names(data) <- gsub("[0-9]* \\[[A-Z]*","X",names(data))
names(data) <- gsub("\\]","",names(data))
names(data) <- gsub(" ","_",names(data))

data <- data %>% mutate_at(vars(matches("[0-9]")), as.numeric)

# 2. Call: .prepare_data() (that calls: .filter_datascope_data())
# 3. Call: .generateTSNE()
  
