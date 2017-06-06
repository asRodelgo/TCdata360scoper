# write data talent

library(RODBC)
require(tidyverse)

EFI_ANALYTICS <- RODBC::odbcDriverConnect('DRIVER={SQL Server};
                                          SERVER=WBGMSSQLEOA001,5800;Database=WBG;uid=EFI_User;pwd=Efi2017go!')

# Prepare data

test <- sqlQuery(EFI_ANALYTICS, "SELECT * FROM Analytics.RM_Employee_Project WHERE [WB Practice Code] = 'TAC' " )
names(test) <- gsub(" ", "_", names(test))

# make factors numeric except for reference variables
# Start with Employee UPI
test2 <- mutate_at(test, vars(-c(Employee_UPI_9)), funs(as.numeric(.))) %>%
  filter(!is.na(Employee_UPI_9), !is.na(WB_Project_ID)) %>%
  group_by(Employee_UPI_9) %>% summarise_all(funs(mean(.,na.rm=TRUE)))

# remove all columns with all NAs
data <- test2[,!(colSums(is.na(test2))==nrow(test2))]

data <- gather(data, Series_Code, Observation, -c(Employee_UPI_9)) %>% 
  mutate_at(vars(-c(Observation)), funs(as.character(.))) %>% 
  rename(X2017 = Observation) %>%
  as.data.frame()

#names(data)[substr(names(data),1,1)==2] <- paste0("X",names(data)[substr(names(data),1,1)==2])

#######################################################################################

# Call functions to create t-sne

# helper functions
writers <- file.path("writers", list.files("write", full.names = FALSE))
for (w in writers) source(w, local = TRUE)
helpers <- file.path("helper_functions", list.files("helper_functions", full.names = FALSE))
for (h in helpers) source(h, local = TRUE)

# put attributes away for later
data_attributes <- select(data, Country_Code = Employee_UPI_9, Series_Code) %>%
  mutate(Country_Name = Country_Code, Series_Name = Series_Code) %>%
  distinct(.keep_all=TRUE)

write.csv(data_attributes, "data/data_attributes.csv", row.names = FALSE)

# reduce data to minimum required for tSNE
# main_object: variable to plot
# Period: time period
# indicatorID: dimensions to be reduced
data <- select(data, main_object = Employee_UPI_9, indicatorID = Series_Code, starts_with("X"))
## to display attributes when mouse over the dots
data_filter <- .filter_datascope(data, isCountry = FALSE)
write.csv(data_filter, "data/data_filter.csv", row.names = FALSE)

# 2. Call: .prepare_data() (that calls: .filter_datascope_data()) and
## add attributes to main_object
 data_tsne <- .prepare_data(data)
## remove all columns with all NAs
 data_tsne <- data_tsne[,!(colSums(is.na(data_tsne))==nrow(data_tsne))]

# Ficticious countries data.frame:
countries <- select(test, iso3 = Employee_UPI_9, region = Employee_Unit, name = Employee_Full_Name, incomeLevel = Employee_Employment_Status) %>% 
  distinct(.keep_all=TRUE) %>% mutate_all(funs(as.character(.)))

data_tsne <- left_join(data_tsne,countries[,c("iso3","region","name","incomeLevel")], by = c("main_object"="iso3")) %>%
  select(main_object, Period, Region = region, Country=name, IncomeLevel = incomeLevel, everything())

 write.csv(data_tsne, "data/data_tsne.csv",row.names = FALSE)

# 3. Call: .generateTSNE() which calls .prepare_data() which calls: .filter_datascope_data() and writes tsne_points.csv to disk
.generateTSNE(data, periodMin = "1900", periodMax = "2100",
                             num_iter = 800, max_num_neighbors = 10, num_epochs = 100)

#write.csv(tsne_points, "data/tsne_points.csv", row.names = FALSE)
