# write data talent

library(RODBC)
require(tidyverse)

EFI_ANALYTICS <- RODBC::odbcDriverConnect('DRIVER={SQL Server};
                                          SERVER=WBGMSSQLEOA001,5800;Database=WBG;uid=EFI_User;pwd=Efi2017go!')

# Prepare data
sqlTables(EFI_ANALYTICS)
# Employee_TRS_Project_Team
# RM_Employee_Project
talent <- sqlQuery(EFI_ANALYTICS, "SELECT * FROM Analytics.Employee_TRS_Project_Team ")
#talent <- sqlQuery(EFI_ANALYTICS, "SELECT top 100 * FROM Analytics.Employee_TRS_Project_Team ")
# WHERE [WB Practice Code] = 'TAC' 
names(talent) <- gsub(" ", "_", names(talent))
staff <- select(talent, Employee_Full_Name, Employee_Last_Name) %>% distinct()
talentHead <- head(talent)
talent_EFI <- filter(talent, !is.na(Employee_Practice_Code)) %>% mutate(Employee_Practice_Code = trimws(Employee_Practice_Code)) %>%
  filter(Employee_Practice_Code %in% c("TAC","GOV","POV","FAM","MFM"))
names(talent_EFI) <- gsub("/","_",names(talent_EFI),fixed=TRUE)

# make factors numeric except for reference variables
# Start with Employee UPI
talent2 <- mutate_at(talent_EFI, vars(-c(Employee_UPI_9)), funs(as.numeric(.))) %>%
  filter(!is.na(Employee_UPI_9), !is.na(Project_ID)) %>%
  group_by(Employee_UPI_9) %>% summarise_all(funs(mean(.,na.rm=TRUE)))

talent_original <- filter(talent_EFI, !is.na(Employee_UPI_9), !is.na(Project_ID)) %>%
  group_by(Employee_UPI_9) %>% mutate_all(funs(paste0(.)))

# remove all columns with all NAs
data <- talent2[,!(colSums(is.na(talent2))==nrow(talent2))]

data <- gather(data, Series_Code, Observation, -c(Employee_UPI_9)) %>% 
  mutate_at(vars(-c(Observation)), funs(as.character(.))) %>% 
  rename(X2017 = Observation) %>%
  as.data.frame()

data_original <- talent_original[,!(colSums(is.na(talent_original))==nrow(talent_original))]

data_original <- gather(data_original, Series_Code, Observation, -c(Employee_UPI_9)) %>% 
  mutate_at(vars(-c(Observation)), funs(as.character(.))) %>% 
  rename(X2017 = Observation) %>% 
  distinct(Employee_UPI_9,Series_Code,.keep_all=TRUE) %>%
  as.data.frame()

#names(data)[substr(names(data),1,1)==2] <- paste0("X",names(data)[substr(names(data),1,1)==2])

#######################################################################################

# Call functions to create t-sne

# helper functions
writers <- file.path("writers", list.files("writers", full.names = FALSE))
for (w in writers) source(w, local = TRUE)
helpers <- file.path("helper_functions", list.files("helper_functions", full.names = FALSE))
for (h in helpers) source(h, local = TRUE)

# put attributes away for later
data_attributes <- select(data, Country_Code = Employee_UPI_9, Series_Code) %>%
  mutate(Country_Name = Country_Code, Series_Name = Series_Code) %>%
  distinct(.keep_all=TRUE)

# reduce data to minimum required for tSNE
# main_object: variable to plot
# Period: time period
# indicatorID: dimensions to be reduced
data <- select(data, main_object = Employee_UPI_9, indicatorID = Series_Code, starts_with("X"))
data_original <- select(data_original, main_object = Employee_UPI_9, indicatorID = Series_Code, starts_with("X"))

### thematic subsets: -------------------------------------

## Project
#data <- filter(data, grepl("^WB",indicatorID))
#data_original <- filter(data_original, grepl("^WB",indicatorID))
#data_attributes <- filter(data_attributes, grepl("^WB",Series_Code))

## Employee
#data <- filter(data, grepl("^Employee",indicatorID))
#data_original <- filter(data_original, grepl("^Employee",indicatorID))
#data_attributes <- filter(data_attributes, grepl("^Employee",Series_Code))

## TRS
#data <- filter(data, grepl("^TRS",indicatorID))
#data_original <- filter(data_original, grepl("^TRS",indicatorID))
#data_attributes <- filter(data_attributes, grepl("^TRS",Series_Code))

## Proximity (physical)
# indicator_List_Proximity <- c("Employee_City_Code", # To distinguish between IBRD and IFC use: "Employee_Company","Employee_Division",
#                               "Employee_Duty_Country","Employee_Location",
#                               "Employee_Practice_Code","Employee_Room_Nbr")
## Proximity (skills)
indicator_List_Proximity <- c(# To distinguish between IBRD and IFC use: "Employee_Company","Employee_Division",
                              "Employee_FCS_Country_Flag","Employee_Job_Code",
                              "Employee_Primary_Sector",
                              "Employee_Subtype","Employee_Unit","Team_Role")
data <- filter(data,indicatorID %in% indicator_List_Proximity)

## Add Term Document Matrix for Employee_Skill variable ------ ## SEE: tm_analysis.R
tdm <- read.csv("data/termDocMatrix.csv")
tdm <- gather(tdm, indicatorID, X2017, - X)
names(tdm)[1] <- "main_object"
tdm$main_object <- as.character(tdm$main_object)
data <- bind_rows(data,tdm)


data_original <- filter(data_original, indicatorID %in% indicator_List_Proximity)
data_attributes <- filter(data_attributes, Series_Code %in% indicator_List_Proximity)

write.csv(data_attributes, "data/data_attributes.csv", row.names = FALSE)

## to display attributes when mouse over the dots
#data_filter <- .filter_datascope(data, isCountry = FALSE)
data_filter <- .filter_datascope(data_original, isCountry = FALSE)
write.csv(data_filter, "data/data_filter.csv", row.names = FALSE)

# 2. Call: .prepare_data() (that calls: .filter_datascope_data()) and
## add attributes to main_object
 data_tsne <- .prepare_data(data)
## remove all columns with all NAs
 data_tsne <- data_tsne[,!(colSums(is.na(data_tsne))==nrow(data_tsne))]
# Ficticious countries data.frame:

countries <- select(talent, iso3 = Employee_UPI_9, region = Employee_Practice_Code, name = Employee_Full_Name, incomeLevel = Employee_Employment_Status) %>% 
  mutate(region = trimws(region)) %>%
  distinct(.keep_all=TRUE) %>% mutate_all(funs(as.character(.)))

data_tsne <- left_join(data_tsne,countries[,c("iso3","region","name","incomeLevel")], by = c("main_object"="iso3")) %>%
  select(main_object, Period, Region = region, Country=name, IncomeLevel = incomeLevel, everything())

 write.csv(data_tsne, "data/data_tsne.csv",row.names = FALSE)

# 3. Call: .generateTSNE() which calls .prepare_data() which calls: .filter_datascope_data() and writes tsne_points.csv to disk
 library(tsne)
.generateTSNE(data, periodMin = "1900", periodMax = "2100",
                             num_iter = 400, max_num_neighbors = 20, num_epochs = 100)

#write.csv(tsne_points, "data/tsne_points.csv", row.names = FALSE)
