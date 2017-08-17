# TRS Project TTL-ing workload evolution
library(tidyverse)

selectedTRS <- dplyr::select_(talent_EFI, "WBG_Project_ID","TRS_FY","WBG_Team_Lead","WBG_Project_Practice","TRS_Staff_Weeks",
                              "WBG_Project_Status","WBG_Product_Type","TRS_Employee_Full_Name","Team_Role") %>%
  filter(Team_Role == "Team Leader", TRS_FY %in% c("FY14","FY15","FY16","FY17","FY18"), WBG_Project_Status %in% c("Active","Closed","Pipeline"))

# count distinct #projects and Sum staff weeks
workload_TRS <- group_by(selectedTRS, TRS_Employee_Full_Name, TRS_FY, WBG_Project_Practice) %>%
  mutate(count_projects = n_distinct(WBG_Project_ID), Total_Staff_Weeks = sum(TRS_Staff_Weeks, na.rm = TRUE)) %>% 
  ungroup() %>%
  distinct(TRS_Employee_Full_Name, WBG_Project_ID, TRS_FY, WBG_Project_Practice, .keep_all = TRUE)

write.csv(workload_TRS, "C:/Users/wb493327/Desktop/workload_TRS.csv", row.names = FALSE)
  




################################### ---------------------------------------------------
# read from Projects
EFI_ANALYTICS <- RODBC::odbcDriverConnect('DRIVER={SQL Server};
                                          SERVER=WBGMSSQLEOA001,5800;Database=WBG;uid=EFI_User;pwd=Efi2017go!')

project <- sqlQuery(EFI_ANALYTICS, "SELECT * FROM Operations.Project")
names(project) <- gsub(" ", "_", names(project))

fy_preparation <- filter(project, WBG_Project_Practice %in% c("TAC","GOV","POV","FAM","MFM")) %>%
  distinct(WB_Approval_FY, WBG_Project_Practice, WBG_Team_Lead,WBG_Project_ID) %>%
  filter(!is.na(WB_Approval_FY)) %>%
  mutate_all(as.character)

fy_implementation <- filter(project, WBG_Project_Practice %in% c("TAC","GOV","POV","FAM","MFM")) %>%
  distinct(WBG_Board_Approval_or_Implementation_Start_FY,WBG_Project_Practice, WBG_Team_Lead, WBG_Project_ID) %>%
  filter(!is.na(WBG_Board_Approval_or_Implementation_Start_FY)) %>%
  mutate_all(as.character)
  
  
  
  