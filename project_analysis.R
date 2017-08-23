## project analysis

library(tidyverse)

projectCount <- select(talent_EFI, Employee_UPI_9, WBG_Project_ID, TRS_FY, TRS_Staff_Weeks) %>%
  filter(!is.na(WBG_Project_ID), as.character(TRS_FY) > "FY14") %>%
  group_by(Employee_UPI_9,WBG_Project_ID) %>%
  mutate(count_projects = n_distinct(WBG_Project_ID), Total_Staff_Weeks = sum(TRS_Staff_Weeks, na.rm = TRUE)) %>% 
  ungroup() %>%
  distinct(Employee_UPI_9,WBG_Project_ID,count_projects) %>%
  spread(WBG_Project_ID, count_projects)


# Returns very sparse matrices just like tm_analysis with Employee_Skill
# Try creating a text field containing project name information and other relevant information and repeat tm_analysis with it
# Select text fields with relevant information and create a new column that concatenates all the information

selectedSkills <- select(talent_EFI, Employee_UPI_9, Employee_Degree_Name, Employee_Job_Title, Employee_Language, Employee_Primary_Specialization,
#Employee_Skill 
`Employee_T&C_Cross-Cutting_Affiliation`, `Employee_T&C_Primary_Affiliation`, `Employee_T&C_Secondary_Affiliation`, WBG_Project_ID, WBG_Product,
WBG_Project_Name, WBG_Project_Region) %>%
  filter(!is.na(WBG_Project_ID))

upiSkills <- select(selectedSkills, -c(WBG_Project_ID, WBG_Product, WBG_Project_Name, WBG_Project_Region)) %>%
  distinct(Employee_UPI_9, .keep_all=TRUE) %>%
  mutate(pastedSkills = paste(Employee_Degree_Name, Employee_Job_Title, Employee_Language, Employee_Primary_Specialization,
                              `Employee_T&C_Cross-Cutting_Affiliation`, `Employee_T&C_Primary_Affiliation`, `Employee_T&C_Secondary_Affiliation`)) %>%
  select(Employee_UPI_9, pastedSkills)


projectSkills <- select(selectedSkills, Employee_UPI_9, WBG_Project_ID, WBG_Product, WBG_Project_Name, WBG_Project_Region) %>%
  group_by(Employee_UPI_9,WBG_Project_ID) %>%
  mutate(projectText = paste(WBG_Product, WBG_Project_Name, WBG_Project_Region)) %>%
  distinct(Employee_UPI_9,WBG_Project_ID, .keep_all=TRUE) %>%
  select(Employee_UPI_9,WBG_Project_ID,projectText) %>%
  ungroup() %>%
  as.data.frame() %>% 
  group_by(Employee_UPI_9) %>%
  summarise(pasteText = paste(projectText, collapse = " ")) %>%
  distinct(Employee_UPI_9,pasteText)
  
upiSkills <- merge(upiSkills,projectSkills, by = "Employee_UPI_9") %>% 
  mutate(Employee_All_Skills = paste(pastedSkills,pasteText)) %>%
  distinct(Employee_UPI_9,Employee_All_Skills)
  
upiSkills2 <- merge(upiSkills,talent_EFI[,c("Employee_UPI_9","Employee_Full_Name")], by="Employee_UPI_9", all.x = TRUE) %>% 
  distinct(Employee_UPI_9,.keep_all=TRUE)
  
# #### Understanding the clusters -------------------------
# # group 1  
# filter(upiSkills2, grepl("cusolito|silva mendez|michel bellier|anael kayani|chenjerani|diletta dor|kalavakon|mneney|musoke muna|nistha sinha|tegwa",tolower(Employee_Full_Name)))  
# # group 2  
# filter(upiSkills2, grepl("mallari|de aguiar falco|nicholas menzies|meg dondog|imogen cara|apurva sanghi|pierre m. lenaud|grant wai-poi|miriam bensky|bronwyn grie|georgia harley",tolower(Employee_Full_Name)))    
# # group 3  
# filter(upiSkills2, grepl("aliyev|henry amena|karen grigorian|kisunko|robertus cornelis|luz maria meyer|abreu rojas|wangari kamau|fuente hoyes|crnomarkovic|kolie ousmane",tolower(Employee_Full_Name)))    
# 
# filter(upiSkills2, grepl("85662|295924|257897",tolower(Employee_UPI_9)))
