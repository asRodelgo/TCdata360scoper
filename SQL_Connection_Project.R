#
library(RODBC)
library(tidyverse)

EFI_ANALYTICS <- RODBC::odbcDriverConnect('DRIVER={SQL Server};
                                          SERVER=WBGMSSQLEOA001,5800;Database=WBG;uid=EFI_User;pwd=Efi2017go!')


sqlTables(EFI_ANALYTICS)

project_Gender <- sqlQuery(EFI_ANALYTICS, 
"SELECT [WBG Project ID],[WBG Project Name],[WBG Project Status],[WBG PM/Manager],[WBG Project Region],[WBG Project Country],
[WBG Gender Flag Type],[WBG Project Practice],[WBG Project Stage],[WBG Product Type] FROM Operations.Project 
WHERE [WBG Gender Component] = 'Gender Component'
                           ")
#WHERE [WBG Project Region] = 'LCR' AND [WBG Project Status] IN ('Active','Pipeline') AND

write.csv(project_Gender, "C:/Users/wb493327/Desktop/projects_Gender_Aug23_2017.csv", row.names = FALSE)
