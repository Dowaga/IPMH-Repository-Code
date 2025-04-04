# Header ------------------------------------------------------------------

# Author(s): David
# Date: March 10, 2025
# Script to facilitate import of data from sharepoint

# Setup ------------------------------------------------------------------------
source("DataTeam_ipmh.R")
source("Dependencies.R")
file_date <- fileSnapshot(file.path(ipmh_filepath, "/Data/6. RCT PPW data/"))
file_date <- rownames(file_date$info[which.max(file_date$info$mtime),])
file_date <- gsub("^.*?_2","2",file_date)
file_date <- str_remove(file_date,".csv")
file_date <- format(file_date, format="%d %B %Y")



ppw_rct_df <- read.csv(paste0(ipmh_filepath, "/Data/6. RCT PPW data/RCT_PPW_", 
                              file_date, ".csv", sep="")) %>% 
    filter(is.na(redcap_repeat_instance))

daily_closeout_df <- read.csv(paste0(ipmh_filepath, "/Data/7. RCT admin data/Daily_closeout_", 
                                     file_date, ".csv", sep=""))

screening_consent_df <- read.csv(paste0(ipmh_filepath, 
                                        "/Data/2. Consenting database/RCT_PPW_consenting_", 
                                        file_date, ".csv", sep=""))

pm_survey_df <- read.csv(paste0(ipmh_filepath, "/Data/7. RCT admin data/PM_", 
                                file_date, ".csv", sep="" ))

ppw_sae_df <- read.csv(paste0(ipmh_filepath, "/Data/6. RCT PPW data/RCT_PPW_", 
                                             file_date, ".csv", sep="")) %>% 
    filter(redcap_repeat_instrument == "ADMIN: Adverse Experience")



