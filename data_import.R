# Header ------------------------------------------------------------------

# Author(s): David
# Date: March 10, 2025
# Script to facilitate import of data from sharepoint

# Setup ------------------------------------------------------------------------
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("REDCap_datapull.R")

if (exists("data_freeze")) {
    latest <- fileSnapshot(file.path(ipmh_filepath, "/Data/6. RCT PPW data/"))
    latest <- rownames(latest$info[which.max(latest$info$mtime),])
    latest <- as.Date(gsub(".*RCT_PPW_(.+)\\.csv", "\\1", latest))  # extracts YYYY-MM-DD
    
    file_date <- format(min(as.Date(data_freeze), latest), "%Y-%m-%d")  # keep YYYY-MM-DD
} else {
    latest <- fileSnapshot(file.path(ipmh_filepath, "/Data/6. RCT PPW data/"))
    latest <- rownames(latest$info[which.max(latest$info$mtime),])
    file_date <- gsub(".*RCT_PPW_(.+)\\.csv", "\\1", latest)  # YYYY-MM-DD string
}

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



