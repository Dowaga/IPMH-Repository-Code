# Header ------------------------------------------------------------------

# Author(s): David
# Date: March 10, 2025
# Script to facilitate import of data from sharepoint

# Setup ------------------------------------------------------------------------
source("DataTeam_ipmh.R")
source("Dependencies.R")
#source("REDCap_datapull.R")
#data_freeze <- as.Date("2026-04-30") 

all_files <- fileSnapshot(file.path(ipmh_filepath, "/Data/6. RCT PPW data/"))$info

# Keep only relevant CSV files
csv_files <- rownames(all_files)[
    grepl("RCT_PPW_\\d{4}-\\d{2}-\\d{2}\\.csv$", rownames(all_files))
]

# Extract dates from filenames
file_dates <- as.Date(
    gsub(".*RCT_PPW_(\\d{4}-\\d{2}-\\d{2})\\.csv", "\\1", csv_files),
    format = "%Y-%m-%d"
)

# Create a lookup table
df <- data.frame(file = csv_files, date = file_dates)

if (exists("data_freeze")) {
    
    # Keep only files on/before freeze date
    df <- df[df$date <= data_freeze, ]
    
    if (nrow(df) == 0) stop("No CSV files found on or before data_freeze")
    
    # Pick closest date to freeze
    selected <- df[which.max(df$date), ]
    
    file_date <- format(selected$date, "%Y-%m-%d")
    selected_file <- selected$file
    
} else {
    
    # If no freeze, just pick latest by date (not mtime)
    selected <- df[which.max(df$date), ]
    
    file_date <- format(selected$date, "%Y-%m-%d")
    selected_file <- selected$file
}

rct_ppw_consenting <- read.csv(paste0(ipmh_filepath, "/Data/2. Consenting database/RCT_PPW_consenting_", 
                                      file_date, ".csv", sep="")) %>% 
    filter(rct_enrolling == "Yes")

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

telepsych <- read.csv(paste0(ipmh_filepath, "/Data/7. RCT admin data/telepsych_", 
                              file_date, ".csv", sep=""))

pm_survey_df <- read.csv(paste0(ipmh_filepath, "/Data/7. RCT admin data/PM_", 
                               file_date, ".csv", sep=""))

