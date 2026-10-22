# Header ------------------------------------------------------------------

# Author(s): David
# Date: March 10, 2025
# Script to facilitate import of data from sharepoint

# Setup ------------------------------------------------------------------------
source("DataTeam_ipmh.R")
source("Dependencies.R")
#source("REDCap_datapull.R")
#data_freeze <- as.Date("2026-04-30") 

# Data freeze and CSV selection -----------------------------------------------

# Define raw data directory
raw_dir <- file.path(
    ipmh_filepath,
    "Data/6. RCT PPW data"
)

# Identify relevant CSV files
csv_files <- list.files(
    path = raw_dir,
    pattern = "^RCT_PPW_\\d{4}-\\d{2}-\\d{2}\\.csv$",
    full.names = FALSE
)

# Stop if no matching files are available
if (length(csv_files) == 0) {
    stop("No matching RCT_PPW CSV files found in: ", raw_dir)
}

# Extract dates from filenames
file_dates <- as.Date(
    stringr::str_extract(
        csv_files,
        "\\d{4}-\\d{2}-\\d{2}"
    ),
    format = "%Y-%m-%d"
)

# Create lookup table and remove invalid dates
df <- data.frame(
    file = csv_files,
    date = file_dates,
    stringsAsFactors = FALSE
) %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::arrange(date)

if (nrow(df) == 0) {
    stop("No valid dates could be extracted from the CSV filenames.")
}

# Define data_freeze if not already provided
# Default: latest available CSV date
if (!exists("data_freeze", envir = .GlobalEnv, inherits = FALSE) ||
    is.null(data_freeze)) {
    
    data_freeze <- max(df$date)
}

# Standardise data_freeze to Date format
if (!inherits(data_freeze, "Date")) {
    
    freeze_value <- as.character(data_freeze)
    
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", freeze_value)) {
        
        data_freeze <- as.Date(freeze_value, format = "%Y-%m-%d")
        
    } else if (grepl("^\\d{8}$", freeze_value)) {
        
        data_freeze <- as.Date(freeze_value, format = "%Y%m%d")
        
    } else if (grepl("^\\d{2}-\\d{2}-\\d{2}$", freeze_value)) {
        
        # Interpret YY-MM-DD as 20YY-MM-DD
        data_freeze <- as.Date(
            paste0("20", freeze_value),
            format = "%Y-%m-%d"
        )
        
    } else {
        stop("Unexpected data_freeze format: ", freeze_value)
    }
}

# Validate freeze date
if (is.na(data_freeze)) {
    stop("data_freeze could not be converted to a valid date.")
}

# Select files on or before the freeze date
eligible_files <- df %>%
    dplyr::filter(date <= data_freeze)

if (nrow(eligible_files) == 0) {
    stop(
        "No CSV files found on or before data_freeze: ",
        format(data_freeze, "%Y-%m-%d")
    )
}

# Select the most recent eligible file
selected <- eligible_files %>%
    dplyr::slice_max(order_by = date, n = 1, with_ties = FALSE)

selected_file <- selected$file[[1]]
file_date <- format(selected$date[[1]], "%Y-%m-%d")

# Report selection
message("Data freeze date: ", format(data_freeze, "%Y-%m-%d"))
message("Selected CSV file: ", selected_file)
message("Selected file date: ", file_date)

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

screening_consent_hcw_df <- read.csv(paste0(ipmh_filepath, 
                                            "/Data/2. Consenting database/RCT_HCW_consenting_", 
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

costing_df <- read.csv(paste0(ipmh_filepath, "/Data/9. Costing data/Costing_", 
                                file_date, ".csv", sep=""))

phq2_gad2_abstract <- read.csv(paste0(ipmh_filepath, "/Data/7. RCT admin data/PHQ2_GAD2_abstract_", 
                              file_date, ".csv", sep=""))

aim1qual_demo_df  <- read.csv(paste0(ipmh_filepath, "/Data/3. Aim 1 qualitative demo/Aim1_qual_demo_", 
                                  file_date, ".csv", sep=""))

aim1quant_df <- read.csv(paste0(ipmh_filepath, "/Data/4. Aim 1 quantitative data/Aim1_quant_data_", 
                                  file_date, ".csv", sep=""))

hcw_is_df <- read.csv(paste0(ipmh_filepath, "/Data/5. RCT HCW data/RCT_HCW_", 
                             file_date, ".csv", sep=""))

