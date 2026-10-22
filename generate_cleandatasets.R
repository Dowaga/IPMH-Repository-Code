# Header ------------------------------------------------------------------
#
# Author:       Dowaga
# Date:         August 17, 2026
# Description: 

#The purpose of this script is to collate all of the data cleaning scripts and
#save the resulting clean datasets to the study team drive. This script should
#be run weekly after pulling raw data and before running reports. For additional,
#information, please refer to the Data Cleaning SOP.


# ------------------------------------------------------------------------
# Setup: Load shared environment and check required paths
# ------------------------------------------------------------------------
source("DataTeam_ipmh.R", local = .GlobalEnv)   # Load project environment variables
stopifnot(exists("ipmh_filepath"))              # Ensure ipmh_filepath is defined

# ------------------------------------------------------------------------
# Define data_freeze (snapshot date) if not already set
# ------------------------------------------------------------------------
if (!exists("data_freeze", envir = .GlobalEnv, inherits = FALSE) ||
    is.null(data_freeze)) {
    
    # List all CSV files in the RCT PPW data directory
    csv_files <- list.files(
        file.path(ipmh_filepath, "Data/6. RCT PPW data"),
        pattern = "\\.csv$"
    )
    
    # Stop execution if no CSV files are found
    if (length(csv_files) == 0) {
        stop("No CSV files found in the RCT PPW data directory.")
    }
    
    # Use the most recent file (sorted descending) and extract date substring
    data_freeze <- stringr::str_sub(
        sort(csv_files, decreasing = TRUE)[1],
        11, 18
    )
}

# ------------------------------------------------------------------------
# Create output directory for cleaned datasets
# ------------------------------------------------------------------------
clean_dir <- file.path(
    ipmh_filepath,
    "Data/Clean DataSets",
    format(data_freeze, "%Y-%m-%d")   # Ensure consistent date format
)

# Create directory if it doesn't exist (no warnings if already present)
dir.create(clean_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(exists("data_freeze", envir = .GlobalEnv))   # Confirm data_freeze is set

# ------------------------------------------------------------------------
# Run cleaning scripts and save outputs
# ------------------------------------------------------------------------

# 1. Screening & Enrollment
source("screening_enrollment_cleaning.R", local = .GlobalEnv)
write_csv(
    screening_clean_df,
    file.path(
        clean_dir,
        paste0("screening_enrollment_data_", 
               format(data_freeze, "%Y-%m-%d"), ".csv")
    )
)

# 2. Demographics
source("Demographics cleaning.R", local = .GlobalEnv)
write_csv(
    demographic_clean_df,
    file.path(
        clean_dir,
        paste0("demographic_data_", 
               format(data_freeze, "%Y-%m-%d"), ".csv")
    )
)

# 3. Medical & Obstetric History
source("Medical and Obstetric H_variables_cleaning.R", local = .GlobalEnv)
write_csv(
    medical_clean_df,
    file.path(
        clean_dir,
        paste0("Medical_Obstetric_History_data_", 
               format(data_freeze, "%Y-%m-%d"), ".csv")
    )
)


