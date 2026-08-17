# Header ------------------------------------------------------------------
#
# Author:       Dowaga
# Date:         August 17, 2026
# Description: 

#The purpose of this script is to collate all of the data cleaning scripts and
#save the resulting clean datasets to the study team drive. This script should
#be run weekly after pulling raw data and before running reports. For additional,
#information, please refer to the Data Cleaning SOP.


# Setup ------------------------------------------------------------------------
source("DataTeam_ipmh.R")
exists("ipmh_filepath")

# If data_freeze isn't set, use most recent data pull
data_freeze <- if (exists("data_freeze") && !is.null(data_freeze)) {
   data_freeze
} else {
   list.files(
      path = file.path(ipmh_filepath, "Data/6. RCT PPW data"),
      pattern = "*.csv"
   ) %>%
      sort() %>%
      tail(1) %>%
      stringr::str_sub(11, 18)
}

# Make sure Clean DataSets exists
if(!dir.exists(file.path(ipmh_filepath, "Data/Clean DataSets", data_freeze))){
   dir.create(file.path(ipmh_filepath, "Data/Clean DataSets", data_freeze))
}

# screening and enrollment Data -------------------------------------------------------------------
source("screening_enrollment_cleaning.R")

# Save into Clean DataSets with data_freeze in the filename
write_csv(
   screening_clean_df,
   file.path(
      ipmh_filepath,
      "Data/Clean DataSets",
      paste0("screening_enrollment_data_", data_freeze, ".csv")
   )
)
