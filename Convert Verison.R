#______________________________________________________________

# This scrip is used to analyse the consent version 
# participants were consented with for reconsenting purposes

# Load Packages---------------------------------------
rm(list = ls())
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")

#----------------------------------------------------------------
consent_df <- screening_consent_df %>% 
    filter(ipmh_rct_enrollment_consent_complete == "Complete")

consent_version <-consent_df %>% 
    select(partipant_id, study_site, consent_date)%>%
    mutate(
        `Consent Date` = ymd(consent_date),  # Ensure it's a Date
        `Consent Version` = case_when(
            consent_date < ymd("2025-06-16") ~ "v4.0",
            consent_date >= ymd("2025-06-16") & consent_date <= ymd("2025-08-08") ~ "v5.0",
            consent_date > ymd("2025-08-08") ~ "v6.0",
            TRUE ~ NA_character_
        ),
        Reconsented = "",
        `Reconsenting Date` = as.Date(NA),  # Blank date column
        `Staff Name` = "",              # Blank column
        Notes = ""                    # Blank column
    ) %>% 
    mutate(study_site = gsub("^\\d+,\\s*(\\w+).*", "\\1", study_site))

# Split the data by site
consent_split <- split(consent_version, consent_version$study_site)

# Drop the study_site column from each split
consent_split <- lapply(consent_split, function(df) df %>% 
                            select(c(-study_site, consent_date))

# Create a workbook and add a worksheet for each site
wb <- createWorkbook()

for (site_name in names(consent_split)) {
    addWorksheet(wb, site_name)
    writeData(wb, site_name, consent_split[[site_name]])
}


# Save workbook
saveWorkbook(wb, "Consent tracking by Site.xlsx", overwrite = TRUE)
    
