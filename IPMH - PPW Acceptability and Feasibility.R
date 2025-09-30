# Header ------------------------------------------------------------------

# Author(s): Dowaga
# Date: September 30, 2025
# Description: This script is used to generate data reports for IPMH Aceptability, Adaptability and Feasibility of IPMH activities

# Setup ------------------------------------------------------------------------
rm(list = ls())
# Reference source codes & other dependencies: Use this section to reference other scripts and dependencies
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("REDCap_datapull.R")

# Pull the required data
accep_dt <- ppw_rct_df %>%
    mutate(
        visit_type = case_when(
            grepl("Enrollment", redcap_event_name) ~ "Enrollment",
            grepl("6 Weeks", redcap_event_name) ~ "6 Weeks",
            grepl("14 Weeks", redcap_event_name) ~ "14 Weeks", 
            grepl("6 Months", redcap_event_name) ~ "6 Months",
            TRUE ~ NA_character_
        ),
        arm = case_when(
            grepl("Arm 1: Intervention", redcap_event_name) ~ "Intervention",
            grepl("Arm 2: Control", redcap_event_name) ~ "Control",
            TRUE ~ "Unknown"
        )
    ) %>% 
    select(clt_ptid, clt_visit, starts_with("af_"), visit_type) %>% 
    filter(visit_type %in% c("6 Weeks", "14 Weeks", "6 Months"))

# Define level recoding
accep_labels <- c(
    "Completely agree" = 5,
    "Agree" = 4,
    "Neither agree not disagree" = 3,
    "Disagree" = 2,
    "Completely disagree" = 1
)

# Recode PHQ9 variables
accep_dt <- accep_dt %>%
    mutate(across(c(af_meets, af_appeal, af_enjoy, af_welcome, af_use, 
                    af_seems, af_doable, af_easy, af_life, af_suitable, 
                    af_applicable, af_match), 
                  ~ recode(., !!!accep_labels)))







long_data <- accep_dt %>%
    pivot_longer(cols = -c(clt_ptid, clt_visit), names_to = "item", values_to = "response")






