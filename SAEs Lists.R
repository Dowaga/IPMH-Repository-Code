# Header ------------------------------------------------------------------

# Author(s): Owaga
# Date: March 11, 2025
# AEs Lists

# Setup ------------------------------------------------------------------------
rm(list = ls())
# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")

ae_df <- ppw_sae_df %>% 
    select(record_id, redcap_repeat_instance, starts_with("ae_"), 
           redcap_event_name, redcap_repeat_instance) %>% 
    filter(ae_yn == "Yes" & !str_detect(ae_cat, "SAE"))



sae_df <- ppw_sae_df %>% 
    select(record_id, redcap_repeat_instance, starts_with("ae_"), redcap_event_name) %>% 
    filter(ae_yn == "Yes" & str_detect(ae_cat, "SAE"))

# SAEs Report-------------------------------------------------------------------
clean_sae_df <- sae_df %>% 
    group_by(ae_cat) %>% 
    mutate(ae_cat = if_else(record_id == "21031003" & ae_cat == "New/prolonged hospitalization (SAE)",
                            "Death (Infant or Maternal) (SAE)",
                            ae_cat)) %>% 
    rename(Event = ae_cat) 

sae_report <- clean_sae_df %>% 
    summarise(`Number of Events` = n(),
              `SAE Reported` = n(),
              `% Reported` = ifelse(`Number of Events` > 0, round((`Number of Events` / `SAE Reported`)) * 100, 1)) %>%
    gt()

sae_report

# AE Report---------------------------------------------------------------------
ae_report <- ae_df %>% 
    filter(redcap_repeat_instance == 2|!str_detect(redcap_event_name, "Enrollment")) %>% 
    group_by(ae_cat) %>% 
    mutate(ae_cat = if_else(record_id == "21031003" & ae_cat == "New/prolonged hospitalization (SAE)",
                            "Death (Infant or Maternal) (SAE)",ae_cat)) %>% 
    rename(Event = ae_cat) %>% 
    summarise(`Number of Events` = n()) %>% 
    gt()

ae_report


#### SAEs by ARM
clean_sae_df <- clean_sae_df %>%
    mutate(Arm = case_when(
        grepl("Arm 2: Control", redcap_event_name) ~ "Control",
        grepl("Arm 1: Intervention", redcap_event_name) ~ "Intervention",
        TRUE ~ NA_character_
    ))

# Step 2: Create a binary variable for each Event category

sae_wide <- clean_sae_df %>%
    mutate(event_flag = 1) %>%  # to allow counting
    select(record_id, Arm, Event, event_flag) %>%
    tidyr::pivot_wider(names_from = Event, values_from = event_flag, values_fill = 0) %>% 
    select(-record_id)

# Step 3: Summarize using tbl_summary by arm
sae_summary <- tbl_summary(
    data = sae_wide,
    by = Arm,
    type = all_continuous() ~ "continuous",
    statistic = all_continuous() ~ "{sum}",
    missing = "no"
) %>%
    modify_header(label = "**SAE Category**") %>%
    modify_caption("**Summary of Serious Adverse Events by Study Arm**")


sae_summary 

# Summary of Adverse events-----------------------------------------------------
# Select age and site from main df
ae_ages <- ppw_rct_df %>% 
    mutate(dem_age = if_else(
        dem_dob_uk == "Yes",
        floor(time_length(interval(dem_dob, clt_date), "years")),
        dem_age
    )) %>% 
    select(record_id, clt_study_site,redcap_event_name, dem_age)

# left joing with aes df
ae_summary <- ae_df %>% 
    select(record_id, ae_datereport,ae_dateonset , ae_cat, ae_resolutiondate) %>% 
    left_join(ae_ages, by = "record_id") %>% 
    filter(!is.na(dem_age)) %>% 
    select(record_id, clt_study_site, dem_age,ae_datereport,ae_cat,ae_dateonset, 
           ae_resolutiondate) %>% 
    # Remove the facility code
    mutate(clt_study_site = gsub("^[0-9]+,\\s*", "", clt_study_site)) %>% 
    rename(PTID = record_id,
           `Study Site` = "clt_study_site",
           `Date Averse Event was reported` = ae_datereport,
           `Adverse Event reported` = ae_cat, 
           `Start date of adverse event` = ae_dateonset,
           `End date of adverse event` = ae_resolutiondate,
           `Age of participant` = dem_age)

ae_summary <- ae_summary %>% 
    gt()

# Save the table to a Word document
gtsave(ae_summary, filename = "Summary of Adverse Events.docx") 
