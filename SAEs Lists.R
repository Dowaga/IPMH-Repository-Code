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
