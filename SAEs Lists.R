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
    select(record_id, redcap_repeat_instance, starts_with("ae_")) %>% 
    filter(ae_yn == "Yes" & !str_detect(ae_cat, "SAE"))

sae_df <- ppw_sae_df %>% 
    select(record_id, redcap_repeat_instance, starts_with("ae_")) %>% 
    filter(ae_yn == "Yes" & str_detect(ae_cat, "SAE"))

# SAEs--------------------------------------------------------------------------
sae_report <- sae_df %>% 
    group_by(ae_cat) %>% 
    mutate(ae_cat = if_else(record_id == "21031003" & ae_cat == "New/prolonged hospitalization (SAE)",
                            "Death (Infant or Maternal) (SAE)",
                            ae_cat)) %>% 
    rename(Event = ae_cat) %>% 
    summarise(`Number of Events` = n(),
              `SAE Reported` = n(),
              `% Reported` = ifelse(`Number of Events` > 0, round((`Number of Events` / `SAE Reported`)) * 100, 1)) %>% 
    gt()

sae_report

ae_report <- ae_df %>% 
    group_by(ae_cat) %>% 
    mutate(ae_cat = if_else(record_id == "21031003" & ae_cat == "New/prolonged hospitalization (SAE)",
                            "Death (Infant or Maternal) (SAE)",
                            ae_cat)) %>% 
    rename(Event = ae_cat) %>% 
    summarise(`Number of Events` = n(),
              `SAE Reported` = n(),
              `% Reported` = ifelse(`Number of Events` > 0, round((`Number of Events` / `SAE Reported`)) * 100, 1)) %>% 
    gt()

ae_report
