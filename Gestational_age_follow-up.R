# Header ------------------------------------------------------------------

# Author(s): Dowaga
# Date: August 6, 2025
# This is a script that analyzes the PPW without Gestational age at 6 Weeks visit

# Setup ------------------------------------------------------------------------
rm(list = ls())     

# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")

# data prep --------------------------------------------------------------------
six_weeks <- ppw_rct_df %>% 
    filter(clt_visit == "6 weeks post-partum") %>% 
    select(clt_ptid, tpnc_ended, tpnc_lb, tpnc_gestage, tpnc_gestfill, mv_visit,
           mv_reason, admin_missed_visit_complete) %>% 
    mutate(missed = case_when(
        admin_missed_visit_complete == "Complete" & mv_visit == "6 weeks post-partum" ~ "6 weeks",
        admin_missed_visit_complete == "Complete" & mv_visit == "14 weeks post-partum" ~ "14 weeks",
        admin_missed_visit_complete == "Complete" & mv_visit == "6 months post-partum" ~ "6 months",
        TRUE ~ NA_character_
    ))

missed_week6 <- six_weeks %>% 
    filter(mv_visit == "6 weeks post-partum"|missed == "6 weeks")

# Missing GAs
gest_age_qc <- six_weeks %>% 
    filter(!missed %in% c("6 weeks")) %>% 
    filter(is.na(tpnc_gestfill))
