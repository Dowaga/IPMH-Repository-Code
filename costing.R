# Header ------------------------------------------------------------------

# Author(s): Yuwei
# Date: July 14, 2025
# This is a script that analyzes the costing data.

# Setup ------------------------------------------------------------------------
# Reference source codes & other dependencies:
source("dependencies.R")
source("DataTeam_ipmh.R")
source("REDCap_datapull.R")

# load data
rm(list = setdiff(ls(), c("costing")))

# screening & enrollment - int ------------
screening_int_costing <- costing %>% 
    filter(redcap_event_name == "Event 1 (Arm 1: Intervention)" & 
               study_visit == "Initial screening and enrollment") %>% 
    filter(!is.na(pt_id)) %>% 
    select(date, study_site, pt_id, enter_desig, enter_time_in,
           triage_desig, triage_time_in, triage_time_out,
           screen_desig, screen_time_in, screen_time_out,
           room_type, room_desig, phq2, gad2, score_time_in,
           score_time_out, preg, info_time_in, info_time_out,
           phq9, gad7, phq9_time_in, phq9_time_out,
           refer_service, refer_time_in, pm_desig, pmass_time_in,
           pmass_time_out, telesch_time_in, telesch_time_out,
           harm_time_in, harm_time_out, eli_yn, eligi_time_in,
           eligi_time_out, enrol_yn, base_time_in,
           base_time_out, visit_time_out)




# screening & enrollment - con ------------
screening_ctrl_costing <- costing %>%
    filter(redcap_event_name == "Event 1 (Arm 2: Control)" & 
               study_visit == "Initial screening and enrollment") %>%
    filter(!is.na(pt_id_con)) %>%
    select(date, study_site, c(96, 98:122), -visit_initial_con)

# PM+ ---------------
pm_plus_costing <- costing %>%
    filter(redcap_event_name == "Event 1 (Arm 1: Intervention)" & 
               study_visit == "PM+") %>%
    filter(!is.na(pt_id_pm)) %>%
    select(date, study_site, starts_with("pm"), -pm_desig, -pmass_time_in,
           -pmass_time_out, -pmsch_time_in, -pmsch_time_out, -pm_int_complete)


# telepsychiatry ------------------
telepsychiatry_costing <- costing %>%
    filter(redcap_event_name == "Event 1 (Arm 1: Intervention)" & 
               study_visit == "Telepsychiatry session") %>%
    filter(!is.na(pt_id_tele)) %>%
    select(date, study_site, starts_with("tele"), -telesch_time_in, -telesch_time_out,
           -tele_initial, -tele_end_initial, -telepsychiatry_int_complete)

# Audit and Feedback ------------------
audit_feedback_costing <- costing %>%
    filter(study_visit == "Audit and Feedback") 

