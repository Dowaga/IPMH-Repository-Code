# Header ------------------------------------------------------------------

# Author(s): Owaga
# Date: March 10, 2025
# Consort Diagram for weekly report

# Setup ------------------------------------------------------------------------

# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")

pm_follow_up <- pm_survey_df %>% 
    select(pm_ptid, pm_facility, pm_date, pm_session) %>%
    right_join(pm_telep_df %>% 
                   select(record_id, clt_date), by = c("pm_ptid" = "record_id")) %>% 
    select(pm_ptid, clt_date, pm_date, pm_session) %>% 
    filter(!is.na(pm_date)) %>% 
    arrange(pm_ptid)

# Sample data: Ensure dates are in Date format
pm_follow_up <- pm_follow_up %>%
    mutate(clt_date = as.Date(clt_date),
           pm_date = as.Date(pm_date))  

# Define expected visit date (one week after enrollment)
pm_follow_up <- pm_follow_up %>%
    mutate(session1_date = clt_date + weeks(1),
           `Session 1` = case_when(
               !is.na(pm_date) ~ "Attended",  # If visit_date is available
               Sys.Date() >= expected_visit_date & is.na(pm_date) ~ "Missed",  # If due but not attended
               Sys.Date() < expected_visit_date & is.na(pm_date) ~ "Due"  # If upcoming but not attended
           )) %>% 
    mutate(session2_date = clt_date + weeks(2),
           `Session 2` = case_when(
               !is.na(pm_date) ~ "Attended",  # If visit_date is available
               Sys.Date() >= session2_date & is.na(pm_date) ~ "Missed",  # If due but not attended
               Sys.Date() < session2_date & is.na(pm_date) ~ "Due"  # If upcoming but not attended
           ))
