# Header ------------------------------------------------------------------

# Author(s): Owaga
# Date: May 28, 2025
# Expected Date of Delivery

# Setup ------------------------------------------------------------------------
rm(list = ls())
# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")

#----------------------------------------------------------------

edd <- ppw_rct_df %>%
    filter(clt_visit == "Enrollment") %>%
    select(record_id, clt_study_site, clt_date, med_pre_gestage_current, med_pre_edd) %>%
    mutate(clt_date = ymd(clt_date),  # Convert to Date format
           calculated_edd = clt_date + weeks(40 - med_pre_gestage_current))


####
xy <- edd %>% 
    filter(is.na(med_pre_edd))

# Already expected to have delivered
expected_deiveries <- edd %>% 
    filter(calculated_edd < Sys.Date())  # Keep only past EDDs


total_expe_deliveries <- expected_deiveries %>% 
    group_by(clt_study_site) %>% 
    reframe(Total = n())
