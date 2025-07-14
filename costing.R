# Header ------------------------------------------------------------------

# Author(s): Yuwei
# Date: July 14, 2025
# This is a script that analyzes the costing data.

# Setup ------------------------------------------------------------------------
# Reference source codes & other dependencies:
source("REDCap_datapull.R")
source("dependencies.R")

# load data
costing <- read_csv("IPMHCostingSurvey_DATA_2025-07-14_2140.csv")

# screening & enrollment - int ------------
screening_int_costing <- costing %>% 
    filter(study_visit == 1 & 
               redcap_event_name == "event_1_arm_1") %>% 
    select(study_site, pt_id:screening_enrollment_int_complete) %>% 
    filter(!is.na(pt_id))



# screening & enrollment - con ------------

# PM+ ---------------

# telepsychiatry ------------------