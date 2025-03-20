# Header ------------------------------------------------------------------

# Author(s): Owaga
# Date: March 10, 2025
# Consort Diagram for weekly report

# Setup ------------------------------------------------------------------------

# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")

Attendees <- daily_closeout_df %>% 
    summarise(`ANC Attendees` = sum(rct_anc_number))

pm_abstractions <- pm_survey_df %>% 
    select(record_id, ipmh_participant) %>% 
    filter(ipmh_participant == "Yes")

consort_data <- screening_consent_df %>% 
    select(record_id, rct_harm_thought, rct_aud_hallucinations, 
           rct_vis_hallucinations, rct_paranoia,rct_delusions,
           rct_memory_problem,rct_eligible_gestation, rct_eligible_harm, 
           rct_eligible, rct_enrolling, rct_decline_reason, 
           rct_other_reasons) %>% 
    mutate(
        eligible = case_when(
            rct_eligible == 1 ~ "1",
            TRUE ~ NA_character_)) %>% 
    mutate(exclusion = case_when(
        rct_eligible == 0 & rct_eligible_gestation == "No" ~ "Gestation <28 Weeks",
        rct_harm_thought == "Yes" & rct_memory_problem == "No" ~"Self harm",
        rct_harm_thought == "Yes" & rct_memory_problem == "Yes" ~"Self harm and memory problem",
        rct_eligible == 0 & rct_aud_hallucinations == "Yes" ~ "Hearing voices that others cannot hear",
        rct_memory_problem == "Yes" ~"Memory problem",
        rct_delusions == "Yes" ~ "Holding unusual beliefs",
        TRUE ~ NA_character_)) %>% 
    mutate(rct_decline_reason = case_when(
            rct_decline_reason == "Other (specify) ___" ~ "Relocate post delivery",
            TRUE ~ rct_decline_reason  # Keep other values unchanged
        ))

elig <- consort_data %>% 
    filter(is.na(eligible)) %>% 
    filter(is.na(exclusion))

decline_reason <- consort_data %>% 
    filter(!is.na(rct_decline_reason))

consort_diagram <- consort_plot(data = consort_data,
                    orders = c(record_id = "Assessed for Eligibility ",
                               exclusion = "Excluded",
                               eligible = "Eligible",
                               rct_decline_reason = "Declined Enrollment",
                               rct_enrolling = "Enrolled"),
                    side_box = c("exclusion", "rct_decline_reason"),
                    cex = 0.9)



