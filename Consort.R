pacman::p_load(consort)

Attendees <- daily_closeout_df %>% 
    summarise(`ANC Attendees` = sum(rct_anc_number))



pm_refers <- pm_survey_df %>% 
    select(record_id, ipmh_participant) %>% 
    filter(ipmh_participant == "Yes")

consort_data <- screening_consent_df %>% 
    select(record_id, rct_harm_thought, rct_aud_hallucinations,
           rct_memory_problem,rct_eligible_gestation, rct_eligible_harm, 
           rct_eligible, rct_enrolling, rct_decline_reason) %>%
    mutate(
        eligible = case_when(
            rct_eligible == 1 ~ "1",
            TRUE ~ NA_character_)) %>% 
    mutate(exclusion = case_when(
        rct_eligible == 0 & rct_eligible_gestation == "No" ~ "Gestation <28 Weeks",
        rct_harm_thought == "Yes" & rct_memory_problem == "No" ~"Self harm",
        rct_harm_thought == "Yes" & rct_memory_problem == "Yes" ~"Self harm and memory problem",
        rct_eligible == 0 & rct_aud_hallucinations == "Yes" ~ "Hallucinations",
        TRUE ~ NA_character_)) 
        

consort_diagram <- consort_plot(data = consort_data,
                    orders = c(record_id = "Assessed for Eligibility ",
                               exclusion = "Excluded",
                               eligible = "Eligible",
                               rct_decline_reason = "Declined Enrollment",
                               rct_enrolling = "Enrolled"),
                    side_box = c("exclusion", "rct_decline_reason"),
                    cex = 0.9)



