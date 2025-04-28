# Header ------------------------------------------------------------------

# Author(s): Owaga & Yuwei
# Date: Apr 10, 2025
# Consort Diagram for weekly report

# Setup ------------------------------------------------------------------------

# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")

# Summarise ANC attendees and extract the sum as a numeric value
Attendees <- daily_closeout_df %>%
    summarise(`anc attendees` = sum(rct_anc_number, na.rm = TRUE)) %>%
    pull(`anc attendees`)  # Extract the sum value as a number

# Create a new data frame with anc_count rows, value = "Yes"
anc_attendees_df <- data.frame(
    anc_id = paste0("anc_", seq_len(Attendees)),
    anc_attendees = rep("Yes", Attendees)
)

#Study participants in the PM+ Survey
pm_abstractions <- pm_survey_df %>%
    mutate(pm_ptid = as.numeric(pm_ptid)) %>%       # if needed
    filter(ipmh_participant == "Yes") %>%
    distinct(pm_ptid, .keep_all = TRUE)

pm_df <- pm_abstractions %>% 
    select(pm_ptid, ipmh_participant)



consort_data <- screening_consent_df %>% 
    select(record_id, partipant_id, rct_harm_thought, rct_aud_hallucinations, 
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
    mutate(facility = as.numeric(str_sub(partipant_id, 3, 4))) %>% 
    mutate(arm = case_when(
        facility %in% c(2, 5, 6, 8, 11, 14, 15, 18, 20, 21) ~ "Control",
        TRUE ~ "Intervention"
    )) %>% 
    mutate(rct_decline_reason = case_when(
            rct_decline_reason == "Other (specify) ___" ~ "Relocate post delivery",
            TRUE ~ rct_decline_reason  # Keep other values unchanged
        )) 

# Merge consort data with pm_df
consort_data <- consort_data %>% 
    left_join(pm_df, by = c("partipant_id"="pm_ptid"))


consort_data <- bind_rows(anc_attendees_df, consort_data)

elig <- consort_data %>% 
    filter(is.na(eligible)) %>% 
    filter(is.na(exclusion))

decline_reason <- consort_data %>% 
    filter(!is.na(rct_decline_reason))

secondvisit <- ppw_rct_df %>% filter(
    clt_visit == "6 weeks post-partum") %>% 
    select(clt_ptid) %>% mutate(secondvisit = "Yes") 

#merge secondvisit people into consort_data
consort_data <- consort_data %>% 
    left_join(secondvisit, by = c("partipant_id" = "clt_ptid")) 
    
consort_diagram <- consort_plot(data = consort_data,
                    orders = c(anc_attendees = "ANC Attendees",
                               arm = "Assessed for Eligibility",
                        #record_id = "Assessed for Eligibility",
                               exclusion = "Excluded",
                               eligible = "Eligible",
                               rct_decline_reason = "Declined Enrollment",
                               rct_enrolling = "Enrolled",
                        ipmh_participant = "PM+",
                        secondvisit = "6 weeks postpartum visit"),
                    side_box = c("exclusion", "rct_decline_reason"),
                    allocation = "arm")
consort_diagram

library(grid)
options(txt_gp = gpar(cex = 0.8)) 
txt_anc <- c("ANC Attendees (n=4,089)")
txt_ass <- c("Assessed for Eligibility\n (n=1,018, 24.90%)") 
txt_arm <- c("Control\n (n=527, 51.77%)", "Intervention\n (n=491, 48.23%)")
txt_ex_control <- "Excluded (n=194, 36.81%):\n\u2022 Gestation < 28 weeks (n=177, 91.24%)\n\u2022 Hearing voices that others cannot hear (n=3, 1.55%)\n\u2022 Holding unusual beliefs (n=4, 2.06%)\n\u2022 Memory problem (n=3, 1.55%)\n\u2022 Self harm (n=5, 2.58%)\n\u2022 Self harm and memory problem (n=1, 0.52%)"
txt_ex_intervention <- "Excluded (n=203, 41.34%):\n\u2022 Gestation < 28 weeks (n=198, 97.54%)\n\u2022 Memory problem (n=1, 0.49%)\n\u2022 Self harm (n=4, 1.97%)"
txt_ex <- c(txt_ex_control, txt_ex_intervention)
txt_eli <- c("Eligible\n (n=332, 63.00%)", "Eligible\n (n=288, 58.66%)")
txt_decline_control <- "Declined Enrollment (n=17, 5.12%)\n\u2022 Consult spouse (n=2, 11.76%)\n\u2022 Not enough time (n=3, 17.65%)\n\u2022 Not interested (n=3, 17.65%)\n\u2022 Relocate post delivery (n=6, 35.29%)\n\u2022 Time to think about it (n=3, 17.65%)"
txt_decline_intervention <- "Declined Enrollment (n=12, 4.17%)\n\u2022 Not enough time (n=5, 41.67%)\n\u2022 Relocate post delivery (n=7, 58.33%)"
txt_decline <- c(txt_decline_control, txt_decline_intervention)
txt_enrol <- c("Enrolled (n=315, 94.88%)", 
               "Enrolled (n=276, 95.83%)")
txt_pm <- c("PM+\n (n=0, 0%)", 
            "PM+\n (n=65, 23.55%)")
txt_secondvisit <- c("6 weeks postpartum visit\n (n=14, 4.44%)", 
                      "6 weeks postpartum visit\n (n=10, 3.62%)")

consort_per <- add_box(txt = txt_anc) |>
    add_box(txt = txt_ass) |>
    add_split(txt = txt_arm) |>
    add_side_box(txt = txt_ex) |>
    add_box(txt = txt_eli) |>
    add_side_box(txt = txt_decline) |>
    add_box(txt = txt_enrol) |>
    add_box(txt= txt_pm) |>
    add_box(txt = txt_secondvisit)

consort_per


