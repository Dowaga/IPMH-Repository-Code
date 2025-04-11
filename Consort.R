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


consort_diagram <- consort_plot(data = consort_data,
                    orders = c(anc_attendees = "ANC Attendees",
                               arm = "Assessed for Eligibility",
                        #record_id = "Assessed for Eligibility",
                               exclusion = "Excluded",
                               eligible = "Eligible",
                               rct_decline_reason = "Declined Enrollment",
                               rct_enrolling = "Enrolled",
                        ipmh_participant = "Study Nurse PM+ Yields"),
                    side_box = c("exclusion", "rct_decline_reason"),
                    allocation = "arm")

library(grid)
options(txt_gp = gpar(cex = 0.8)) 
txt_anc <- c("ANC Attendees (n=3,383)")
txt_ass <- c("Assessed for Eligibility\n (n=886, 26.19%)") 
txt_arm <- c("Control\n (n=461, 52.03%)", "Intervention\n (n=425, 47.97%)")
txt_ex_control <- "Excluded (n=168, 36.44%):\n\u2022 Gestation < 28 weeks (n=152, 90.48%)\n\u2022 Hearing voices that others cannot hear (n=3, 1.89%)\n\u2022 Holding unusual beliefs (n=4, 2.38%)\n\u2022 Memory problem (n=3, 1.89%)\n\u2022 Self harm (n=5, 2.98%)\n\u2022 Self harm and memory problem (n=1, 0.60%)"
txt_ex_intervention <- "Excluded (n=170, 40.00%):\n\u2022 Gestation < 28 weeks (n=165, 97.06%)\n\u2022 Memory problem (n=1, 0.59%)\n\u2022 Self harm (n=4, 2.35%)"
txt_ex <- c(txt_ex_control, txt_ex_intervention)
txt_eli <- c("Eligible\n (n=293, 63.56%)", "Eligible\n (n=255, 60.00%)")
txt_decline_control <- "Declined Enrollment (n=16, 5.46%)\n\u2022 Consult spouse (n=2, 12.50%)\n\u2022 Not enough time (n=3, 18.75%)\n\u2022 Not interested (n=3, 18.75%)\n\u2022 Relocate post delivery (n=5, 31.25%)\n\u2022 Time to think about it (n=3, 18.75%)"
txt_decline_intervention <- "Declined Enrollment (n=11, 4.31%)\n\u2022 Not enough time (n=4, 36.36%)\n\u2022 Relocate post delivery (n=7, 63.64%)"
txt_decline <- c(txt_decline_control, txt_decline_intervention)
txt_enrol <- c("Enrolled (n=277, 94.54%)", 
               "Enrolled (n=244, 95.69%)")
txt_pm <- c("Study Nurse PM+ Yields\n (n=0, 0%)", 
            "Study Nurse PM+ Yields\n (n=59, 21.31%)")

consort_per <- add_box(txt = txt_anc) |>
    add_box(txt = txt_ass) |>
    add_split(txt = txt_arm) |>
    add_side_box(txt = txt_ex) |>
    add_box(txt = txt_eli) |>
    add_side_box(txt = txt_decline) |>
    add_box(txt = txt_enrol) |>
    add_box(txt= txt_pm)

consort_per


