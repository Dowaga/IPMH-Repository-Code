# Header ------------------------------------------------------------------

# Author(s): Dowaga
# Date: May 8, 2025
# Telpsychaitry Sessions

# Setup ------------------------------------------------------------------------

# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")

# data prep --------------------------------------------------------------------
telepsych_dates <- telepsychiatry_df %>%
    mutate(tele_date = as_date(tele_date, format = "%Y-%m-%d")) %>% 
    select(tele_provider, tele_date, pt_attend, tele_ancid) %>% 
    rename(Psychiatrist = tele_provider,
           `Psychiatry Date` = tele_date,
           `Participant Attened` = pt_attend)

# PM+ and Telepsychiatry Referrals
pm_telep_df <- ppw_rct_df %>% 
    filter(redcap_event_name == "Enrollment (Arm 1: Intervention)") %>% 
    select(record_id, clt_study_site, clt_date, starts_with("abs_")) %>% 
    filter(!is.na(clt_date)) 

# Define PHQ9 recoding
phq9_labels <- c(
    "not at all" = 0,
    "several days" = 1,
    "more than half the days" = 2,
    "nearly every day" = 3
)

gad7_labels <-c(
    "Not at all" = 0,
    "Several days" = 1,
    "Over half the days"= 2,
    "Nearly every day" = 3)


# Recode PHQ9 variables
pm_telep_df <- pm_telep_df %>%
    mutate(across(c(abs_phq_interest, abs_phq_down, abs_phq_sleep,
                    abs_phq_tired, abs_phq_appetite, abs_phq_bad,
                    abs_phq_concentrate, abs_phq_slow, abs_phq_dead), ~ recode(., !!!phq9_labels)),
           across(c(abs_gad7_nerve, abs_gad7_uncontrol, 
                    abs_gad7_worry, abs_gad7_relax, abs_gad7_restless,
                    abs_gad7_annoyed, abs_gad7_afraid), ~ recode(., !!!gad7_labels)))



pm_telep_df <- pm_telep_df %>% 
    mutate(phq9_scores = rowSums(select(., abs_phq_interest, abs_phq_down,
                                        abs_phq_sleep, abs_phq_tired, 
                                        abs_phq_appetite, abs_phq_bad,
                                        abs_phq_concentrate, abs_phq_slow, 
                                        abs_phq_dead), na.rm = TRUE),
           gad7_scores = rowSums(select(., abs_gad7_nerve, abs_gad7_uncontrol,
                                        abs_gad7_worry, abs_gad7_relax, 
                                        abs_gad7_restless, abs_gad7_annoyed,
                                        abs_gad7_afraid), na.rm = TRUE))

# Endorsed PHQ9 Question 9
self_harm <- pm_telep_df %>% 
    filter(abs_phq_dead > 0)

pm_telep_df <- pm_telep_df %>% 
    filter((phq9_scores >= 10)|(gad7_scores >= 10)|(abs_phq_dead == 1 & abs_phq_ref_tele == "Yes")) %>% 
    mutate(
        max_score = pmax(phq9_scores, gad7_scores, na.rm = TRUE),  # Get the greatest score
        eligible_for = case_when(
            abs_phq_dead > 0 ~ "Telepsychiatry",
            (max_score >= 10 & max_score < 15 & (max_score == phq9_scores | max_score == gad7_scores)) ~ "PM+",
            max_score >= 15 ~ "Telepsychiatry",
            TRUE ~ "Not Eligible"),
        referred_to = case_when(abs_phq_ref_pm == "Yes"|abs_gad7_ref_pm == "Yes" ~ "PM+",
                                abs_gad7_ref_tele == "Yes" |abs_phq_ref_tele == "Yes" ~ "Telpsychiatry",
                                TRUE ~ NA_character_))


#PM+ participants
pm_plus_df <- pm_telep_df %>% 
    filter(max_score >= 10 & max_score < 15) %>%
    select(-max_score)  # Remove max_score if not needed

# Tele-psychiatry referrals
telepsych_referrals <- pm_telep_df %>% 
    filter((phq9_scores>=15)|(gad7_scores>=19)|(abs_phq_dead >0 & abs_phq_ref_tele == "Yes"))

telepsych_ids <- telepsych_referrals %>% 
    select(clt_study_site, record_id) %>% 
    mutate(tele = "Yes")

consent_ids <- screening_consent_df %>% 
    filter(rct_enrolling == "Yes") %>% 
    select(anc_num, partipant_id)

# Keep participants who were referred to telepsychiatry
telepsy_ancids <- right_join(
    consent_ids,
    telepsych_ids,
    by = c("partipant_id" = "record_id")
)

# Join Telepsychiatry dates with telepsy_ancids

tele_dates <- telepsych_dates %>% 
    right_join(telepsy_ancids, 
               by = c("tele_ancid" = "anc_num"))


write.csv(telepsych_dates, "C:/Users/DAMARIS/Desktop/IPMH/Telepsychiatry/Telepsychiatry Session Dates.csv", 
          row.names = FALSE)

# Fidelity to treatment---------------------------------------------------------
screened <- screening_consent_df %>% 
    select(partipant_id, anc_num, study_site) %>% 
    mutate(screened = "Yes")

pm_participants <- pm_plus_df %>% 
    select(record_id, clt_study_site) %>% 
    mutate(pm_plus = "Yes")

fidelity_df <- screened %>% 
    left_join(pm_participants, by = c("partipant_id" = "record_id", 
                                      "study_site" = "clt_study_site")) %>% 
    left_join(telepsych_ids, by = c("partipant_id" = "record_id")) %>% 
    select(-clt_study_site)



# Get counts
step_counts <- fidelity_df %>%
    summarise(
        Step_1 = sum(screened == "Yes", na.rm = TRUE),
        Step_2 = sum(pm_plus == "Yes", na.rm = TRUE),
        Step_3 = sum(tele == "Yes", na.rm = TRUE)
    ) %>%
    pivot_longer(cols = everything(), names_to = "Step", values_to = "N Participants") %>%
    mutate(Step = case_when(
        Step == "Step_1" ~ "Step 1",
        Step == "Step_2" ~ "Step 2",
        Step == "Step_3" ~ "Step 3"
    ))

# Fidelity count
step_data <- data.frame(
    Step = c("Step 1", "Step 2", "Step 3"),
    Intervention = c("PMAD Screening", "Problem Management Plus (PM+)",
                     "Telepsych / Specialist Referral"),
    Trigger = c("All ANC Participants", 
                "Moderate symptoms(10-14 Scores in either GAD-7 or PHQ-9)", 
                "Severe symptoms(>=15 in PHQ-9 or GAD7) or no response to PM+"),
    `Delivered By` = c("Lay Health Worker", "Trained HC Worker (Nurse)", 
                       "Psychiatrist"),
    Duration = c("1 session (30-45 mins)", "5 sessions (weekly)", 
                 "As recommended")
)

#
fidelity_table <- step_data %>%
    left_join(step_counts, by = "Step")

fidelity_summary <- fidelity_table %>%
    gt() %>%
    tab_header(
        title = "Stepped Care Intervention Table"
    ) %>%
    cols_label(
        Step = "Step",
        Intervention = "Intervention",
        Trigger = "Eligibility / Trigger",
        `Delivered.By` = "Delivered By",
        Duration = "Duration/Frequency",
        `N Participants` = "Participants (n)"
    ) %>%
    tab_options(
        row.striping.include_table_body = TRUE,
        column_labels.font.weight = "bold",
        heading.title.font.size = 16
    )

fidelity_summary