# Header ------------------------------------------------------------------

# Author(s): Dowaga
# Date: May 8, 2025
# Telpsychaitry Sessions

# Setup ------------------------------------------------------------------------
# rm(list = ls())
# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
# source("Dependencies.R")
# source("data_import.R")

# data prep --------------------------------------------------------------------
telepsych_dates <- telepsych %>%
    mutate(tele_date = ymd(tele_date)) %>% 
    select(tele_provider, tele_date, pt_attend, tele_ancid) %>% 
    rename(Psychiatrist = tele_provider,
           `Psychiatry Date` = tele_date,
           `Participant Attened` = pt_attend)

# ANC Check
anc_number <- telepsych %>% 
    select(record_id, tele_ancid)

#Total Telepsychiatry participants
telep_ids <-  anc_number %>%
    #Offered telepyschaitry as treatment but not referred
    filter(!tele_ancid == "07-2025-03-0092") %>%
   distinct(tele_ancid)

total_telep <- anc_number %>%
    #Offered telepyschaitry as treatment but not referred
    filter(!tele_ancid == "07-2025-03-0092") %>% 
    summarise(total = n_distinct(tele_ancid)) %>%
    pull(total)



# PM+ and Telepsychiatry Referrals
pm_telep_df <- ppw_rct_df %>% 
    #filter(redcap_event_name == "Enrollment (Arm 1: Intervention)") %>% 
    select(record_id, clt_study_site, clt_date, starts_with("abs_")) %>% 
    filter(!is.na(clt_date)) 

# PM+ Session 5 abstractions
pm_session5_df <- ppw_rct_df%>% 
    filter(redcap_event_name == "PM+ Session 5 Abstraction (Arm 1: Intervention)") %>% 
    select(record_id, clt_study_site, clt_date, starts_with("abs_"))


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
                (max_score >= 10 & max_score < 15 &
                     (max_score == phq9_scores | max_score == gad7_scores)) ~ "PM+",
                max_score >= 15 ~ "Telepsychiatry",
                TRUE ~ "Not Eligible"
            ),
            referred_to = case_when(
                abs_gad7_ref_tele == "Yes" | abs_phq_ref_tele == "Yes" ~ "Telepsychiatry",
                abs_phq_ref_pm == "Yes" | abs_gad7_ref_pm == "Yes" ~ "PM+",
                TRUE ~ NA_character_
            )
        )

referral_QCs <- pm_telep_df %>% 
    filter(eligible_for == "Telepsychiatry" & referred_to == "PM+")
#------
# Two participants endorsed PHQ9 item9 though were not referred
# to Telepsychiatry due to low Self harm Assessment Score
#----
# PM+ participants
pm_plus_df <- pm_telep_df %>% 
    filter(max_score >= 10 & max_score < 15) %>%
    select(-max_score)  # Remove max_score if not needed

# Tele-psychiatry referrals
telepsych_referrals <- pm_telep_df %>% 
    filter((phq9_scores>=15)|(gad7_scores>=15)|(abs_phq_dead > 0 & abs_phq_ref_tele == "Yes"))

tel_refer <- pm_telep_df %>% 
    filter(abs_phq_ref_tele == "Yes" & referred_to == "PM+")

telepsych_ids <- telepsych_referrals %>% 
    select(clt_study_site, record_id) %>% 
    mutate(tele = "Yes")


# Check those who had no remission after PM+ completion----
session5_abstractions <- ppw_rct_df%>% 
    filter(redcap_event_name == "PM+ Session 5 Abstraction (Arm 1: Intervention)") %>% 
    select(record_id, clt_study_site, clt_date, starts_with("abs_"))

# Recode PHQ9 variables
session5_abstractions <- session5_abstractions %>%
    mutate(across(c(abs_phq_interest, abs_phq_down, abs_phq_sleep,
                    abs_phq_tired, abs_phq_appetite, abs_phq_bad,
                    abs_phq_concentrate, abs_phq_slow, abs_phq_dead), ~ recode(., !!!phq9_labels)),
           across(c(abs_gad7_nerve, abs_gad7_uncontrol, 
                    abs_gad7_worry, abs_gad7_relax, abs_gad7_restless,
                    abs_gad7_annoyed, abs_gad7_afraid), ~ recode(., !!!gad7_labels)))



session5_abstractions <- session5_abstractions %>% 
    mutate(phq9_scores = rowSums(select(., abs_phq_interest, abs_phq_down,
                                        abs_phq_sleep, abs_phq_tired, 
                                        abs_phq_appetite, abs_phq_bad,
                                        abs_phq_concentrate, abs_phq_slow, 
                                        abs_phq_dead), na.rm = TRUE),
           gad7_scores = rowSums(select(., abs_gad7_nerve, abs_gad7_uncontrol,
                                        abs_gad7_worry, abs_gad7_relax, 
                                        abs_gad7_restless, abs_gad7_annoyed,
                                        abs_gad7_afraid), na.rm = TRUE))



session5_abstractions <- session5_abstractions %>% 
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
# Endorsed PHQ9 Question 9
self_harm <- pm_telep_df %>% 
    filter(abs_phq_dead > 0)

# Select the participant IDs and ANC Numbers from the consent----
consent_ids <- screening_consent_df %>% 
    filter(rct_enrolling == "Yes") %>% 
    select(anc_num, partipant_id)

# Telepsychiatry Uptake Summary----
# correcting table
anc_corrections <- data.frame(
    wrong = c("03-2025/12/26", "2026/2/28", "07-2026--05-0185", 
              "2024/11/0496", "137/11/2025"),
    correct = c("03-2025/12/06", "03-2026/02/28", "07-2026-05-0185", 
                "17-2024/11/0496", "04-2025/11/137")
)

# join and replace
telepsych_dates <- telepsych_dates %>%
    left_join(anc_corrections, by = c("tele_ancid" = "wrong")) %>%
    mutate(tele_ancid = if_else(!is.na(correct), correct, tele_ancid)) %>%
    select(-correct)


# Keep participants who were referred to telepsychiatry
telepsy_ancids <- right_join(
    consent_ids,
    telepsych_ids,
    by = c("partipant_id" = "record_id")
)

# Join Telepsychiatry dates with ANCIDs
tele_dates <- telepsych_dates %>% 
    full_join(telepsy_ancids, 
               by = c("tele_ancid" = "anc_num"))


# Sessions summary per participant
sessions_summary <- tele_dates %>% 
    group_by(tele_ancid) %>%
    summarise(
        sessions_attended = sum(`Participant Attened` == "Yes", na.rm = TRUE),  
        .groups = "drop"
    )


# Summarise attendance per participant
attendance_summary <- tele_dates %>%
    group_by(tele_ancid) %>%
    summarise(
        ever_attended = ifelse(any(`Participant Attened` == "Yes", na.rm = TRUE), "Yes", "No"),
        .groups = "drop"
    )

# Merge with referral + sessions summary
ref_summary <- tele_dates %>%
    distinct(tele_ancid, .keep_all = TRUE) %>%
    select(tele_ancid, tele) %>%
    right_join(sessions_summary, by = "tele_ancid") %>%
    left_join(attendance_summary, by = "tele_ancid") %>% 
    mutate(tele = if_else(is.na(tele), "No", tele)) %>% 
    #drop a none study participant offered Telepsychiatry as a treatment
    filter(!tele_ancid %in% c("07-2025-03-0092"))


Summary <- ref_summary %>%
    tbl_summary(
        include = c(tele, sessions_attended, ever_attended),
        type = list(sessions_attended ~ "continuous"),
        missing = "no",
        statistic = list(
            tele ~ "{n} ({p}%)",
            sessions_attended ~ "{median} ({p25}, {p75})",
            ever_attended ~ "{n} ({p}%)"
        ),
        label = list(
            tele ~ "Telepsychiatry Referral (Yes/No)",
            sessions_attended ~ "Sessions Attended (Median [IQR])",
            ever_attended ~ "Ever Attended (Yes/No)"
        )
    ) %>%
    modify_caption("**Telepsychiatry Referral and Uptake Summary**") %>%
    modify_footnote(update = list(
        all_stat_cols() ~ "Attendance defined as ???1 session attended"
    ))


#write.csv(telepsych_dates, "C:/Users/DAMARIS/Desktop/IPMH/Telepsychiatry/Telepsychiatry Session Dates.csv", 
          #row.names = FALSE)

# Fidelity to treatment---------------------------------------------------------
screened <- screening_consent_df %>% 
    select(partipant_id, anc_num, study_site) %>% 
    mutate(screened = "Yes")

screened_pmad <- screened %>% 
    nrow()

pm_participants <- pm_plus_df %>% 
    select(record_id, clt_study_site) %>% 
    mutate(pm_plus = "Yes")

PM_referrals <- pm_participants %>% 
    nrow()

# Select distinct ptids in the PM+ Survey
pm_ptids <- pm_survey_df %>% 
    filter(!is.na(pm_ptid)) %>%
    select(pm_ptid, pm_facility) %>%
    distinct(pm_ptid, .keep_all = TRUE)

# First, rename pm_ptid to match the column in pm_participants
new_pm_ptids <- pm_ptids %>%
    rename(record_id = pm_ptid,
           clt_study_site = pm_facility) %>%
    filter(!(record_id %in% pm_participants$record_id)) %>%
    mutate(pm_plus = "Yes")

# Then, bind these new participants to the original pm_participants
pm_participants <- bind_rows(pm_participants, new_pm_ptids)

#Study participants in the PM+ Survey
pm_abstractions <- pm_survey_df %>%
    mutate(pm_ptid = as.numeric(pm_ptid)) %>%       # if needed
    filter(ipmh_participant == "Yes") %>%
    distinct(pm_ptid, .keep_all = TRUE)

pm_df <- pm_abstractions %>% 
    select(pm_ptid, pm_facility) %>% 
    mutate(pm_plus = "Yes")


fidelity_df <- screened %>% 
    left_join(pm_df, by = c("partipant_id" = "pm_ptid")) %>% 
    left_join(telepsy_ancids, by = c("partipant_id" = "partipant_id")) %>% 
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
                "Moderate symptoms (10-14 Scores in either GAD-7 or PHQ-9)", 
                "Severe symptoms (>=15 in PHQ-9 or GAD7) or no response to PM+"),
    `Delivered By` = c("Lay Health Worker", "Trained HC Worker (Nurse)", 
                       "Psychiatrist"),
    Duration = c("1 session (30-45 mins)", "5 sessions (weekly)", 
                 "As recommended")
)

#---
fidelity_table <- step_data %>%
    left_join(step_counts, by = "Step")

fidelity_summary <- fidelity_table %>%
    rename(
        "Step" = Step,
        "Intervention" = Intervention,
        "Eligibility / Trigger" = Trigger,
        "Delivered By" = Delivered.By,
        "Duration/Frequency" = Duration,
        "Participants (n)" = `N Participants`
    ) %>%
    kbl(
        caption = "Stepped Care Intervention Table",
        booktabs = TRUE,
        align = "l"
    ) %>%
    kable_styling(
        font_size = 10,
        bootstrap_options = c("striped", "hover", "condensed", "scale_down"),
        full_width = FALSE,
        position = "center"
    )

fidelity_summary

