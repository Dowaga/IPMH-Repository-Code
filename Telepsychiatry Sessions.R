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
    filter(!tele_ancid == "2025-03-0092") %>%
   distinct(tele_ancid)

total_telep <- anc_number %>%
    #Offered telepyschaitry as treatment but not referred
    filter(!tele_ancid == "2025-03-0092") %>% 
    summarise(total = n_distinct(tele_ancid)) %>%
    pull(total)



# PM+ and Telepsychiatry Referrals
pm_telep_df <- ppw_rct_df %>% 
    filter(redcap_event_name == "Enrollment (Arm 1: Intervention)") %>% 
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
    filter(eligible_for == "PM+" & referred_to == "Telepsychiatry")

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

telep_referrals <- telepsych_referrals %>% 
    nrow()

consent_ids <- screening_consent_df %>% 
    filter(rct_enrolling == "Yes") %>% 
    select(anc_num, partipant_id)

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

# Endorsed PHQ9 Question 9
self_harm <- pm_telep_df %>% 
    filter(abs_phq_dead > 0)

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


# Keep participants who were referred to telepsychiatry----
telepsy_ancids <- right_join(
    consent_ids,
    telepsych_ids,
    by = c("partipant_id" = "record_id")
)

# Join Telepsychiatry dates with telepsy_ancids
tele_dates <- telepsych_dates %>% 
    full_join(telepsy_ancids, 
               by = c("tele_ancid" = "anc_num"))

# Telepsychiatry summary
sessions_summary <- tele_dates %>% 
    group_by(partipant_id) %>%
    summarise(
        sessions_attended = sum(`Participant Attened` == "Yes", na.rm = TRUE),  
        .groups = "drop"
    ) 

ref_summary <- tele_dates %>%
    distinct(partipant_id, .keep_all = TRUE) %>%   # keep first occurrence only
    select(partipant_id, tele) %>%
    right_join(sessions_summary, by = "partipant_id")

# Continuous summary (median [IQR])
tbl_cont <- ref_summary %>%
    tbl_summary(
        include = c(tele, sessions_attended),
        type = list(sessions_attended ~ "continuous"),
        statistic = list(all_continuous() ~ "{median} ({p25}, {p75})"),
        
    )

# Categorical summary (counts)
tbl_cat <- ref_summary %>%
    mutate(sessions_attended = as.factor(sessions_attended)) %>%
    select(sessions_attended) %>%
    tbl_summary(
        label = list(sessions_attended ~ "Sessions Attended")
    )

# Combine tables
Summary <- tbl_merge(
    list(tbl_cont, tbl_cat),
    tab_spanner = c("**Continuous Summary**", "**Categorical Summary**")
)

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
                "Moderate symptoms (10-14 Scores in either GAD-7 or PHQ-9)", 
                "Severe symptoms (>=15 in PHQ-9 or GAD7) or no response to PM+"),
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
    )%>% 
    opt_table_lines()

fidelity_summary


#----
# Clean and prepare
tele_clean <- telepsych %>%
    mutate(
        tele_date = ymd(tele_date),
        diagnosis = case_when(
            tele_diag___1 == "Checked" ~ "Major Depressive Disorder",
            tele_diag___2 == "Checked" ~ "Generalized Anxiety Disorder",
            tele_diag___3 == "Checked" ~ "Bipolar Mood Disorder",
            tele_diag___4 == "Checked" ~ "Perinatal Depression",
            tele_diag___5 == "Checked" ~ "Postpartum Psychosis",
            tele_diag___6 == "Checked" ~ "PTSD",
            tele_diag___00 == "Checked" ~ "None/NA",
            tele_diag___99 == "Checked" ~ "Other",
            TRUE ~ NA_character_
        ),
        medication = case_when(
            tele_med___1 == "Checked" ~ "Chlorpromazine",
            tele_med___2 == "Checked" ~ "Olanzapine",
            tele_med___3 == "Checked" ~ "Haloperidol",
            tele_med___4 == "Checked" ~ "Propiomazine",
            tele_med___5 == "Checked" ~ "Amitriptyline",
            tele_med___6 == "Checked" ~ "Fluoxetine",
            tele_med___7 == "Checked" ~ "Diazepam",
            tele_med___8 == "Checked" ~ "Midazolam",
            tele_med___9 == "Checked" ~ "Bromazepam",
            tele_med___10 == "Checked" ~ "Carbamazepine",
            tele_med___11 == "Checked" ~ "Phenobarbitone",
            tele_med___12 == "Checked" ~ "Phenytoin",
            tele_med___13 == "Checked" ~ "Benzhexol",
            tele_med___99 == "Checked" ~ "Benzhexol",
            tele_medoth == "Checked" ~ "Other",
            TRUE ~ NA_character_
        )
    )

# Summary table: Attendance and Prescriptions
summary_table <- tele_clean %>%
    summarise(
        Total_Sessions = n(),
        Attended = sum(pt_attend == "Yes", na.rm = TRUE),
        Prescribed = sum(tele_presc == "Yes", na.rm = TRUE),
        Attendance_Rate = round(Attended / Total_Sessions * 100, 1),
        Prescription_Rate = round(Prescribed / Attended * 100, 1)
    ) %>%
    gt() %>%
    tab_header(
        title = "Telepsychiatry Session Summary",
        subtitle = "Attendance and Prescription Overview"
    )

# Diagnosis frequency
diag_freq <- tele_clean %>%
    filter(pt_attend == "Yes") %>% 
    filter(!is.na(diagnosis)) %>%
    count(diagnosis, sort = TRUE) %>%
    gt() %>%
    tab_header(
        title = "Diagnosis Distribution",
        subtitle = "Across Attended Sessions"
    )

# Medication Prescribed
med_long <- tele_clean %>%
    filter(pt_attend == "Yes", tele_presc == "Yes") %>%
    select(starts_with("tele_med___")) %>%
    pivot_longer(
        everything(),
        names_to = "med_code",
        values_to = "prescribed"
    ) %>%
    filter(prescribed == "Checked")

med_freq <- med_long %>%
    count(med_code, sort = TRUE) %>%
    gt() %>%
    tab_header(
        title = "Medications Prescribed",
        subtitle = "Across Attended Sessions"
    )

