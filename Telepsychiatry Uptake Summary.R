# Header ------------------------------------------------------------------

# Author(s): Dowaga
# Date: June 8, 2026
# Telpsychaitry Sessions

# Setup ------------------------------------------------------------------------
rm(list = ls())

# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")

# data prep --------------------------------------------------------------------

telepsych <- telepsych %>%
    group_by(tele_date, tele_ancid, tele_initials) %>%   # group by date, ANC and initials
    distinct() %>% # drop duplicate rows within groups
    ungroup()

telepsych_dates <- telepsych %>%
    mutate(tele_date = ymd(tele_date)) %>% 
    select(tele_provider, tele_date, pt_attend, tele_ancid) %>% 
    rename(Psychiatrist = tele_provider,
           `Psychiatry Date` = tele_date)


# ANC Check in the Telepsychiatry survey sessions
anc_number <- telepsych %>% 
    select(record_id, tele_ancid)


# PM+ and Telepsychiatry Referrals for the RCT Database
pm_telep_df <- ppw_rct_df %>% 
    # filter(redcap_event_name == "Enrollment (Arm 1: Intervention)") %>% 
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

# Those who endorsed PHQ9 item 9-----
referral_QCs <- pm_telep_df %>% 
    filter(eligible_for == "Telepsychiatry" & referred_to == "PM+")

# Three participants endorsed PHQ9 item9 though were not referred
# to Telepsychiatry due to low Self harm Assessment Score
#----

# Tele-psychiatry referrals
telepsych_referrals <- pm_telep_df %>% 
    filter((phq9_scores>=15)|(gad7_scores>=15)|(abs_phq_dead > 0 & abs_phq_ref_tele == "Yes"))


telepsych_ids <- telepsych_referrals %>% 
    select(clt_study_site, record_id) %>% 
    mutate(tele = "Yes")


# Check those who had no remission after PM+ completion----
session5_abstractions <- ppw_rct_df%>% 
    filter(redcap_event_name == "PM+ Session 5 Abstraction (Arm 1: Intervention)") %>% 
    select(record_id, clt_study_site, clt_date, starts_with("abs_"))



# Select the participant IDs and ANC Numbers from the consent----
consent_ids <- screening_consent_df %>% 
    filter(rct_enrolling == "Yes") %>% 
    select(anc_num, partipant_id)


# Telepsychiatry Uptake Summary----


# Keep participants who were referred to telepsychiatry
telepsy_ancids <- right_join(
    consent_ids,
    telepsych_ids,
    by = c("partipant_id" = "record_id")
)

# correcting table
anc_corrections <- data.frame(
    wrong = c("03-2025/12/26", "2026/2/28", "07-2026--05-0185",
              "2024/11/0496", "137/11/2025", "13-233/25", "03 - 2026/2/0030",
              "03 - 2026/3/31", "03 - 2026/5/37", "042/2026", "018/01/2026",
              "16-226/03/29", "238/1/2026", "13-014/26", "2026/05/13", 
              "2026/3/6", "13-85/25", "19-2026/03/010"),
    correct = c("03-2025/12/06", "03-2026/02/28", "07-2026-05-0185",
                "17-2024/11/0496", "04-2025/11/137", "13-2025/09/035",
                "03-2026/2/10030", "03-2026/3/31", "03-2026/5/37",
                "13-042/2026", "13-018/01/2026", "16-2026/03/29",
                "16-2026/1/238","13-014/2026", "16-2026/05/13", 
                "16-2026/3/6", "13-85/26", "19-2026/02/010")
)

# join and replace
telepsych_dates <- telepsych_dates %>%
    left_join(anc_corrections, by = c("tele_ancid" = "wrong")) %>%
    mutate(tele_ancid = if_else(!is.na(correct), correct, tele_ancid)) %>%
    select(-correct)

# Join Telepsychiatry dates by ANCIDs
tele_dates <- telepsych_dates %>% 
    full_join(telepsy_ancids, 
              by = c("tele_ancid" = "anc_num"))


# Sessions summary per participant
sessions_summary <- tele_dates %>% 
    group_by(tele_ancid) %>%
    summarise(
        sessions_attended = sum(pt_attend == "Yes", na.rm = TRUE),  
        .groups = "drop"
    )


# Summarise attendance per participant
attendance_summary <- tele_dates %>%
    group_by(tele_ancid) %>%
    summarise(
        ever_attended = ifelse(any(pt_attend == "Yes", na.rm = TRUE), "Yes", "No"),
        .groups = "drop"
    )

# Merge with referral + sessions summary
ref_summary <- tele_dates %>%
    distinct(tele_ancid, .keep_all = TRUE) %>%
    select(tele_ancid, tele) %>%
    right_join(sessions_summary, by = "tele_ancid") %>%
    left_join(attendance_summary, by = "tele_ancid") %>% 
    mutate(tele = if_else(is.na(tele), "No", tele)) %>% 
    # drop a none study participant offered Telepsychiatry as a treatment
    filter(!tele_ancid %in% c("07-2025-03-0092")) %>% 
    # drop participant reffered out of study visit
    filter(!tele_ancid %in% c("07-2025-10-363")) %>% 
    # Drop Tunawiri data
    filter(!tele_ancid %in% c("KAR-T-14106-1-09")) %>%
    mutate(
        sessions_cat = case_when(
            sessions_attended == 0 ~ "0 Sessions (None)",
            TRUE ~ paste0(sessions_attended, " Sessions")
        ),
        # Factor levels: only those present in the data, ordered descending
        sessions_cat = factor(
            sessions_cat,
            levels = sort(unique(sessions_cat), decreasing = TRUE)
        )
    )

# --------------------------------------------------------------------
# Telepsychiatry drug prescribing summary
# --------------------------------------------------------------------

# Define lookup for drug codes
drug_lookup <- c(
    tele_med___1  = "Chlorpromazine",
    tele_med___2  = "Olanzapine",
    tele_med___3  = "Haloperidol",
    tele_med___4  = "Propiomazine",
    tele_med___5  = "Amitriptyline",
    tele_med___6  = "Fluoxetine",
    tele_med___7  = "Diazepam",
    tele_med___8  = "Midazolam",
    tele_med___9  = "Bromazepam",
    tele_med___10 = "Carbamazepine",
    tele_med___11 = "Phenobarbitone",
    tele_med___12 = "Phenytoin",
    tele_med___13 = "Benzhexol",
    tele_med___99 = "Other"
)

# Pivot drug checkboxes into long format
drug_long <- telepsych %>%
    select(tele_ancid, tele_date, starts_with("tele_med___"),
           tele_medoth, tele_dose) %>%
    pivot_longer(cols = starts_with("tele_med___"),
                 names_to = "drug_code", values_to = "selected") %>%
    filter(selected %in% c("Yes","1","Checked")) %>%
    mutate(
        drug_name = recode(drug_code, !!!drug_lookup),
        drug_name = case_when(
            drug_name == "Other" & !is.na(tele_medoth) ~ paste0("Other: ", tele_medoth),
            drug_name == "Other" & !is.na(tele_dose) ~ paste0("Other: ", tele_dose),
            TRUE ~ drug_name
        )
    )

# Summarise per participant
drug_summary <- drug_long %>%
    group_by(tele_ancid) %>%
    summarise(
        ever_prescribed = "Yes",
        drugs_list = paste(unique(drug_name), collapse = "; "),
        .groups = "drop"
    )

# Merge into your uptake summary
ref_summary <- ref_summary %>%
    left_join(drug_summary, by = "tele_ancid") %>%
    mutate(
        ever_prescribed = ifelse(is.na(ever_prescribed), "No", ever_prescribed),
        drugs_list_clean = case_when(
            str_detect(str_to_lower(drugs_list), "chlorpromazine") ~ "Chlorpromazine",
            str_detect(str_to_lower(drugs_list), "olanzapine")    ~ "Olanzapine",
            str_detect(str_to_lower(drugs_list), "haloperidol")   ~ "Haloperidol",
            str_detect(str_to_lower(drugs_list), "propiomazine")  ~ "Propiomazine",
            str_detect(str_to_lower(drugs_list), "amitriptyline") ~ "Amitriptyline",
            str_detect(str_to_lower(drugs_list), "fluoxetine")    ~ "Fluoxetine",
            str_detect(str_to_lower(drugs_list), "diazepam")      ~ "Diazepam",
            str_detect(str_to_lower(drugs_list), "midazolam")     ~ "Midazolam",
            str_detect(str_to_lower(drugs_list), "bromazepam")    ~ "Bromazepam",
            str_detect(str_to_lower(drugs_list), "carbamazepine") ~ "Carbamazepine",
            str_detect(str_to_lower(drugs_list), "phenobarbitone")~ "Phenobarbitone",
            str_detect(str_to_lower(drugs_list), "phenytoin")     ~ "Phenytoin",
            str_detect(str_to_lower(drugs_list), "benzhexol")     ~ "Benzhexol",
            str_detect(str_to_lower(drugs_list), "sertraline|setraline") ~ "Sertraline",
            str_detect(str_to_lower(drugs_list), "folic acid")    ~ "Folic acid",
            TRUE ~ NA_character_
        ))


# Update summary table
tele_uptake_summary <- ref_summary %>%
    tbl_summary(
        include = c(tele, sessions_attended, ever_attended, sessions_cat, 
                    ever_prescribed, drugs_list_clean),
        type = list(sessions_attended ~ "continuous"),
        missing = "no",
        statistic = list(
            tele ~ "{n} ({p}%)",
            sessions_attended ~ "{median} ({p25}, {p75})",
            ever_attended ~ "{n} ({p}%)",
            ever_prescribed ~ "{n} ({p}%)"
        ),
        label = list(
            tele ~ "Telepsychiatry Referral (Yes/No)",
            sessions_attended ~ "Sessions Attended (Median [IQR])",
            ever_attended ~ "Ever Attended a session (Yes/No)",
            sessions_cat ~ "Distribution of Sessions Attended",
            ever_prescribed ~ "Any Drugs Prescribed During Telepsychiatry (Yes/No))",
            drugs_list_clean ~ "Drugs Prescribed"
        )
    ) %>%
    bold_labels() %>%
    italicize_levels() %>%
    modify_caption("**Telepsychiatry Referral, Uptake, and Drug Prescription Summary**") %>%
    modify_footnote(update = list(all_stat_cols() ~ "Attendance defined as >=1 session attended"))


