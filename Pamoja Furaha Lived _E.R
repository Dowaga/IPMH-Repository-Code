# Header ------------------------------------------------------------------

# Author(s): Dowaga
# Date: July 13, 2026
# Baseline Demographics

# Setup ------------------------------------------------------------------------
rm(list = ls())
# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")

# data prep --------------------------------------------------------------------
demographics_df <- ppw_rct_df %>% 
    mutate(
        visit_type = case_when(
            grepl("Enrollment", redcap_event_name) ~ "Enrollment",
            grepl("6 Weeks", redcap_event_name) ~ "6 Weeks",
            grepl("14 Weeks", redcap_event_name) ~ "14 Weeks", 
            grepl("6 Months", redcap_event_name) ~ "6 Months",
            TRUE ~ NA_character_
        )) %>% 
    filter(visit_type == "Enrollment") %>%
    group_by(record_id) %>%
    slice_head(n = 1) %>%   # keep only the Enrollment row per record_id
    ungroup() %>% 
    select(clt_ptid, clt_study_site, clt_date,starts_with("dem_"), demographics_complete, med_pre_preg, 
           med_pastdiag___2)%>% 
    mutate(
        med_pre_preg = replace_na(med_pre_preg, "No")
    ) %>% 
    mutate(dem_age = if_else(
        dem_dob_uk == "Yes",
        floor(time_length(interval(dem_dob, clt_date), "years")),
        dem_age
    ),
    dem_current_partner_num = ifelse(dem_current_partner == "Yes", 1, 0),
    dem_maritalstat_num = dplyr::case_when(
        dem_maritalstat == "Currently married"~ 1,
        dem_maritalstat %in% c("Prefer not to answer", NA) ~ NA_real_,
        TRUE ~ 0),
    dem_marriage = dplyr::case_when(
        dem_marriage == "Monogamous" ~ 1,
        dem_marriage %in% c("Prefer not to answer", NA) ~ NA_real_,
        TRUE ~ 0),
    dem_employment = recode(dem_employment,
                            `Prefer not to answer` = "Yes"),
    dem_pc_residence_num = dplyr::case_when(
        dem_pc_residence == "Yes (Ndio) [Kamano]" ~ 1,
        dem_pc_residence == "No (La) [Ooyo]" ~ 0,
        dem_pc_residence == "Prefer not to answer (Singependa kujibu) [Ok daher mar duoko penjo ni]" ~ 0,
        TRUE ~ NA_real_
    ))%>%
    mutate(arm = stringr::str_extract(clt_study_site,"^\\d{2}"),
           arm_group = ifelse(arm %in% c("02","05", "06", "08", "11", "14", "15", "18", "20", "21"),
                              "Control", "Intervention"))

# Step 1: Select 14-19 year olds from demographics
pamoja_furaha_age <- demographics_df %>% 
    filter(dem_age >= 14 & dem_age <= 19) %>% 
    select(clt_ptid, clt_date, dem_age)

# Step 2: Flag participants who completed 6-month visit
completed_6mths <- ppw_rct_df %>% 
    mutate(
        visit_type = case_when(
            grepl("Enrollment", redcap_event_name) ~ "Enrollment",
            grepl("6 Weeks", redcap_event_name) ~ "6 Weeks",
            grepl("14 Weeks", redcap_event_name) ~ "14 Weeks", 
            grepl("6 Months", redcap_event_name) ~ "6 Months",
            TRUE ~ NA_character_
        )
    ) %>% 
    filter(visit_type == "6 Months") %>% 
    distinct(clt_ptid) %>% 
    mutate(completed_visit = "Yes")

# Step 3: Merge back with the first dataset
pamoja_furaha <- pamoja_furaha_age %>% 
    left_join(completed_6mths, by = "clt_ptid")



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

treatment_referrals <- pm_telep_df %>% 
    select(clt_ptid=record_id, referred_to)

pamoja_furaha_final <- pamoja_furaha%>% left_join(treatment_referrals, 
                                                  by = "clt_ptid")

# PM+ Session 5 abstractions
pm_session5_df <- ppw_rct_df%>% 
    filter(redcap_event_name == "PM+ Session 5 Abstraction (Arm 1: Intervention)") %>% 
    select(record_id, clt_study_site, clt_date, starts_with("abs_")) %>% 
    mutate(session5_abstracted = "Yes") %>% 
    select(clt_ptid = record_id, session5_abstracted)

pamoja_furaha_final <- pamoja_furaha_final %>% 
    left_join(pm_session5_df, by = "clt_ptid")

# --------------------------------------------------------------------
# Youths (14-19 years) enrolled in PM+ and completed study
# --------------------------------------------------------------------

# Step 0: Prepare demographics dataset -------------------------------
demographics_df <- ppw_rct_df %>% 
    mutate(
        # Harmonize visit types
        visit_type = case_when(
            grepl("Enrollment", redcap_event_name) ~ "Enrollment",
            grepl("6 Weeks", redcap_event_name) ~ "6 Weeks",
            grepl("14 Weeks", redcap_event_name) ~ "14 Weeks", 
            grepl("6 Months", redcap_event_name) ~ "6 Months",
            TRUE ~ NA_character_
        )) %>% 
    filter(visit_type == "Enrollment") %>% 
    group_by(record_id) %>%
    slice_head(n = 1) %>%   # keep only the Enrollment row per record_id
    ungroup() %>% 
    select(clt_ptid, clt_study_site, clt_date, starts_with("dem_"), demographics_complete, 
           med_pre_preg, med_pastdiag___2) %>% 
    mutate(
        med_pre_preg = replace_na(med_pre_preg, "No"),
        # Calculate age if DOB available
        dem_age = if_else(
            dem_dob_uk == "Yes",
            floor(time_length(interval(dem_dob, clt_date), "years")),
            dem_age
        ),
        # Numeric encodings for marital/partner status
        dem_current_partner_num = ifelse(dem_current_partner == "Yes", 1, 0),
        dem_maritalstat_num = case_when(
            dem_maritalstat == "Currently married" ~ 1,
            dem_maritalstat %in% c("Prefer not to answer", NA) ~ NA_real_,
            TRUE ~ 0),
        dem_marriage = case_when(
            dem_marriage == "Monogamous" ~ 1,
            dem_marriage %in% c("Prefer not to answer", NA) ~ NA_real_,
            TRUE ~ 0),
        dem_employment = recode(dem_employment, `Prefer not to answer` = "Yes"),
        dem_pc_residence_num = case_when(
            dem_pc_residence == "Yes (Ndio) [Kamano]" ~ 1,
            dem_pc_residence == "No (La) [Ooyo]" ~ 0,
            dem_pc_residence == "Prefer not to answer (Singependa kujibu) [Ok daher mar duoko penjo ni]" ~ 0,
            TRUE ~ NA_real_
        ),
        # Intervention vs Control arms
        arm = stringr::str_extract(clt_study_site,"^\\d{2}"),
        arm_group = ifelse(arm %in% c("02","05","06","08","11","14","15","18","20","21"),
                           "Control", "Intervention")
    )

# Step 1: Select 14-19 year olds -------------------------------------
pamoja_furaha_age <- demographics_df %>% 
    filter(dem_age >= 14 & dem_age <= 19) %>% 
    select(clt_ptid, clt_date, dem_age)

# Step 2: Flag participants who completed 6-month visit --------------
completed_6mths <- ppw_rct_df %>% 
    mutate(
        visit_type = case_when(
            grepl("Enrollment", redcap_event_name) ~ "Enrollment",
            grepl("6 Weeks", redcap_event_name) ~ "6 Weeks",
            grepl("14 Weeks", redcap_event_name) ~ "14 Weeks", 
            grepl("6 Months", redcap_event_name) ~ "6 Months",
            TRUE ~ NA_character_
        )) %>% 
    filter(visit_type == "6 Months") %>% 
    distinct(clt_ptid) %>% 
    mutate(completed_visit = "Yes")

# Step 3: Merge age + completion -------------------------------------
pamoja_furaha <- pamoja_furaha_age %>% 
    left_join(completed_6mths, by = "clt_ptid")

# Step 4: Treatment referrals (PM+ / Telepsychiatry) -----------------
# Recode PHQ9 and GAD7, calculate scores, and determine referral
phq9_labels <- c("not at all" = 0,"several days" = 1,
                 "more than half the days" = 2,"nearly every day" = 3)
gad7_labels <- c("Not at all" = 0,"Several days" = 1,
                 "Over half the days"= 2,"Nearly every day" = 3)

pm_telep_df <- ppw_rct_df %>% 
    filter(redcap_event_name == "Enrollment (Arm 1: Intervention)") %>% 
    select(record_id, clt_study_site, clt_date, starts_with("abs_")) %>% 
    filter(!is.na(clt_date)) %>% 
    mutate(
        # Recode PHQ9 items ??? numeric
        across(c(abs_phq_interest, abs_phq_down, abs_phq_sleep,
                 abs_phq_tired, abs_phq_appetite, abs_phq_bad,
                 abs_phq_concentrate, abs_phq_slow, abs_phq_dead),
               ~ recode(., !!!phq9_labels) %>% as.numeric),
        # Recode GAD7 items ??? numeric
        across(c(abs_gad7_nerve, abs_gad7_uncontrol, abs_gad7_worry,
                 abs_gad7_relax, abs_gad7_restless, abs_gad7_annoyed,
                 abs_gad7_afraid),
               ~ recode(., !!!gad7_labels) %>% as.numeric)
    ) %>% 
    mutate(
        # Compute scores
        phq9_scores = rowSums(select(., abs_phq_interest, abs_phq_down, abs_phq_sleep,
                                     abs_phq_tired, abs_phq_appetite, abs_phq_bad,
                                     abs_phq_concentrate, abs_phq_slow, abs_phq_dead), na.rm = TRUE),
        gad7_scores = rowSums(select(., abs_gad7_nerve, abs_gad7_uncontrol, abs_gad7_worry,
                                     abs_gad7_relax, abs_gad7_restless, abs_gad7_annoyed,
                                     abs_gad7_afraid), na.rm = TRUE),
        max_score = pmax(phq9_scores, gad7_scores, na.rm = TRUE),
        eligible_for = case_when(
            abs_phq_dead > 0 ~ "Telepsychiatry",
            (max_score >= 10 & max_score < 15) ~ "PM+",
            max_score >= 15 ~ "Telepsychiatry",
            TRUE ~ "Not Eligible"
        ),
        # Referral flags remain as Yes/No
        referred_to = case_when(
            abs_gad7_ref_tele %in% c("Yes","1","Checked") | abs_phq_ref_tele %in% c("Yes","1","Checked") ~ "Telepsychiatry",
            abs_phq_ref_pm   %in% c("Yes","1","Checked") | abs_gad7_ref_pm   %in% c("Yes","1","Checked") ~ "PM+",
            TRUE ~ NA_character_
        )
    ) %>% 
    # Keep only those who meet criteria
    filter((phq9_scores >= 10) | (gad7_scores >= 10) | (abs_phq_dead == 1 & abs_phq_ref_tele %in% c("Yes","1","Checked")))


treatment_referrals <- pm_telep_df %>% 
    select(clt_ptid = record_id, referred_to)

# Step 5: Merge referrals --------------------------------------------
pamoja_furaha_final <- pamoja_furaha %>% 
    left_join(treatment_referrals, by = "clt_ptid")

# Step 6: Add PM+ Session 5 abstraction flag -------------------------
pm_session5_df <- ppw_rct_df %>% 
    filter(redcap_event_name == "PM+ Session 5 Abstraction (Arm 1: Intervention)") %>% 
    select(record_id, clt_study_site, clt_date, starts_with("abs_")) %>% 
    mutate(session5_abstracted = "Yes") %>% 
    select(clt_ptid = record_id, session5_abstracted)

pamoja_furaha_final <- pamoja_furaha_final %>% 
    left_join(pm_session5_df, by = "clt_ptid")

# --------------------------------------------------------------------
# Final Output: Linelist of youths (14-19) with lived experience -----
# --------------------------------------------------------------------
pamoja_furaha_final %>% 
    arrange(dem_age) %>% 
    select(clt_ptid, dem_age, completed_visit, referred_to, session5_abstracted) %>% 
    knitr::kable(caption = "Linelist of 14-19 year olds enrolled in PM+ who completed study")
