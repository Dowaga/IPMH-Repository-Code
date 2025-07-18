# Header ------------------------------------------------------------------

# Author(s): Yuwei
# Date: Jun 27, 2025
# This script is used to analyze the follow-up data from the PPW RCT study. 
# The generated tables will be included in the weekly data report for team review.

# Setup ------------------------------------------------------------------------

# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")

# Data for baseline vs. follow-up
ppw_sae_df <- ppw_sae_df %>%
    mutate(
        visit_type = case_when(
            grepl("Enrollment", redcap_event_name) ~ "Enrollment",
            grepl("6 Weeks", redcap_event_name) ~ "6 Weeks",
            grepl("14 Weeks", redcap_event_name) ~ "14 Weeks", 
            grepl("6 Months", redcap_event_name) ~ "6 Months",
            TRUE ~ NA_character_
        ),
        arm = case_when(
            grepl("Arm 1: Intervention", redcap_event_name) ~ "Intervention",
            grepl("Arm 2: Control", redcap_event_name) ~ "Control",
            TRUE ~ "Unknown"
        )
    )

ppw_rct_df <- ppw_rct_df %>%
    mutate(
        visit_type = case_when(
            grepl("Enrollment", redcap_event_name) ~ "Enrollment",
            grepl("6 Weeks", redcap_event_name) ~ "6 Weeks",
            grepl("14 Weeks", redcap_event_name) ~ "14 Weeks", 
            grepl("6 Months", redcap_event_name) ~ "6 Months",
            TRUE ~ NA_character_
        ),
        arm = case_when(
            grepl("Arm 1: Intervention", redcap_event_name) ~ "Intervention",
            grepl("Arm 2: Control", redcap_event_name) ~ "Control",
            TRUE ~ "Unknown"
        )
    )

rct_ppw_followup <- ppw_rct_df %>%
    filter(visit_type %in% c("6 Weeks", "14 Weeks", "6 Months"))

rct_ppw_baseline <- ppw_rct_df %>%
    filter(visit_type == "Enrollment")

# Table 1: Pregnancy outcomes at 6 weeks postpartum --------------
# Keep relevant variables for pregnancy outcomes at 6 weeks follow-up
pregnancy_outcomes_6week <- rct_ppw_followup %>%
    filter(visit_type == "6 Weeks") %>%
    select(clt_ptid, all_of(starts_with("tpnc_")))

baseline_lmp <- rct_ppw_baseline %>%
    select(clt_ptid, med_lmp) 

pregnancy_outcomes_6week <- pregnancy_outcomes_6week %>%
    left_join(baseline_lmp, by = "clt_ptid")

# calculate gestational age at birth in weeks
pregnancy_outcomes_6week <- pregnancy_outcomes_6week %>%
    mutate(
        tpnc_date = as.Date(tpnc_date),
        med_lmp = as.Date(med_lmp),
        
        gestage_calculated = case_when(
            !is.na(tpnc_date) & !is.na(med_lmp) ~ 
                as.numeric(tpnc_date - med_lmp) / 7,
            TRUE ~ NA_real_
        ),
        tpnc_gestfill_numeric = as.numeric(tpnc_gestfill),
        # Final gestational age: use calculated if available, otherwise use tpnc_gestfill
        gestage = case_when(
            !is.na(gestage_calculated) ~ gestage_calculated,
            !is.na(tpnc_gestfill_numeric) ~ tpnc_gestfill_numeric,
            TRUE ~ NA_real_
        )
    )

#table(pregnancy_outcomes_6week$gestage, useNA = "ifany")            

table1 <- pregnancy_outcomes_6week %>%
    select(
        tpnc_lb,              # Live birth
        gestage,         # Gestational age at birth
        tpnc_place,           # Place of delivery
        tpnc_mode            # Mode of delivery
    ) %>%
    tbl_summary(
        label = list(
            tpnc_lb ~ "Live birth",
            gestage ~ "Gestational age at birth (weeks)",
            tpnc_place ~ "Place of delivery",
            tpnc_mode ~ "Mode of delivery"
        ),
        statistic = list(
            all_continuous() ~ "{median} ({p25}, {p75})",
            all_categorical() ~ "{n} ({p}%)"
        ),
        missing = "no"  # This removes missing values from the display
    ) %>%
    add_n() %>%
    modify_header(label = "**Pregnancy Outcome**") %>%
    modify_caption("**Table 1. Pregnancy Outcomes at 6 Weeks Follow-up**") %>%
    bold_labels()

# Table 2: Infant outcomes at 6 weeks, 14 weeks, & 6 months postpartum --------------
infant_outcomes <- rct_ppw_followup %>%
    select(visit_type, clt_ptid, all_of(starts_with("tpnc_")), -tpnc_date,
           -tpnc_ended, -tpnc_lb, -tpnc_gestage, -tpnc_gestfill, -tpnc_place, 
           -tpnc_mode, all_of(starts_with("io_")), inf_status)

table(infant_outcomes$tpnc_ihiv, infant_outcomes$io_ihiv, useNA = "ifany")
table(infant_outcomes$tpnc_iarv, infant_outcomes$io_iarv, useNA = "ifany")
table(infant_outcomes$tpnc_icorti, infant_outcomes$io_icorti, useNA = "ifany")

table2a <- infant_outcomes %>%
    filter(visit_type == "6 Weeks") %>% 
    select(
        inf_status,  # Infant status,
        tpnc_sex,
        tpnc_birthweight,
        tpnc_birthlength,
        tpnc_twin,
        io_ill,
        io_hosp,
        io_bf,
        io_mf,
        io_ihiv,  # HIV status
        io_iarv,  # ARV status
        io_icorti, 
    ) %>%
    tbl_summary(
        label = list(
            io_ihiv ~ "Infant HIV status",
            io_iarv ~ "Infant given ARV prophylaxis",
            io_icorti ~ "Infant on given cotrimoxazole prophylaxis",
            tpnc_sex ~ "Infant sex",
            tpnc_birthweight ~ "Birth weight (grams)",
            tpnc_birthlength ~ "Birth length (cm)",
            tpnc_twin ~ "Twin birth",
            inf_status ~ "Infant status",
            io_ill ~ "Infant illness prior to today's visit",
            io_hosp ~ "Infant hospitalization prior to today's visit",
            io_bf ~ "Currently breastfeeding",
            io_mf ~ "Fed infant any drink or food other than breastmilk"
        ),
        statistic = list(
            all_continuous() ~ "{median} ({p25}, {p75})",
            all_categorical() ~ "{n} ({p}%)"
        ),
        missing = "no"
    ) %>%
    add_n() %>%
    modify_header(label = "**Infant Outcome**") %>%
    modify_caption("**Table 2a. Infant Outcomes at 6 Weeks Postpartum**") %>%
    bold_labels()

table2b <- infant_outcomes %>%
    filter(visit_type == "14 Weeks") %>% 
    select(
        inf_status,  # Infant status,
        io_ill,
        io_hosp,
        io_bf,
        io_mf,
        io_ihiv,  # HIV status
        io_iarv,  # ARV status
        io_icorti, 
    ) %>%
    tbl_summary(
        label = list(
            io_ihiv ~ "Infant HIV status",
            io_iarv ~ "Infant given ARV prophylaxis",
            io_icorti ~ "Infant on given cotrimoxazole prophylaxis",
            inf_status ~ "Infant status",
            io_ill ~ "Infant illness prior to today's visit",
            io_hosp ~ "Infant hospitalization prior to today's visit",
            io_bf ~ "Currently breastfeeding",
            io_mf ~ "Fed infant any drink or food other than breastmilk"
        ),
        statistic = list(
            all_continuous() ~ "{mean} ({sd})",
            all_categorical() ~ "{n} ({p}%)"
        ),
        missing = "no"
    ) %>%
    add_n() %>%
    modify_header(label = "**Infant Outcome**") %>%
    modify_caption("**Table 2b. Infant Outcomes at 14 Weeks Postpartum**") %>%
    bold_labels()

# table2c <- infant_outcomes %>%
#     filter(visit_type == "6 Months") %>% 
#     select(
#         inf_status,  # Infant status,
#         io_ill,
#         io_hosp,
#         io_bf,
#         io_mf,
#         io_ihiv,  # HIV status
#         io_iarv,  # ARV status
#         io_icorti, 
#     ) %>%
#     tbl_summary(
#         label = list(
#             io_ihiv ~ "Infant HIV status",
#             io_iarv ~ "Infant given ARV prophylaxis",
#             io_icorti ~ "Infant on given cotrimoxazole prophylaxis",
#             inf_status ~ "Infant status",
#             io_ill ~ "Infant illness prior to today's visit",
#             io_hosp ~ "Infant hospitalization prior to today's visit",
#             io_bf ~ "Currently breastfeeding",
#             io_mf ~ "Fed infant any drink or food other than breastmilk"
#         ),
#         statistic = list(
#             all_continuous() ~ "{mean} ({sd})",
#             all_categorical() ~ "{n} ({p}%)"
#         ),
#         missing = "no"
#     ) %>%
#     add_n() %>%
#     modify_header(label = "**Infant Outcome**") %>%
#     modify_caption("**Table 2c. Infant Outcomes at 6 Months Postpartum**") %>%
#     bold_labels()

# Table 3: Primary and secondary clinical outcomes across times--------------
# PHQ9, GAD7, WHOQOL BREF score, Any adverse pregnant outcome (%) 
outcomes <- ppw_rct_df %>%
    select(visit_type, clt_ptid, all_of(starts_with("phq_")), 
           all_of(starts_with("gad7_")), all_of(starts_with("qol_")))
# PHQ9 & GAD7 scores
outcomes <- outcomes %>% 
    mutate(across(c(starts_with("phq_")), 
                  ~ case_when(
                      . == "not at all" ~ 0,
                      . == "several days" ~ 1,
                      . == "more than half the days" ~ 2,
                      . == "nearly every day" ~ 3,
                      TRUE ~ NA_real_
                  ), 
                  .names = "{.col}_number")) %>% 
    mutate(across(starts_with("gad7_"), 
                  ~ case_when(
                      . == "Not at all" ~ 0,
                      . == "Several days" ~ 1,
                      . == "Over half the days" ~ 2,
                      . == "Nearly every day" ~ 3,
                      TRUE ~ NA_real_
                  ), 
                  .names = "{.col}_number")) %>% 
    mutate(phq9_total = rowSums(select(., starts_with("phq_")& ends_with("_number")), na.rm = TRUE),
           gad7_total = rowSums(select(., starts_with("gad7_")& ends_with("_number")), na.rm = TRUE)) %>% 
    mutate(phq9_high = ifelse(phq9_total >= 10, "Yes", "No"),
           gad7_high = ifelse(gad7_total >= 10, "Yes", "No"))

# WHOQOL BREF ======
outcomes <- outcomes %>%
    mutate(qol_rate_number = case_when(
        qol_rate == "Very poor" ~ 1,
        qol_rate == "Poor" ~ 2,
        qol_rate == "Neither poor nor good" ~ 3,
        qol_rate == "Good" ~ 4,
        qol_rate == "Very good" ~ 5,
        TRUE ~ NA_real_  )) %>% 
    mutate(qol_sat_number = case_when(
        qol_sat == "Very dissatisfied" ~ 1,
        qol_sat == "Dissatisfied" ~ 2,
        qol_sat == "Neither satisfied nor dissatisfied" ~ 3,
        qol_sat == "Satisfied" ~ 4,
        qol_sat == "Very satisfied" ~ 5,
        TRUE ~ NA_real_  )) %>% 
    mutate(across(
        .cols = c(qol_enjoy, qol_mean),
                  .fns = ~ case_when(
                      . == "Not at all" ~ 1,
                      . == "A little" ~ 2,
                      . == "A moderate amount" ~ 3,
                      . == "Very much" ~ 4,
                      . == "An extreme amount" ~ 5,
                      TRUE ~ NA_real_
                  ),
                  .names = "{.col}_number"
    )) %>% 
    mutate(across(
        .cols = c(qol_medtreat, qol_pain),
        .fns = ~ case_when(
            . == "Not at all" ~ 5,
            . == "A little" ~ 4,
            . == "A moderate amount" ~ 3,
            . == "Very much" ~ 2,
            . == "An extreme amount" ~ 1,
            TRUE ~ NA_real_
        ),
        .names = "{.col}_number"
    )) %>% 
    mutate(across(
        .cols = c(qol_conc, qol_safe, qol_env),
        .fns = ~ case_when(
            . == "Not at all" ~ 1,
            . == "A little" ~ 2,
            . == "A moderate amount" ~ 3,
            . == "Very much" ~ 4,
            . == "Extremely" ~ 5,
            TRUE ~ NA_real_
        ),
        .names = "{.col}_number"
    )) %>% 
    mutate(across(
        .cols = c(qol_energy, qol_bod, qol_money, qol_info, qol_leis),
        .fns = ~ case_when(
            . == "Not at all" ~ 1,
            . == "A little" ~ 2,
            . == "Moderately" ~ 3,
            . == "Mostly" ~ 4,
            . == "Completely" ~ 5,
            TRUE ~ NA_real_
        ),
        .names = "{.col}_number"
    )) %>% 
    mutate(qol_get_number = case_when(
        qol_get == "Very poor" ~ 1,
        qol_get == "Poor" ~ 2,
        qol_get == "Neither poor nor good" ~ 3,
        qol_get == "Good" ~ 4,
        qol_get == "Very good" ~ 5,
        TRUE ~ NA_real_
    )) %>% 
    mutate(across(
        .cols = c(qol_sleep, qol_perf, qol_cap, qol_self, qol_rel, qol_sex,
                  qol_sup, qol_con_, qol_serv, qol_trans),
        .fns = ~ case_when(
            . == "Very dissatisfied" ~ 1,
            . == "Dissatisfied" ~ 2,
            . == "Neither satisfied nor dissatisfied" ~ 3,
            . == "Satisfied" ~ 4,
            . == "Very satisfied" ~ 5,
            TRUE ~ NA_real_
        ),
        .names = "{.col}_number"
    )) %>% 
        mutate(qol_blue_number = case_when(
            qol_blue == "Never" ~ 5,
            qol_blue == "Seldom" ~ 4,
            qol_blue == "Quite often" ~ 3,
            qol_blue == "Very often" ~ 2,
            qol_blue == "Always" ~ 1,
            TRUE ~ NA_real_
        ))
        
# Calculate WHOQOL BREF scores
outcomes <- outcomes %>% 
    mutate(qol_overall = qol_rate_number + qol_sat_number,
           qol_physical = qol_pain_number + qol_medtreat_number + qol_energy_number + qol_get_number +
               qol_sleep_number + qol_perf_number + qol_cap_number,
           qol_psycho = qol_enjoy_number + qol_mean_number + qol_conc_number + qol_bod_number +
               qol_self_number + qol_blue_number,
           qol_social = qol_rel_number + qol_sex_number + qol_sup_number,
           qol_environ = qol_safe_number + qol_env_number + qol_money_number + qol_info_number +
               qol_leis_number + qol_con__number + qol_trans_number + qol_serv_number
    )

#scoring instruction: https://depts.washington.edu/seaqol/docs/WHOQOL-BREF%20and%20Scoring%20Instructions.pdf

# Define item vectors for each domain
physical_items <- c("qol_pain_number", "qol_medtreat_number", "qol_energy_number",
                    "qol_get_number", "qol_sleep_number", "qol_perf_number", "qol_cap_number")

psych_items <- c("qol_enjoy_number", "qol_mean_number", "qol_conc_number",
                 "qol_bod_number", "qol_self_number", "qol_blue_number")

social_items <- c("qol_rel_number", "qol_sex_number", "qol_sup_number")

env_items <- c("qol_safe_number", "qol_env_number", "qol_money_number",
               "qol_info_number", "qol_leis_number", "qol_con__number",
               "qol_trans_number", "qol_serv_number")

# Impute physical domain
outcomes <- outcomes %>%
    rowwise() %>%
    mutate(
        physical_non_missing = sum(!is.na(c_across(all_of(physical_items)))),
        qol_physical = ifelse(
            physical_non_missing >= 6,
            sum(replace_na(c_across(all_of(physical_items)),
                           mean(c_across(all_of(physical_items)), na.rm = TRUE)), na.rm = TRUE),
            NA_real_
        )
    ) %>%
    ungroup()

outcomes <- outcomes %>%
    rowwise() %>%
    mutate(
        env_non_missing = sum(!is.na(c_across(all_of(env_items)))),
        qol_environ = ifelse(
            env_non_missing >= 7,
            sum(replace_na(c_across(all_of(env_items)),
                           mean(c_across(all_of(env_items)), na.rm = TRUE)), na.rm = TRUE),
            NA_real_
        )
    ) %>%
    ungroup()

outcomes <- outcomes %>%
    rowwise() %>%
    mutate(
        qol_psycho = ifelse(any(is.na(c_across(all_of(psych_items))), na.rm = TRUE), NA_real_,
                            sum(c_across(all_of(psych_items)))),
        qol_social = ifelse(any(is.na(c_across(all_of(social_items))), na.rm = TRUE), NA_real_,
                            sum(c_across(all_of(social_items))))
    ) %>%
    ungroup()

outcomes <- outcomes %>%
    mutate(
        qol_physical_scaled = ((qol_physical - 7) / 28) * 100,
        qol_psycho_scaled   = ((qol_psycho - 6) / 24) * 100,
        qol_social_scaled   = ((qol_social - 3) / 12) * 100,
        qol_environ_scaled  = ((qol_environ - 8) / 32) * 100,
        qol_overall_scaled = as.numeric(((qol_overall - 2) / 8) * 100)
    )

# Adverse pregnant outcomes
## here we included: miscarriage or stillbirth, death(infant or maternal), hospitalization
adverse_outcomes <- ppw_sae_df %>%
    select(visit_type, record_id, arm, ae_yn, ae_cat, ae_preglosssp, ae_type___1, ae_type___2,
           ae_type___3, ae_type___4, ae_type___5) %>% 
    rename(
        maternal_death = ae_type___1,
        infant_death = ae_type___2,
        maternal_hospitalization = ae_type___3,
        infant_hospitalization = ae_type___4,
        loss = ae_type___5
    ) %>%
    mutate(
        stillbirth = if_else(ae_preglosssp == "Stillbirth (>20wks gestation)", TRUE, FALSE, missing = FALSE),
        miscarriage = if_else(ae_preglosssp == "Miscarriage (< =20wks gestation)", TRUE, FALSE, missing = FALSE),
        maternal_death = if_else(maternal_death == "Checked", TRUE, FALSE),
        infant_death = if_else(infant_death == "Checked", TRUE, FALSE),
        maternal_hospitalization = if_else(maternal_hospitalization == "Checked", TRUE, FALSE),
        infant_hospitalization = if_else(infant_hospitalization == "Checked", TRUE, FALSE)
    )

outcomes <- outcomes %>% left_join(adverse_outcomes, 
                                   by = c("visit_type", 
                                          "clt_ptid" = "record_id")) 

outcomes <- outcomes %>%
    mutate(visit_type = factor(visit_type,
                               levels = c("Enrollment", "6 Weeks", "14 Weeks", "6 Months")))
table3 <- outcomes %>%
    select(visit_type, phq9_total, phq9_high, gad7_total, gad7_high,
           qol_overall_scaled, qol_physical_scaled, qol_psycho_scaled,
           qol_social_scaled, qol_environ_scaled,
           maternal_death, infant_death, maternal_hospitalization,
           infant_hospitalization, stillbirth, miscarriage) %>%
    tbl_summary(
        by = visit_type,
        type = list(
            phq9_total ~ "continuous",
            gad7_total ~ "continuous",
            qol_overall_scaled ~ "continuous",
            qol_physical_scaled ~ "continuous",
            qol_psycho_scaled ~ "continuous",
            qol_social_scaled ~ "continuous",
            qol_environ_scaled ~ "continuous"
        ),
        label = list(
            phq9_total ~ "PHQ-9 Total Score",
            phq9_high ~ "PHQ-9 High Score (>=10)",
            gad7_total ~ "GAD-7 Total Score",
            gad7_high ~ "GAD-7 High Score (>=10)",
            qol_overall_scaled ~ "WHOQOL Overall Score (Scaled)",
            qol_physical_scaled ~ "WHOQOL Physical Domain Score (Scaled)",
            qol_psycho_scaled ~ "WHOQOL Psychological Domain Score (Scaled)",
            qol_social_scaled ~ "WHOQOL Social Domain Score (Scaled)",
            qol_environ_scaled ~ "WHOQOL Environmental Domain Score (Scaled)",
            maternal_death ~ "Maternal Death",
            infant_death ~ "Infant Death",
            maternal_hospitalization ~ "Maternal Hospitalization",
            infant_hospitalization ~ "Infant Hospitalization",
            stillbirth ~ "Stillbirth",
            miscarriage ~ "Miscarriage"
        ),
        statistic = list(
            all_continuous() ~ "{median} ({p25}, {p75})",
            all_categorical() ~ "{n} ({p}%)"
        ),
        missing = "no"
    ) %>%
    add_n() %>%
    modify_header(label = "**Clinical Outcome**") %>%
    modify_caption("**Table 3. Primary and Secondary Clinical Outcomes Across Visits**") %>%
    bold_labels()

#Table 4: Adverse and severe adverse events --------------------
sae <- ppw_sae_df %>%
    select(visit_type, record_id, arm, ae_yn, ae_datereport, ae_cat, ae_preglosssp,
           ae_multpreg_loss, ae_pregloss_mult, ae_dateonset,
           ae_narrative, ae_type___1, ae_type___2, ae_type___3, ae_type___4, ae_type___5,
           ae_ideath_date, ae_matdeath_date, ae_relation, ae_rational, 
           ae_resolutiondate, ae_outcome) %>% 
    rename(
        maternal_death = ae_type___1,
        infant_death = ae_type___2,
        maternal_hospitalization = ae_type___3,
        infant_hospitalization = ae_type___4,
        loss = ae_type___5
    ) %>%
    mutate(
        stillbirth = if_else(ae_preglosssp == "Stillbirth (>20wks gestation)", TRUE, FALSE, missing = FALSE),
        miscarriage = if_else(ae_preglosssp == "Miscarriage (< =20wks gestation)", TRUE, FALSE, missing = FALSE),
        maternal_death = if_else(maternal_death == "Checked", TRUE, FALSE),
        infant_death = if_else(infant_death == "Checked", TRUE, FALSE),
        maternal_hospitalization = if_else(maternal_hospitalization == "Checked", TRUE, FALSE),
        infant_hospitalization = if_else(infant_hospitalization == "Checked", TRUE, FALSE)
    )

enrollment_date <- ppw_rct_df %>%
    filter(visit_type == "Enrollment") %>%
    select(clt_ptid, clt_date) 

sae <- sae %>%
    left_join(enrollment_date, by = c("record_id" = "clt_ptid"))

sae_flags <- c("maternal_death", "infant_death", "maternal_hospitalization",
               "infant_hospitalization", "stillbirth", "miscarriage")

sae <- sae %>%
    mutate(across(all_of(sae_flags), ~ . %in% c(TRUE, "TRUE", "true", 1, "1", "Yes", "yes")))

sae_filtered <- sae %>%
    filter(if_any(all_of(sae_flags), ~ . == TRUE)) %>%
    rowwise() %>%
    mutate(
        SAE_type = paste(sae_flags[c_across(all_of(sae_flags))], collapse = "; "),
        sae_number = cur_group_id(),  # Sequential ID
        onset_since_randomization = round(as.numeric(difftime(ae_dateonset, 
                                                              clt_date, units = "days")),0)  
    ) %>%
    ungroup()

# Create a summary table for SAEs
sae_table <- sae_filtered %>%
    select(sae_number, record_id, SAE_type, ae_dateonset, ae_datereport,
           onset_since_randomization, ae_outcome, ae_relation) %>% 
    kable(caption = "**Adverse and Severe Adverse Events Across Visits**")


#Table 5: Psychosocial correlates across visits --------------------
#1. reducing tension checklist
rtc <- ppw_rct_df %>%
    select(visit_type, clt_ptid, starts_with("rtc_")) %>%
    mutate(across(
        .cols = c(starts_with("rtc_")),
        .fns = ~ case_when(
            . == "Not at all" ~ 0,
            . == "A little/rarely (once or twice in the past month)" ~ 1,
            . == "Sometimes (about once a week)" ~ 2,
            . == "Most of the time (a few times per week)" ~ 3,
            . == "All the time (almost every day)" ~ 4,
            TRUE ~ NA_real_
        ),
        .names = "{.col}_number"
    ))
rtc <- rtc %>%
    mutate(rtc_total = rowSums(select(., starts_with("rtc_") & ends_with("_number")), na.rm = TRUE)) 

#2. parenting stress (6w & 6m)
#https://www.corc.uk.net/media/2764/parent-stress-scale-fillable-pdf.pdf
parenting_stress <- ppw_rct_df %>%
    select(visit_type, clt_ptid, starts_with("pstress_")) %>%
    mutate(across(
        .cols = c(starts_with("pstress_")),
        .fns = ~ case_when(
            . == "Strongly disagree" ~ 1,
            . == "Disagree" ~ 1,
            . == "Undecided" ~ 2,
            . == "Agree" ~ 3,
            . == "Strongly agree" ~ 4,
            TRUE ~ NA_real_
        ),
        .names = "{.col}_number"
    )) %>%
    mutate(ps_total = rowSums(select(., starts_with("pstress_") & ends_with("_number")), na.rm = TRUE))

#3. IPV
#cutoff: 10
ipv <- ppw_rct_df %>%
    select(visit_type, clt_ptid, starts_with("hit_")) %>%
    mutate(across(
        .cols = c(starts_with("hit_")),
        .fns = ~ case_when(
            . == "Never (Hakuna) [onge]" ~ 0,
            . == "Rarely (Vigumu) [matin]" ~ 1,
            . == "Sometimes (Mara nyingine) [kadichiel]" ~ 2,
            . == "Fairly Often (Karibu mara mingi) [thothne]" ~ 3,
            . == "Frequently (Mara mingi) [mang'eny]" ~ 4,
            TRUE ~ NA_real_
        ),
        .names = "{.col}_number"
    )) %>%
    mutate(hit_total = rowSums(select(., starts_with("hit_") & ends_with("_number")), na.rm = TRUE)) %>% 
    mutate(hit_high = ifelse(hit_total >= 10, "Yes", "No"))

#4. Social suppot (baseline & 14w)
#https://emerge.ucsd.edu/r_1mq2f0ksb7qyidj/
#cutoff: 3~8 poor, 9-11 moderate, 12-14 strong
social_support <- ppw_rct_df %>%
    select(visit_type, clt_ptid, starts_with("ss_")) %>%
    mutate(ss_close_number = case_when(
            ss_close == "None" ~ 1,
            ss_close == "1-2" ~ 2,
            ss_close == "3-5" ~ 3,
            ss_close == "5+" ~ 4,
            TRUE ~ NA_real_
        )) %>% 
    mutate(ss_interest_number = case_when(
            ss_interest == "None" ~ 1,
            ss_interest == "Little" ~ 2,
            ss_interest == "Uncertain" ~ 3,
            ss_interest == "Some" ~ 4,
            ss_interest == "A lot" ~ 5,
            TRUE ~ NA_real_
        )) %>%
    mutate(ss_help_number = case_when(
            ss_help == "very difficult" ~ 1,
            ss_help == "difficult" ~ 2,
            ss_help == "possible" ~ 3,
            ss_help == "easy" ~ 4,
            TRUE ~ NA_real_
        )) %>%
    mutate(ss_total = rowSums(select(., starts_with("ss_") & ends_with("_number")), na.rm = TRUE)) %>% 
    mutate(ss_cat = case_when(
            ss_total >= 3 & ss_total <= 8 ~ "Poor",
            ss_total >= 9 & ss_total <= 11 ~ "Moderate",
            ss_total >= 12 & ss_total <= 14 ~ "Strong",
            TRUE ~ NA_character_
        ))

#5. ACES (14w)
aces <- ppw_rct_df %>%
    select(visit_type, clt_ptid, starts_with("ace_")) %>%
    mutate(across(
        .cols = c(starts_with("ace_")),
        .fns = ~ case_when(
            . == "Yes (Ndio)" ~ 1,
            . == "No (La)" ~ 0,
            . == "No answer (Hakuna jibu)" ~ NA,
            TRUE ~ NA_real_
        ),
        .names = "{.col}_number"
    )) %>%
    mutate(ace_total = round(rowSums(select(., starts_with("ace_") & 
                                                ends_with("_number")), na.rm = TRUE)),0)

# Combine all psychosocial data into one table
psychosocial_data <- rtc %>%
    left_join(parenting_stress, by = c("visit_type", "clt_ptid")) %>%
    left_join(ipv, by = c("visit_type", "clt_ptid")) %>%
    left_join(social_support, by = c("visit_type", "clt_ptid")) %>%
    left_join(aces, by = c("visit_type", "clt_ptid")) 

psychosocial_data <- psychosocial_data %>%
    mutate(visit_type = factor(visit_type,
                               levels = c("Enrollment", "6 Weeks", 
                                          "14 Weeks", "6 Months")))

# Create a summary table for psychosocial correlates
table4 <- psychosocial_data %>%
    select(visit_type, rtc_total, ps_total, hit_total, hit_high,
           ss_total, ss_cat, ace_total) %>%
    tbl_summary(
        by = visit_type,
        type = list(
            all_continuous() ~ "continuous",
            all_categorical() ~ "categorical",
            ace_total ~ "continuous"
        ),
        label = list(
            rtc_total ~ "Reducing Tension Checklist (Score)",
            ps_total ~ "Parenting Stress (Score)\n (6 weeks and 6 months)",
            hit_total ~ "Intimate Partner Violence (Score)",
            hit_high ~ "Intimate Partner Violence High Score (>=10)",
            ss_total ~ "Social Support (Score)\n (Baseline and 14 weeks)",
            ss_cat ~ "Social Support Category",
            ace_total ~ "Adverse Childhood Experiences (Number)\n (14 weeks)"
        ),
        digits = list(
            all_continuous() ~ 0,
            all_categorical() ~ 0
        ),
        statistic = list(
            all_continuous() ~ "{median} ({p25}, {p75})",
            all_categorical() ~ "{n} ({p}%)"
        ),
        missing = "no"
    ) %>%
    add_n() %>%
    modify_header(label = "**Psychosocial Correlates**") %>%
    modify_caption("**Table 4. Psychosocial Correlates Across Visits**") %>%
    bold_labels()

#Table 6: Mental Health Service Utilization========
su <- ppw_rct_df %>% 
    select(visit_type, clt_ptid, starts_with("su_")) 

su <- su %>%
    mutate(
        # Recode su_many: "None" to "0" and make continuous
        su_many = case_when(
            su_many == "None" ~ "0",
            TRUE ~ as.character(su_many)
        ),
        su_many = as.numeric(su_many),
        
        # Recode PM restart variables from Checked/Unchecked to Yes/No
        su_pmrestart___0 = case_when(
            su_pmrestart___0 == "Checked" ~ "Yes",
            su_pmrestart___0 == "Unchecked" ~ "No",
            TRUE ~ su_pmrestart___0
        ),
        su_pmrestart___1 = case_when(
            su_pmrestart___1 == "Checked" ~ "Yes",
            su_pmrestart___1 == "Unchecked" ~ "No", 
            TRUE ~ su_pmrestart___1
        ),
        su_pmrestart___2 = case_when(
            su_pmrestart___2 == "Checked" ~ "Yes",
            su_pmrestart___2 == "Unchecked" ~ "No",
            TRUE ~ su_pmrestart___2
        ),
        
        # Ensure PM restart variables are factors
        across(c(su_pmrestart___0, su_pmrestart___1, su_pmrestart___2), as.factor)
    )


table6 <- su %>%
    # Ensure visit_type is a factor with proper ordering
    mutate(visit_type = factor(visit_type, 
                               levels = c("Enrollment", "6 Weeks", "14 Weeks", "6 Months"))) %>%
    select(visit_type, su_phq2, su_gap2, su_phq9, su_gad7, 
           su_access, su_referral, su_tele) %>%
    tbl_summary(
        by = visit_type,
        statistic = list(all_categorical() ~ "{n} ({p}%)"),
        digits = list(all_categorical() ~ c(0, 1)),
        missing = "no",
        label = list(
            su_phq2 ~ "Screened by PHQ-2",
            su_gap2 ~ "Screened by GAD-2", 
            su_phq9 ~ "Screened by PHQ-9",
            su_gad7 ~ "Screened by GAD-7",
            su_access ~ "Accessed Mental Health Providers",
            su_referral ~ "Mental Health Referral Made",
            su_tele ~ "Used Telepsychiatry"
        )
    ) %>%
    add_overall() %>%
    modify_header(label ~ "**Variable**") %>%
    modify_spanning_header(c("stat_1", "stat_2", "stat_3", "stat_4") ~ "**Visit Type**") %>%
    modify_caption("**Screening and Service Utilization by Visit Type**") %>%
    bold_labels()

table7 <- su %>%
    filter(visit_type != "Enrollment") %>%
    mutate(visit_type = factor(visit_type, 
                               levels = c("6 Weeks", "14 Weeks", "6 Months"))) %>%
    select(visit_type, su_pmp, su_many, su_pmrestart___0, su_pmrestart___1, su_pmrestart___2) %>%
    tbl_summary(
        by = visit_type,
        statistic = list(
            all_continuous() ~ "{mean} ({sd})",
            all_categorical() ~ "{n} ({p}%)"
        ),
        digits = list(
            all_continuous() ~ c(1, 1),
            all_categorical() ~ c(0, 1)
        ),
        missing = "no",
        label = list(
            su_pmp ~ "PM+ Participation",
            su_many ~ "PM+ Sessions Completed",
            su_pmrestart___0 ~ "Stopped PM+",
            su_pmrestart___1 ~ "Restarted PM+", 
            su_pmrestart___2 ~ "No PM+ Changes"
        )
    ) %>%
    add_overall() %>%
    modify_header(label ~ "**Variable**") %>%
    modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Visit Type**") %>%
    modify_caption("**PM+ Program Participation (Post-Enrollment Visits Only)**") %>%
    bold_labels()

#Table for tracking follow-up visits ------------------
ppw_date_track <- ppw_rct_df %>%
    mutate(
        clt_date = as.Date(clt_date),
        med_pre_edd = as.Date(med_pre_edd),
        tpnc_date = as.Date(tpnc_date)
    ) %>%
    # Create participant ID if needed
    group_by(clt_study_site) %>%
    ungroup()

enrollment_data <- ppw_date_track %>%
    filter(grepl("Enrollment", clt_visit)) %>%
    select(clt_ptid, clt_study_site, enrollment_date = clt_date, med_pre_edd)
visit_6wk <- ppw_date_track %>%
    filter(grepl("6 weeks", clt_visit)) %>%
    select(clt_ptid, visit_6wk_date = clt_date, tpnc_date)
visit_14wk <- ppw_date_track %>%
    filter(grepl("14 weeks", clt_visit)) %>%
    select(clt_ptid, visit_14wk_date = clt_date)
visit_6mo <- ppw_date_track %>%
    filter(grepl("6 months", clt_visit)) %>%
    select(clt_ptid, visit_6mo_date = clt_date)

all_data <- enrollment_data %>%
    left_join(visit_6wk, by = "clt_ptid") %>%
    left_join(visit_14wk, by = "clt_ptid") %>%
    left_join(visit_6mo, by = "clt_ptid") %>%
    mutate(
        delivery_date = coalesce(tpnc_date, med_pre_edd)
    )

summary_by_site <- all_data %>%
    group_by(clt_study_site) %>%
    summarise(
        enrolled = n(),
        completed_6wk = sum(!is.na(visit_6wk_date)),
        completed_14wk = sum(!is.na(visit_14wk_date)),
        completed_6mo = sum(!is.na(visit_6mo_date)),
        .groups = 'drop'
    )

total_row <- all_data %>%
    summarise(
        clt_study_site = "TOTAL",
        enrolled = n(),
        completed_6wk = sum(!is.na(visit_6wk_date)),
        completed_14wk = sum(!is.na(visit_14wk_date)),
        completed_6mo = sum(!is.na(visit_6mo_date))
    )

final_summary <- bind_rows(total_row, summary_by_site)

current_date <- Sys.Date()

due_for_visits <- all_data %>%
    filter(!is.na(delivery_date)) %>%
    mutate(
        # Simple eligibility: X weeks since delivery
        due_6wk = current_date >= (delivery_date + 42) & is.na(visit_6wk_date),
        due_14wk = current_date >= (delivery_date + 98) & is.na(visit_14wk_date),
        due_6mo = current_date >= (delivery_date + 182) & is.na(visit_6mo_date)
    )

needs_visits <- due_for_visits %>%
    group_by(clt_study_site) %>%
    summarise(
        needs_6wk = sum(due_6wk, na.rm = TRUE),
        needs_14wk = sum(due_14wk, na.rm = TRUE),
        needs_6mo = sum(due_6mo, na.rm = TRUE),
        .groups = 'drop'
    )

needs_total <- due_for_visits %>%
    summarise(
        clt_study_site = "TOTAL",
        needs_6wk = sum(due_6wk, na.rm = TRUE),
        needs_14wk = sum(due_14wk, na.rm = TRUE),
        needs_6mo = sum(due_6mo, na.rm = TRUE)
    )

needs_summary <- bind_rows(needs_total, needs_visits)

combined_summary <- final_summary %>%
    left_join(needs_summary, by = "clt_study_site") %>%
    select(
        Site = clt_study_site,
        Enrolled = enrolled,
        `6wk_Completed` = completed_6wk,
        `6wk_Needs` = needs_6wk,
        `14wk_Completed` = completed_14wk,
        `14wk_Needs` = needs_14wk,
        `6mo_Completed` = completed_6mo,
        `6mo_Needs` = needs_6mo
    ) %>%
    # Replace NA with 0 for cleaner display
    mutate(across(ends_with("_Needs"), ~replace_na(.x, 0)))

combined_summary