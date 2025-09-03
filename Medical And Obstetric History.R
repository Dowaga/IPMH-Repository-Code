# Header ------------------------------------------------------------------

# Author(s): Owaga
# Date: March 20, 2025
# Medical And Obstetric History

# Setup ------------------------------------------------------------------------
# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")

mo_history <- ppw_rct_df %>% 
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
    ) %>% 
    filter(visit_type == "Enrollment") %>% 
    select(record_id, starts_with("med_"), 
           medical_and_obstetric_history_complete)

# Pregnancy history and medical history recoding
diagnosed_labels <-c(
    "Checked" = 1,
    "Unchecked" = 0)

# Recode Problem and Diagnosis during this prgnancy variables
mo_history <- mo_history %>%
    mutate(across(c(med_pastdiag___1, med_pastdiag___2, med_pastdiag___3,
                    med_pastdiag___4, med_pastdiag___5, med_pastdiag___6,
                    med_pastdiag___7, med_pastdiag___8, med_pastdiag____2),
                  ~ recode(., !!!diagnosed_labels)),
           across(c(med_dx_currentpreg___1, med_dx_currentpreg___2, med_dx_currentpreg___3,
                    med_dx_currentpreg___4, med_dx_currentpreg___5, med_dx_currentpreg___6,
                    med_dx_currentpreg___7, med_dx_currentpreg___8, med_dx_currentpreg___9,
                    med_dx_currentpreg___10, med_dx_currentpreg___2),
                  ~ recode(., !!!diagnosed_labels)))

mo_history <- mo_history %>% 
    mutate(med_pastdiag_othrspec = case_when(
        str_detect(med_pastdiag_othrspec, "(?i)anemia|Anemia in pregnancy") ~ "Anaemia",
        str_detect(med_pastdiag_othrspec, "(?i)asthmatic") ~ "Asthma",
        str_detect(med_pastdiag_othrspec, "(?i)pneumonia and ulcers") ~ "Ulcers",
        TRUE ~ med_pastdiag_othrspec  # Keep other values unchanged
    ),
    med_preg_multi = if_else(med_num_preg > 1, 1, 0),
    had_stillbirth = case_when(
        med_num_stillbirth > 0 ~ 1,         # Had one or more stillbirths
        med_num_stillbirth == 0 ~ 0,        # Had none
        med_num_stillbirth == -2 ~ NA_real_ # No answer / missing
    ),
    had_stillbirth = case_when(
        med_preg_multi == 1 & is.na(had_stillbirth) ~ 0,
        TRUE ~ had_stillbirth
    ),
    stillbirth_count = case_when(
        med_num_stillbirth %in% c(0, -2) ~ NA_real_,
        TRUE ~ med_num_stillbirth
    ))



moh_summary <- mo_history %>% 
tbl_summary(sort = list(all_categorical() ~ "frequency"),  # Sort categorical levels by frequency in descending order
    include = c(med_pre_preg, med_num_preg, med_preg_multi, med_num_livebirth, had_stillbirth,
                stillbirth_count, med_num_child, med_dx_currentpreg___1, 
                med_dx_currentpreg___2, med_dx_currentpreg___3,
                med_dx_currentpreg___4, med_dx_currentpreg___5, med_dx_currentpreg___6,
                med_dx_currentpreg___7, med_dx_currentpreg___8, med_dx_currentpreg___9,
                med_dx_currentpreg___10, med_dx_currentpreg___2, med_pre_firstanc, med_pre_gestage_current, 
                        med_pastdiag___1, med_pastdiag___2, med_pastdiag___3,
                        med_pastdiag___4, med_pastdiag___5, #med_pastdiag___6,
                        med_pastdiag___7, med_pastdiag___8, med_pastdiag_othrspec),
            label = list(med_pre_preg ~ "Been pregnant before",
                         med_num_preg ~ "Total number of pregnancies",
                         med_num_livebirth ~ "Number of babies born alive",
                         stillbirth_count ~ "Number of stillbirths",
                         had_stillbirth ~ "Experienced Stillbirth",
                         med_num_child ~ "Number of living biological children",
                         med_pre_gestage_current ~ "Current Gestational age: Weeks",
                         med_pre_firstanc ~ "First ANC during this pregnancy",
                         med_pastdiag___1 ~ "High blood pressure",
                         med_pastdiag___2 ~ "HIV",
                         med_pastdiag___3 ~ "Other STIs",
                         med_pastdiag___4 ~ "TB",
                         med_pastdiag___5 ~ "Diabetes",
                         med_pastdiag___7 ~ "Low mood",
                         med_pastdiag___8 ~ "Other",
                         med_pastdiag_othrspec ~ "Other dignosis during this pregnancy",
                         med_dx_currentpreg___1 ~ "None",
                         med_dx_currentpreg___2 ~ "Anemia",
                         med_dx_currentpreg___3	~ "Malaria",
                         med_dx_currentpreg___4 ~ "High blood pressure",
                         med_dx_currentpreg___5	~ "Infection",
                         med_dx_currentpreg___6	~ "Low mood",
                         med_dx_currentpreg___7	~ "Antepartum hemorrhage",
                         med_dx_currentpreg___8	~ "Miscarriage",
                         med_dx_currentpreg___9	~ "Diabetes",
                         med_dx_currentpreg___10 ~ "Other",
                         med_preg_multi ~ "Multiparous"),
            missing = "no",
            digits = list(all_continuous() ~ 1), 
            type = list(med_pre_gestage_current ~ "continuous",
                        med_num_child ~ "continuous",
                        stillbirth_count ~ "continuous"),
            statistic = list(all_continuous() ~ "{median} [IQR: {p25}, {p75}]")
) %>% 
    modify_header(label = "**Variable**") %>%
    bold_labels() %>% 
    italicize_levels() %>% 
    add_n()

moh_summary


