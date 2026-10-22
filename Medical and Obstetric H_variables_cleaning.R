# Header ------------------------------------------------------------------

# Author(s): Dowaga
# Date: August 18, 2026
# This is a script to prepare a cleaned Medical and Obstetric History dataset 
# that's ready for analysis, manuscript writing, and sharing 

# Setup ------------------------------------------------------------------------
rm(list = ls())
# Reference source codes & other dependencies:
source("Dependencies.R")
source("data_import.R")
source("DataTeam_ipmh.R")


###############################################################
# 1. only keep the databases we need
###############################################################
rm(list = setdiff(ls(), c("ppw_rct_df", "ipmh_filepath",
                          "data_freeze")))


#============================================================================
# 2. Select only the relevant columns
#============================================================================
medical_raw <- ppw_rct_df %>%
    filter(grepl("Enrollment ", redcap_event_name)) %>% 
    select(
        record_id, clt_date, 
        starts_with("med_"), 
        medical_and_obstetric_history_complete
    )

#===============================================================================
# 3. DATE VARIABLES to date
#===============================================================================
medical_raw <- medical_raw %>%
    mutate(
        med_lmp     = ymd(med_lmp),
        med_pre_edd = ymd(med_pre_edd)
    )


# 3.1 DATA QUALITY CHECKS: LMP

dq_lmp_missing <- medical_raw %>%
    filter(med_lmp_know == "Yes", is.na(med_lmp)) %>%
    select(record_id, med_lmp_know, med_lmp)

dq_lmp_provided <- medical_raw %>%
    filter(med_lmp_know == "No", !is.na(med_lmp)) %>%
    select(record_id, med_lmp_know, med_lmp)

#===============================================================================
# 4. PREVIOUS PREGNANCY & OUTCOMES
#===============================================================================
medical_raw <- medical_raw %>%
    mutate(
        med_pre_preg_num = case_when(
            med_pre_preg == "Yes" ~ 1,
            med_pre_preg == "No"  ~ 0,
            TRUE                  ~ NA_real_
        ),
        # Recode -2 (no answer) to NA before summing pregnancy outcomes
        across(
            c(med_num_livebirth, med_num_stillbirth, med_num_sab, med_num_eab),
            ~ na_if(., -2)
        )
    ) %>%
    mutate(
        preg_outcomes_sum = coalesce(med_num_livebirth, 0) +
            coalesce(med_num_stillbirth, 0) +
            coalesce(med_num_sab, 0) +
            coalesce(med_num_eab, 0)
    )


# 4.1 DQ CHECKS: PREGNANCY OUTCOMES
preg_outcome_flags <- medical_raw %>%
    filter(
        !is.na(med_num_preg),
        !is.na(preg_outcomes_sum),
        preg_outcomes_sum < 0 | preg_outcomes_sum > (med_num_preg - 1)
    ) %>%
    select(record_id, med_num_preg, med_num_livebirth, med_num_stillbirth,
           med_num_sab, med_num_eab, preg_outcomes_sum)


#===============================================================================
# 5. DERIVED PREGNANCY VARIABLES
#===============================================================================
medical_raw <- medical_raw %>%
    mutate(
        med_preg_multi = case_when(
            med_num_preg > 1  ~ 1,
            med_num_preg <= 1 ~ 0,
            TRUE              ~ NA_real_
        ),
        had_stillbirth = case_when(
            med_num_stillbirth > 0  ~ 1,
            med_num_stillbirth == 0 ~ 0,
            TRUE                    ~ NA_real_
        ),
        stillbirth_count = case_when(
            med_num_stillbirth <= 0 ~ NA_real_,
            TRUE                    ~ med_num_stillbirth
        )
    )


#===============================================================================
# 6. PROBLEMS DURING CURRENT PREGNANCY (Recoding Checkboxes)
#===============================================================================
problem_labels <- c("Checked" = 1, "Unchecked" = 0)

current_preg_vars <- c(
    paste0("med_dx_currentpreg___", 1:10),
    "med_dx_currentpreg____2"
)

medical_raw <- medical_raw %>%
    mutate(
        across(all_of(current_preg_vars), ~ recode(., !!!problem_labels))
    )

#===============================================================================
# 7. DERIVE ANC FIRST TRIMESTER
#===============================================================================
medical_raw <- medical_raw %>%
    mutate(
        anc_first_trimester = case_when(
            med_pre_firstanc == "First Trimester (0-13 weeks)"   ~ "First Trimester",
            med_pre_firstanc == "Second Trimester (14-26 weeks)" ~ "Second Trimester",
            med_pre_firstanc == "Third Trimester (27-40 Weeks)"  ~ "Third Trimester",
            TRUE ~ NA_character_
        )
    )

#===============================================================================
# 8. CURRENT GESTATIONAL AGE & OTHER DETERMINATIONS
#===============================================================================
medical_raw <- medical_raw %>% 
    mutate(
        current_ga = as.numeric(med_pre_gestage_current),
        other_ga_determination = case_when(
            str_detect(str_to_lower(med_pre_gestage_other), "palpat") ~ "Palpation",
            str_detect(str_to_lower(med_pre_gestage_other), "hcw|health care worker|nurse") ~ "Health Care Worker Estimation",
            str_detect(str_to_lower(med_pre_gestage_other), "^\\d+(\\.\\d+)?$") ~ "Numeric GA entered",
            TRUE ~ str_squish(med_pre_gestage_other)
        )
    )


#===============================================================================
# 9. PAST DIAGNOSIS VARIABLES
#===============================================================================
past_diag_vars <- c(
    paste0("med_pastdiag___", 1:8),
    "med_pastdiag____2"
)

medical_raw <- medical_raw %>%
    mutate(
        across(all_of(past_diag_vars), ~ recode(., !!!diagnosis_labels))
    )


# ==============================================================================
# 10. SELECT ANALYSIS-READY & SHARING DATASET
# ==============================================================================
medical_clean <- medical_raw %>%
    select(
        # Identifiers & Core Metadata
        record_id,
        
        # Demographic/Timeline fields
        clt_date,
        med_lmp,
        med_pre_edd,
        med_lmp_know,
        
        # Pregnancy History Metrics
        med_pre_preg_num,
        med_num_preg,
        med_preg_multi,
        med_num_livebirth,
        had_stillbirth,
        stillbirth_count,
        med_num_sab,
        med_num_eab,
        med_num_child,
        
        # Current Pregnancy & ANC Factors
        current_ga,
        anc_first_trimester,
        other_ga_determination,
        
        # Checkbox Conditions (Current & Past Diagnoses)
        all_of(current_preg_vars),
        all_of(past_diag_vars)
    )

# Quick check of the final analytic data frame structure
glimpse(medical_clean)



    

      