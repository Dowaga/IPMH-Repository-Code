# Header ------------------------------------------------------------------

# Author(s): Dowaga
# Date: August 05, 2026
# This is a script to prepare a cleaned screening and enrollment dataset 
# that's ready for analysis, manuscript writing, and sharing 

# Setup ------------------------------------------------------------------------
rm(list = ls())
# Reference source codes & other dependencies:
source("Dependencies.R")
source("data_import.R")
source("DataTeam_ipmh.R")

# Set up data freeze time for this report
data_freeze <- as.Date("2026-08-17") 

###############################################################
# 1. only keep the databases we need
###############################################################
rm(list = setdiff(ls(), c("screening_consent_df", "ipmh_filepath",
                          "data_freeze")))


##############################################################
# 2. Select Raw Dataset variables for cleaning
##############################################################

screening_raw_df <- screening_consent_df %>% 
    select(record_id, study_site, latest_consent, anc_num, rct_know_dob,
           rct_dob, rct_know_age, rct_age, rct_pregnant, rct_know_last_mpdate, 
           rct_last_menstrual_date, crt_certain_date, rct_last_mentrual_month,
           rct_told_delivery_date, rct_told_delivery_date, rct_estimated_due_date,
           rct_see_mch_booklet, rct_booklet_edd_recorded, rct_mch_booklet_edd,
           rct_edd_source___1, rct_edd_source___2, rct_edd_source___3, 
           rct_edd_source___4, rct_other_edd_cource, rct_ultrasound_date,
           rct_phq2_1, rct_phq2_2, rct_gaq2_1, rct_gaq2_2, rct_harm_thought,
           rct_risk, rct_memory_problem, rct_aud_hallucinations, 
           rct_vis_hallucinations, rct_paranoia, rct_delusions, 
           rct_eligible_gestation, rct_eligible_phq2, rct_eligible_gad2,
           rct_eligible_harm, rct_eligible, rct_enrolling, rct_decline_reason, 
           rct_other_reasons, ipmh_screening_crf_complete, consent_date, consent_date_v2)

# Unique IDs
anyDuplicated(screening_raw_df$record_id)

####################################################################
# 2. Create Study Arm in the Dataset
####################################################################
screening_raw_df  <- screening_raw_df %>%
    # Study Facility code extraction
    mutate(
        facility_code = as.numeric(str_sub(study_site, 1, 2)) # first two characters
    ) %>%
    # Study arm
    mutate(
        arm = case_when(
            facility_code %in% c(2, 5, 6, 8, 11, 14, 15, 18, 20, 21) ~ "Control",
            TRUE ~ "Intervention"
        )
    )

#########################################################################
# 3. Create a unified Consent date column:
########################################################################

screening_raw_df <- screening_raw_df %>% 
    mutate(
        initial_consent_date = coalesce(consent_date, consent_date_v2))%>%
    mutate(
        # Convert to Date format
        initial_consent_date = as.Date(initial_consent_date),
        rct_dob              = as.Date(rct_dob),
        
        # Calculate age at consent (in years)
        age_at_screening = case_when(
            rct_know_age == "Yes" ~ as.numeric(rct_age),  # use reported age
            rct_know_dob == "Yes" ~ floor(time_length(interval(rct_dob, initial_consent_date), "years")), # calculate from DOB
            TRUE ~ NA_real_  # fallback if neither is available
        )
    )

# Age plausibility
summary(screening_raw_df$age_at_screening)

out_range <- screening_raw_df %>% 
    filter(age_at_screening < 14 | age_at_screening > 49)
###########################################################################
# 4. Create Eligibility column
# Rule: participants marked as eligible (rct_eligible == 1) are coded as 1
#       all others (excluded) are recoded as 0
###########################################################################
screening_raw_df <- screening_raw_df %>%
    mutate(
        eligible_study = case_when(
            rct_eligible == 1 ~ 1,   # keep eligible as 1
            TRUE ~ 0                 # recode everything else as 0
        )
    )


############################################################################
# 5. Create a unified exclusion reasons column
###########################################################################
screening_raw_df <- screening_raw_df %>% 
# Exclusion reasons
    mutate(
    exclusion_reasons = case_when(
        rct_eligible != 1 & rct_eligible_gestation == "No" ~ "Gestation <20 Weeks",
        rct_eligible != 1 & rct_harm_thought == "Yes" & rct_memory_problem == "Yes" ~ "Self harm and memory problem",
        rct_eligible != 1 & rct_harm_thought == "Yes" & rct_memory_problem == "No"  ~ "Self harm",
        rct_eligible != 1 & rct_aud_hallucinations == "Yes" ~ "Hearing voices that others cannot hear",
        rct_eligible != 1 & rct_vis_hallucinations == "Yes" ~ "Seeing things that others cannot see",
        rct_eligible != 1 & rct_delusions == "Yes"          ~ "Holding unusual beliefs",
        rct_eligible != 1 & rct_paranoia == "Yes"           ~ "Feels watched/followed",
        rct_eligible != 1 & rct_memory_problem == "Yes"     ~ "Memory problem",
        TRUE ~ NA_character_
    )
)

# Eligibility vs exclusion
eli_vs_exclusion <- screening_raw_df %>% 
    filter(eligible_study == 1 & !is.na(exclusion_reasons))

###########################################################################
# 6. Create a consented column
##########################################################################
screening_raw_df <- screening_raw_df %>% 
    mutate(
    consented = case_when(
        !is.na(exclusion_reasons) ~ NA_character_,
        rct_eligible == 1 & rct_enrolling == "Yes" ~ "Consented",
        TRUE ~ "Not Consented"
    )
)


############################################################################
# 7. Create enrollement column:
###########################################################################
screening_raw_df <- screening_raw_df %>% 
    mutate(
        enrolled = case_when(
            rct_eligible == 1 &rct_enrolling == "Yes" &
                !is.na(initial_consent_date) ~ 1L,
            TRUE ~ 0L
        )
    )

# Consent date consistency
date_consistency <- screening_raw_df %>% 
    filter(enrolled == 1 & is.na(initial_consent_date))

############################################################################
# 7. Create a unified decline reasons column:
###########################################################################
screening_raw_df <- screening_raw_df %>% 
    mutate(
        # Create decline_reason only if eligible == 1 and enrolling == "No"
        decline_reason = case_when(
            rct_eligible == 1 & rct_enrolling == "No" & rct_decline_reason == "Other (specify) ___" ~ rct_other_reasons,
            rct_eligible == 1 & rct_enrolling == "No" ~ rct_decline_reason,
            TRUE ~ NA_character_   # otherwise leave blank
        )
    )

# Decline reason consistency
decl_consistency <- screening_raw_df %>% 
    filter(eligible_study == 0 & !is.na(decline_reason))
 

###########################################################################
# 7. Clean Screening Dataset
########################################################################

screening_clean_df <- screening_raw_df %>% 
    select(record_id, arm, anc_num, initial_consent_date, eligible_study, 
           exclusion_reasons, consented, enrolled, decline_reason)

