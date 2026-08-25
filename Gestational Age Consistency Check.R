# Header ------------------------------------------------------------------

# Author(s): Dowaga
# Date: March 04, 2026


# Setup ------------------------------------------------------------------------
rm(list = ls())
# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")

# Data for baseline vs. follow-up
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



## RCT Enrollment LMP----
enr_lmp <- ppw_rct_df %>% 
    filter(visit_type == "Enrollment") %>% 
    select(clt_ptid, clt_date, med_lmp_know, med_lmp, med_pre_gestage_current)

# LMP after enrollment Qcs
lmp_enrollment <- enr_lmp %>% 
    filter(med_lmp > clt_date) %>% 
    select(clt_ptid, med_lmp, clt_date)

# Current recorded GA in weeks < 20
Ga_less20 <- enr_lmp %>% 
    filter(med_pre_gestage_current < 20)


# Current recorded GA in weeks > 40
Ga_greater40 <- enr_lmp %>% 
    filter(med_pre_gestage_current > 40)

# LMP at enrollment is missing
enro_lmp_missing_gc <- enr_lmp %>% 
    filter(is.na(med_lmp))

## RCT Outcome(6 weeks delivery date)----
outcome_lmp <- rct_ppw_followup %>% 
    filter(visit_type == "6 Weeks") %>%
    filter(tpnc_ended == "Yes") %>% 
    select(clt_ptid, tpnc_ended, tpnc_lb, tpnc_date, tpnc_gestage, tpnc_gestfill)

## Marge enrollmet LMP and delivery date and calculate GA at delivery----
compared_lmp <- enr_lmp %>% 
    right_join(outcome_lmp, by = "clt_ptid") %>% 
    mutate(med_lmp = as.Date(med_lmp),
        clt_date = as.Date(clt_date),
        tpnc_date = as.Date(tpnc_date), 
        ga_at_enrollment = round(as.numeric(clt_date - med_lmp) / 7,2),
        ga_at_birth = round(as.numeric(tpnc_date - med_lmp) / 7,2))


## Select LMP from screening database--- 
screening_lmp <- screening_consent_df %>% 
    filter(ipmh_rct_enrollment_consent_v50_complete == "Complete"|
               ipmh_rct_enrollment_consent_v60_complete == "Complete") %>% 
    select(partipant_id_v2, rct_know_last_mpdate, rct_last_menstrual_date) %>% 
    #filter(rct_know_last_mpdate == "Yes") %>% 
    mutate(rct_last_menstrual_date = as.Date(rct_last_menstrual_date))

## merge screening LMP with the enrollment LMP and deliver date
all_lmps <- screening_lmp %>% 
    right_join(compared_lmp, by = c("partipant_id_v2" = "clt_ptid")) %>% 
    mutate(screening_ga = round(as.numeric(clt_date - rct_last_menstrual_date) / 7,2),
        lmp_mismatch = if_else(rct_last_menstrual_date != med_lmp, 1, 0)
    ) 

# GA at birth is less than GA at enrollment Qcs----
birth_less_enro <- all_lmps %>% 
    filter(ga_at_birth < ga_at_enrollment)

# Screening GA < 20 
screening_ga_qc <- all_lmps %>% 
    filter(screening_ga < 20)

# Gestational age at enrollment smaller than 20
less_enro_ga <- all_lmps %>% 
    filter(ga_at_enrollment < 20)

# Gestational age at enrollment greater than 50
enrollment_ga_50qc <- all_lmps %>% 
    filter(ga_at_enrollment > 50) %>% 
    select("partipant_id_v2", "clt_date", "rct_last_menstrual_date", 
           "med_lmp", "tpnc_date", "ga_at_birth", "ga_at_enrollment", "screening_ga")
    
# check record with mismatch screening and enrollment GAs---
screening_less_enro_qc <- all_lmps %>%
    mutate(
        screening_less_enro = ifelse(screening_ga != ga_at_enrollment,
                                    1,0)
    ) %>% 
    filter(screening_less_enro == 1)

# Create a new workbook
wb <- createWorkbook()

# Add first sheet: LMP after enrollment Qcs
addWorksheet(wb, "LMP after enrollment")
writeData(wb, "LMP after enrollment", lmp_enrollment)

# Add second sheet: Delivery less enrollment GA
addWorksheet(wb, "Delivery less enrollment GA")
writeData(wb, "Delivery less enrollment GA", birth_less_enro)

# Add third sheet: Current Recorded GA < 20
addWorksheet(wb, "Current Recorded GA < 20")
writeData(wb, "Current Recorded GA < 20", Ga_less20)

# Add fourth sheet: Current Recorded GA > 40
addWorksheet(wb, "Current Recorded GA > 40")
writeData(wb, "Current Recorded GA > 40", Ga_greater40)

# Add fifth sheet: LMP missing
addWorksheet(wb, "Enrollment LMP Missing")
writeData(wb, "Enrollment LMP Missing", enro_lmp_missing_gc)

# Add sixth sheet: Screening GA <20
addWorksheet(wb, "Screening GA <20")
writeData(wb, "Screening GA <20", screening_ga_qc)

# Add seventh sheet: Enrollment GA > 50
addWorksheet(wb, "Enrollment GA >50 Weeks")
writeData(wb, "Enrollment GA >50 Weeks", enrollment_ga_50qc)

# Add eighth sheet: Screening GA less Enrollment GA
addWorksheet(wb, "Screening less Enrollment(GA)")
writeData(wb, "Screening less Enrollment(GA)", screening_less_enro_qc)

# Add tenth sheet: Enrollment GA <20
addWorksheet(wb, "Enrollment GA <20")
writeData(wb, "Enrollment GA <20", less_enro_ga)


# Save workbook to a different directory
saveWorkbook(wb, "C:/Users/hp/OneDrive/Desktop/IPMH/QCs/LMP or GA QC_checks.xlsx", overwrite = TRUE)


