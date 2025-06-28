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

# Only keep the necessary dataset
rm(list = ls()[! ls() %in% c("rct_ppw")])

# Data for baseline vs. follow-up
rct_ppw <- rct_ppw %>%
    mutate(
        visit_type = case_when(
            grepl("Enrollment", redcap_event_name) ~ "Enrollment",
            grepl("6 Weeks", redcap_event_name) ~ "6 Weeks",
            grepl("14 Weeks", redcap_event_name) ~ "14 Weeks", 
            grepl("6 Months", redcap_event_name) ~ "6 Months",
            grepl("PM\\+ Session 5", redcap_event_name) ~ "PM+ Session 5",
            TRUE ~ "Other"
        ),
        arm = case_when(
            grepl("Arm 1: Intervention", redcap_event_name) ~ "Intervention",
            grepl("Arm 2: Control", redcap_event_name) ~ "Control",
            TRUE ~ "Unknown"
        )
    )

ase_data <- rct_ppw %>% 
    filter(!is.na(redcap_repeat_instrument))
non_ase_data <- rct_ppw %>% 
    filter(is.na(redcap_repeat_instrument))

rct_ppw_followup <- non_ase_data %>%
    filter(visit_type %in% c("6 Weeks", "14 Weeks", "6 Months"))

rct_ppw_baseline <- non_ase_data %>%
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
            all_continuous() ~ "{mean} ({sd})",
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

# Table 3: Primary and secondary clinical outcomes --------------
