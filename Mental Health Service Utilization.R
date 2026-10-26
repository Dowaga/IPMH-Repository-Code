# Header ------------------------------------------------------------------

# Author(s): Dowaga
# Date: Jun 27, 2025
# This script is used to analyze Mental Health Service Utilization from the PPW RCT study. 
# The generated tables will be included in the weekly data report for team review.

# Included outcomes: mental health service utilization.

# N/B: Codes were cut from PPR RCT follow up to shorten the script

# Setup ------------------------------------------------------------------------
# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")


# Data pre---
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
# Mental Health Service Utilization========
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
    mutate(visit_type = factor(visit_type, 
                               levels = c("Enrollment", "6 Weeks", "14 Weeks", "6 Months"))) %>%
    select(visit_type, su_phq2, su_gap2, su_phq9, su_gad7, 
           su_access, su_referral, su_tele) %>%
    tbl_summary(
        by = visit_type,
        statistic = list(all_categorical() ~ "{n} ({p}%)"),
        missing = "no",
        digits = list(
            all_continuous() ~ 1,       # continuous variables ??? 1 d.p.
            all_categorical() ~ c(0, 1) # categorical ??? 0 decimals for n, 1 d.p. for %
        ),
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
    bold_labels() %>%
    gtsummary::as_gt() %>%
    gt::tab_header(
        title = gt::md(
            "**Screening and Service Utilization by Visit Type (Self-Reported During Visits)**"
        )
    )

table6

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
        missing = "no",
        digits = list(
            all_continuous() ~ 1,       # continuous variables ??? 1 d.p.
            all_categorical() ~ c(0, 1) # categorical ??? 0 decimals for n, 1 d.p. for %
        ),
        label = list(
            su_pmp ~ "PM+ Participation",
            su_many ~ "PM+ Sessions Completed",
            su_pmrestart___0 ~ "No Disruption",
            su_pmrestart___1 ~ "Stopped PM+", 
            su_pmrestart___2 ~ "Restarted PM+"
        )
    )%>%
    add_overall() %>%
    modify_header(label ~ "**Variable**") %>%
    modify_spanning_header(
        c("stat_1", "stat_2", "stat_3") ~ "**Visit Type**"
    ) %>%
    bold_labels() %>%
    gtsummary::as_gt() %>%
    gt::tab_header(
        title = gt::md(
            "**PM+ Program Participation Status (Self-Reported During Visits)**"
        )
    ) %>%
    gt::tab_options(
        table.font.size = 12
    )

table7


over <- su %>% filter(su_many > 5)
