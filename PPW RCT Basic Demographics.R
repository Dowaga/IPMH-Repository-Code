# Header ------------------------------------------------------------------

# Author(s): Dowaga
# Date: May 6, 2025
# Baseline Demographics

# Setup ------------------------------------------------------------------------
# rm(list = ls())
# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
# source("Dependencies.R")
# source("data_import.R")

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

# basic demo table
basic_demo <- demographics_df %>%
    tbl_summary(
    sort = list(all_categorical() ~ "frequency"),  
    include=c(dem_age, dem_current_partner_num, 
              dem_maritalstat_num, dem_marriage, 
              dem_pc_residence_num, dem_current_school, 
              dem_school, dem_employment, 
              dem_household_num, dem_housesleep, 
              dem_houserooms,dem_traveltime_min,
              med_pre_preg),
    label = list(dem_age ~ "Age (Years)",
                 dem_current_partner_num ~ "Do you currently have a partner",
                 dem_employment ~ "Do you have regular employment (Yes)",
                 dem_maritalstat_num ~ "Currently Married (Yes)",
                 dem_pc_residence_num ~ "Shares residence with partner",
                 dem_marriage ~ "Marriage (Monogamous)",
                 dem_current_school ~ "Currently in School",
                 dem_school ~ "Completed years in School",
                 dem_household_num ~ "Number of people in Household",
                 dem_housesleep ~ "Number of people sleep in same house",
                 dem_houserooms ~ "Rooms in the house most often sleep in",
                 dem_traveltime_min ~ "Time to the Clinic (Minutes)",
                 med_pre_preg ~ "Have you been pregnant before (Yes)"),
    missing = "no",
    digits = list(
        all_continuous() ~ 1,       
        all_categorical() ~ c(0, 1) 
    ), 
    type = list(dem_age ~ "continuous", 
                dem_household_num ~ "continuous",
                dem_housesleep ~ "continuous", 
                dem_houserooms ~ "continuous", 
                dem_traveltime_min ~ "continuous"),
    statistic = list(all_continuous() ~ "{median} [IQR: {p25}, {p75}]")) %>% 
    bold_labels() %>%
    add_n() %>% 
    #add_p() %>% 
    # convert from gtsummary object to gt object
    gtsummary::as_gt() %>%
    fmt_number(
        columns = where(is.numeric),
        sep_mark = ""      # removes commas
    ) %>% 
    # modify with gt functions
    gt::tab_header("Basic Demographic Summary") %>% 
    gt::tab_options(
        table.font.size = "medium",
        data_row.padding = gt::px(1)) %>%
    tab_options(
        table.font.size = px(14))

basic_demo

# WLWH Cohort Baseline Demographics
# basic demo table
wlwh_demo <- demographics_df %>%
    filter(med_pastdiag___2 == "Checked") %>% 
    tbl_summary(
        sort = list(all_categorical() ~ "frequency"),  # Sort categorical levels by frequency in descending order
        include=c(dem_age, dem_current_partner, dem_maritalstat,
                  dem_marriage, dem_pc_residence, dem_current_school, dem_school, 
                  dem_employment, dem_household_num, dem_housesleep, 
                  dem_houserooms,dem_traveltime_min, med_pre_preg),
        label = list(dem_age ~ "Age (Years)",
                     dem_current_partner ~ "Do you currently have a partner",
                     dem_employment ~ "Do you have regular employment (Yes)",
                     dem_maritalstat ~ "Currently Married (Yes)",
                     dem_pc_residence ~ "Shares residence with partner (Yes)",
                     dem_marriage ~ "Marriage (Monogamous)",
                     dem_current_school ~ "Currently in School",
                     dem_school ~ "Completed years in School",
                     dem_household_num ~ "Number of people in Household",
                     dem_housesleep ~ "Number of people sleep in same house",
                     dem_houserooms ~ "Rooms in the house most often sleep in",
                     dem_traveltime_min ~ "Time to the Clinic (Minutes)",
                     med_pre_preg ~ "Have you been pregnant before (Yes)"),
        missing = "no",
        digits = list(
            all_continuous() ~ 1,       # continuous variables ??? 1 d.p.
            all_categorical() ~ c(0, 1) # categorical ??? 0 decimals for n, 1 d.p. for %
        ), 
        type = list(dem_age ~ "continuous", 
                    dem_household_num ~ "continuous",
                    dem_housesleep ~ "continuous", 
                    dem_houserooms ~ "continuous", 
                    dem_traveltime_min ~ "continuous"),
        statistic = list(all_continuous() ~ "{median} [IQR: {p25}, {p75}]")) %>% 
    bold_labels() %>%
    add_n() %>% 
    #add_p() %>% 
    # convert from gtsummary object to gt object
    gtsummary::as_gt() %>%
    # modify with gt functions
    gt::tab_header("WLWH Demographic Summary") %>% 
    gt::tab_options(
        table.font.size = "medium",
        data_row.padding = gt::px(1)) %>%
    tab_options(
        table.font.size = px(14)) %>%
    opt_table_lines()

wlwh_demo

#------------------------------------------------------------------------------
#### By Arm
demographics_df <- demographics_df %>% 
    mutate(
        dummy_arm = case_when(
            arm_group == "Control" ~ "Arm X",
            arm_group == "Intervention" ~ "Arm Y"))

# basic demo table
arm_demo <- demographics_df %>%
    tbl_summary(by =arm_group, 
        sort = list(all_categorical() ~ "frequency"),  # Sort categorical levels by frequency in descending order
        include=c(dem_age, dem_current_partner, dem_maritalstat,
                  dem_marriage, dem_pc_residence, dem_current_school, dem_school, 
                  dem_employment, dem_household_num, dem_housesleep, 
                  dem_houserooms,dem_traveltime_min, med_pre_preg),
        label = list(dem_age ~ "Age (Years)",
                     dem_current_partner ~ "Do you currently have a partner",
                     dem_employment ~ "Do you have regular employment (Yes)",
                     dem_maritalstat ~ "Currently Married (Yes)",
                     dem_pc_residence ~ "Shares residence with partner (Yes)",
                     dem_marriage ~ "Marriage (Monogamous)",
                     dem_current_school ~ "Currently in School",
                     dem_school ~ "Completed years in School",
                     dem_household_num ~ "Number of people in Household",
                     dem_housesleep ~ "Number of people sleep in same house",
                     dem_houserooms ~ "Rooms in the house most often sleep in",
                     dem_traveltime_min ~ "Time to the Clinic (Minutes)",
                     med_pre_preg ~ "Have you been pregnant before (Yes)"),
        missing = "no",
        digits = list(
            all_continuous() ~ 1,       # continuous variables ??? 1 d.p.
            all_categorical() ~ c(0, 1) # categorical ??? 0 decimals for n, 1 d.p. for %
        ), 
        type = list(dem_age ~ "continuous", 
                    dem_household_num ~ "continuous",
                    dem_housesleep ~ "continuous", 
                    dem_houserooms ~ "continuous", 
                    dem_traveltime_min ~ "continuous"),
        statistic = list(all_continuous() ~ "{median} [IQR: {p25}, {p75}]")) %>% 
    bold_labels() %>%
    #add_n() %>% 
    add_overall() %>% 
    # convert from gtsummary object to gt object
    gtsummary::as_gt() %>%
    # modify with gt functions
    gt::tab_header("Basic Demographic Summary") %>% 
    gt::tab_options(
        table.font.size = "medium",
        data_row.padding = gt::px(1)) %>%
    tab_options(
        table.font.size = px(14)) %>%
    opt_table_lines()

arm_demo


# WLWH Cohort Baseline Demographics by arm
# basic demo table
arm_wlwh_demo <- demographics_df %>%
    mutate(
        dummy_arm = case_when(
            arm_group == "Control" ~ "Arm X",
            arm_group == "Intervention" ~ "Arm Y")) %>% 
    filter(med_pastdiag___2 == "Checked") %>% 
    tbl_summary(by = arm_group,
        sort = list(all_categorical() ~ "frequency"),  # Sort categorical levels by frequency in descending order
        include=c(dem_age, dem_current_partner, dem_maritalstat,
                  dem_marriage, dem_pc_residence, dem_current_school, dem_school, 
                  dem_employment, dem_household_num, dem_housesleep, 
                  dem_houserooms,dem_traveltime_min, med_pre_preg),
        label = list(dem_age ~ "Age (Years)",
                     dem_current_partner ~ "Do you currently have a partner (Yes)",
                     dem_employment ~ "Do you have regular employment (Yes)",
                     dem_maritalstat ~ "Currently Married (Yes)",
                     dem_pc_residence ~ "Shares residence with partner (Yes)",
                     dem_marriage ~ "Marriage (Monogamous)",
                     dem_current_school ~ "Currently in School",
                     dem_school ~ "Completed years in School",
                     dem_household_num ~ "Number of people in Household",
                     dem_housesleep ~ "Number of people sleep in same house",
                     dem_houserooms ~ "Rooms in the house most often sleep in",
                     dem_traveltime_min ~ "Time to the Clinic (Minutes)",
                     med_pre_preg ~ "Have you been pregnant before (Yes)"),
        missing = "no",
        digits = list(all_continuous() ~ 1), 
        type = list(dem_age ~ "continuous", 
                    dem_household_num ~ "continuous",
                    dem_housesleep ~ "continuous", 
                    dem_houserooms ~ "continuous", 
                    dem_traveltime_min ~ "continuous"),
        statistic = list(all_continuous() ~ "{median} [IQR: {p25}, {p75}]")) %>% 
    bold_labels() %>%
    #add_p() %>% 
    add_overall() %>% 
    # convert from gtsummary object to gt object
    gtsummary::as_gt() %>%
    # modify with gt functions
    gt::tab_header("WLWH Demographic Summary") %>% 
    gt::tab_options(
        table.font.size = "medium",
        data_row.padding = gt::px(1)) %>%
    tab_options(
        table.font.size = px(14))%>%
    opt_table_lines()

arm_wlwh_demo

