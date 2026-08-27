# Header ------------------------------------------------------------------

# Author(s): Dowaga
# Date: August 25, 2026
# This is a script to prepare a cleaned RCT Demographic dataset 
# that's ready for analysis, manuscript writing, and sharing 

# Setup ------------------------------------------------------------------------
rm(list = ls())
# Reference source codes & other dependencies:
source("Dependencies.R")
source("DataTeam_ipmh.R")
source("data_import.R")

###############################################################
# 1. only keep the databases we need----
###############################################################
rm(list = setdiff(ls(), c("ppw_rct_df", "ipmh_filepath",
                          "data_freeze")))


##############################################################
# 2. Create visit type from redcap event names and filter Enrollment----
##############################################################

enrollment_df <- ppw_rct_df %>% 
    mutate(
        visit_type = case_when(
            grepl("Enrollment", redcap_event_name) ~ "Enrollment",
            grepl("6 Weeks", redcap_event_name) ~ "6 Weeks",
            grepl("14 Weeks", redcap_event_name) ~ "14 Weeks", 
            grepl("6 Months", redcap_event_name) ~ "6 Months",
            TRUE ~ NA_character_
        )) %>% 
    filter(visit_type == "Enrollment")

# Check duplicated IDs
anyDuplicated(enrollment_df$record_id)

################################################################################
# 3. Select varibales from the Demographic CRF----
################################################################################
demographics_df <- enrollment_df %>% 
    select(clt_ptid, clt_study_site, clt_date,starts_with("dem_"), demographics_complete, med_pre_preg, 
           med_pastdiag___2)

################################################################################
# 4. Calculating age from date of birth for those who knew DoB:----
################################################################################
demographics_df <- demographics_df %>% 
    mutate(dem_age = if_else(
        dem_dob_uk == "Yes",
        floor(time_length(interval(dem_dob, clt_date), "years")),
        dem_age
    ))

# Check those who knew DoB but date not given----
dob_missing <- demographics_df %>% 
    filter(dem_dob_uk == "Yes" & is.na(dem_dob))

# check that Dob is not missing and is before clt_date
dob_consistency <- demographics_df %>% 
    filter(dem_dob_uk == "Yes" & (is.na(dem_dob) | dem_dob > clt_date))


# Check age distribution----
summary(demographics_df$dem_age)

################################################################################
# 5. Recode current partner status:----
# Assign 1 if response is "Yes", otherwise 0 (covers "No", "Don't know/No Answer")
###############################################################################
demographics_df <- demographics_df %>%
    mutate(
        dem_current_partner_num = dplyr::case_when(
            dem_current_partner == "Yes" ~ 1,
            dem_current_partner == "No" ~ 0,
            dem_current_partner %in% c("Don't know", "No answer") ~ NA_real_,
            TRUE ~ NA_real_
        )
    )


tabyl(demographics_df$dem_current_partner_num)

################################################################################
# 6. Binary recode of marital status:----
# 1 = Currently married
# 0 = All other statuses (single, divorced, widow, steady boyfriend, come we stay)
# NA = Prefer not to answer or missing
################################################################################
demographics_df <- demographics_df %>%
    mutate(
        dem_maritalstat_num = dplyr::case_when(
            dem_maritalstat == "Currently married" ~ 1,
            dem_maritalstat %in% c("Prefer not to answer", NA) ~ NA_real_,
            TRUE ~ 0)
    )

# Check distribution of binary marital status (1 = married, 0 = not married, NA = missing/prefer not to answer)
tabyl(demographics_df$dem_maritalstat_num)

demographics_df %>% 
    tabyl(dem_maritalstat_num, dem_maritalstat)

# Check marriage type distribution among married participants----
demographics_df %>% 
    filter(dem_maritalstat_num == 1) %>% 
    tabyl(dem_marriage)

# Check marriage type distribution among non-married participants (if any)----
demographics_df %>% 
    filter(dem_maritalstat_num == 0) %>% 
    tabyl(dem_marriage)

################################################################################
# 7. Recode marriage type to NA for participants with inconsistent responses
# (e.g., divorced, has a partner, and prefers not to answer marriage type)
################################################################################

demographics_df <- demographics_df %>%
    mutate(
        dem_marriage = case_when(
            dem_maritalstat == "Divorced/separated" & dem_current_partner_num == 1 &
                grepl("Prefer not to answer", dem_marriage) ~ NA_character_,
            TRUE ~ dem_marriage
        )
    )

################################################################################
# 8. Recoding if the partner is the baby's father
################################################################################
demographics_df <- demographics_df %>%
    mutate(
        dem_partner_father_num = dplyr::case_when(
            dem_current_partner_num == 1 & dem_currentpartner_father == "Yes" ~ 1,
            dem_current_partner_num == 1 & dem_currentpartner_father == "No" ~ 0,
            dem_current_partner_num == 1 & dem_currentpartner_father %in% c("Unsure", "No Answer") ~ NA_real_,
            TRUE ~ NA_real_
        )
    )


demographics_df %>%
    filter(dem_current_partner_num == 1) %>%
    tabyl(dem_partner_financial)


################################################################################
# 9. Recoding if the partner provides financial support
################################################################################
demographics_df <- demographics_df %>%
    mutate(
        dem_partner_support_num = dplyr::case_when(
            dem_current_partner_num == 1 & dem_partner_financial == "Yes" ~ 1,
            dem_current_partner_num == 1 & dem_partner_financial == "No" ~ 0,
            dem_current_partner_num == 1 & dem_partner_financial == "No Answer" ~ NA_real_,
            TRUE ~ NA_real_
        )
    )

# Check
demographics_df %>%
    filter(dem_current_partner_num == 1) %>%
    tabyl(dem_partner_support_num)

################################################################################
# 10. Recoding if respondent shares primary residence with partner
################################################################################
demographics_df <- demographics_df %>%
    mutate(
        dem_partner_residence_num = dplyr::case_when(
            dem_current_partner_num == 1 & dem_pc_residence == "Yes (Ndio) [Kamano]" ~ 1,
            dem_current_partner_num == 1 & dem_pc_residence == "No (La) [Ooyo]" ~ 0,
            dem_current_partner_num == 1 & grepl("Prefer not to answer", dem_pc_residence) ~ NA_real_,
            TRUE ~ NA_real_
        )
    )

# Check
demographics_df %>%
    filter(dem_current_partner_num == 1) %>%
    tabyl(dem_partner_residence_num)

################################################################################
# 11. Recode currently in school
################################################################################
demographics_df <- demographics_df %>%
    mutate(
        current_school_num = dplyr::case_when(
            dem_current_school == "Yes" ~ 1,
            dem_current_school == "No" ~ 0,
            TRUE ~ NA_real_
        )
    )

# Check distribution of currently in school
demographics_df %>%
    tabyl(current_school_num)

# Check distribution of number of years completed in school
demographics_df %>%
    tabyl(dem_school)

# Schooling vs. age: Years in school greater than age
school_age <- demographics_df %>% 
    filter(dem_school > dem_age)

# Schooling vs. age: Years in school should not exceed age minus 4
school_age <- demographics_df %>% 
    filter(dem_school > (dem_age - 4))


#filter those who took > 24 in school
more_schooling <- demographics_df %>%
    filter(dem_school > 24)

################################################################################
# 12. Recoding regular employment
################################################################################
demographics_df <- demographics_df %>%
    mutate(
        dem_regular_num = dplyr::case_when(
            dem_employment == "Yes" ~ 1,
            dem_employment == "No" ~ 0,
            dem_employment == 1 & grepl("Prefer not to answer", dem_employment) ~ NA_real_,
            TRUE ~ NA_real_
        )
    )

# Check distribution of regular employment
demographics_df %>%
    tabyl(dem_regular_num)

# Check those who are currently in school & regular employment
schol <- demographics_df %>% 
    filter(current_school_num == 1) %>% 
    filter(dem_regular_num == 1)

################################################################################
# Check distribution of people in the household----
demographics_df %>%
    tabyl(dem_household_num)


# Distribution of number of rooms in respondent's sleeping house----
demographics_df %>%
    tabyl(dem_houserooms)

# Distribution of people sleeping in same house as respondent----
demographics_df %>% 
    tabyl(dem_housesleep)


# more people sleeping in house than household size----
household_vs_sleep_check <- demographics_df %>% 
    filter(dem_housesleep > dem_household_num)



###########################################################################
# 13. Clean Demographic Dataset
########################################################################

demographic_clean_df <- demographics_df %>% 
    select(clt_ptid, dem_age, dem_maritalstat_num, dem_current_partner_num, 
           dem_maritalstat_num, dem_partner_father_num, dem_partner_support_num,
           dem_partner_residence_num,current_school_num, dem_regular_num)
