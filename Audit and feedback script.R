# Header ------------------------------------------------------------------

# Author(s): Yuwei Wang
# Date: March 20, 2025
# Description: This script is used to generate data reports for the monthly audit and feedback meetings. 
# The following indicators are calculated:
# 1. PHQ2/GAD2 screening (among all sites for all women) [weekly/monthly]
# 2. PHQ9/GAD7 screening (among intervention sites for study participants) [weekly/monthly]
# 3. PM+ session referral (among intervention sites for study participants) 
# 4. Telepsychiatry session referral (among intervention sites for study participants)
# 5. PM+ session initiation (among intervention sites for study participants)
# 6. Telepsychiatry session initiation (among intervention sites for study participants)
# 7. PM+ session completion (among intervention sites for study participants)
# 8. Health talks & flipbook use (among all sties)

# Setup ------------------------------------------------------------------------
rm(list = ls())             


# Reference source codes & other dependencies: Use this section to reference other scripts and dependencies
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("REDCap_datapull.R")

##### main database needed
#Data close out survey 
#PHQ2/GAD2 abstraction database
#PHQ9/GAD7 abstraction form in the PPW longitudinal database
#PM+ session survey
#Telepsychiatry session survey

#drop all other datasets
rm(list = ls()[! ls() %in% c("daily_closeout", "phq2_gad2_abstract", "rct_ppw", "pm", "telepsych")])

#Ensure date columns are in proper format
daily_closeout$rct_dcr_date <- as.Date(daily_closeout$rct_dcr_date)
phq2_gad2_abstract$screening_date <- as.Date(phq2_gad2_abstract$screening_date) 
rct_ppw$clt_timestamp <- as.Date(rct_ppw$clt_timestamp)
pm$pm_date <- as.Date(pm$pm_date)
telepsych$telepsych_date <- as.Date(telepsych$telepsych_date)

#time period for the data
daily_closeout <- daily_closeout %>% filter(rct_dcr_date <= "2025-03-15")
phq2_gad2_abstract <- phq2_gad2_abstract %>% filter(screening_date <= "2025-03-15")
rct_ppw <- rct_ppw %>% filter(clt_timestamp <= "2025-03-15")
pm <- pm %>% filter(pm_date <= "2025-03-15")
#telepsych <- telepsych %>% filter(tele_date <= "2025-03-15")

#uniform study site number for all datasets
phq2_gad2_abstract <- phq2_gad2_abstract %>%
    mutate(study_site = str_replace(study_site, "^\\d+,\\s*", ""))

rct_ppw <- rct_ppw %>%
    mutate(clt_study_site = str_replace(clt_study_site, "^\\d+,\\s*", ""))

rct_ppw <- rct_ppw %>% 
    filter(is.na(redcap_repeat_instrument))

#manually making sure record_id in abstraction database do not have space
phq2_gad2_abstract$record_id <- str_replace(phq2_gad2_abstract$record_id, " ", "")

#mirogi is spelt wrong in the daily close out
daily_closeout <- daily_closeout %>%
    mutate(rct_facility_name = str_replace(rct_facility_name, "Mirogi Heath Centre", "Mirogi Health Centre"))

#1. PHQ2/GAD2 screening---------------

#1.1 cross checking the number ------------
# method 1 - daily checking
#get the total number of clients screened per facility per day using abstraction dataset
num_screened <- phq2_gad2_abstract %>%
    group_by(study_site, screening_date) %>%
    summarise(num_screened = n(), .groups = "drop")

#compare this with rct_screening in daily closeout data
daily_closeout %>%
    mutate(day = floor_date(rct_dcr_date, "day")) %>%
    group_by(rct_facility_name, day) %>%
    summarise(rct_screening = sum(rct_screening, na.rm = TRUE), .groups = "drop") %>%
    left_join(num_screened, by = c("rct_facility_name" = "study_site", "day" = "screening_date")) %>%
    mutate(diff = rct_screening - num_screened) %>%
    filter(diff != 0) %>% knitr::kable()
#rct_screening is the number of clients screened in the daily closeout data
#num_screened is the number of clients screened in the abstraction data

#method 2 - total checking
total_num_screened <- phq2_gad2_abstract %>%
    group_by(study_site) %>%
    summarise(num_screened = n(), .groups = "drop")

daily_closeout %>%
    group_by(rct_facility_name) %>%
    summarise(rct_screening = sum(rct_screening, na.rm = TRUE), .groups = "drop") %>%
    left_join(total_num_screened, by = c("rct_facility_name" = "study_site")) %>%
    mutate(diff = rct_screening - num_screened) %>%
    filter(diff != 0) %>% knitr::kable()

#method 3 - weekly checking
##get the total number of clients screened per facility per week using abstraction dataset
num_screened_weekly <- phq2_gad2_abstract %>%
    mutate(week = floor_date(screening_date, "week")) %>%
    group_by(study_site, week) %>%
    summarise(num_screened = n(), .groups = "drop")

#compare this with rct_screening in daily closeout data
daily_closeout %>%
    mutate(week = floor_date(rct_dcr_date, "week")) %>%
    group_by(rct_facility_name, week) %>%
    summarise(rct_screening = sum(rct_screening, na.rm = TRUE), .groups = "drop") %>%
    left_join(num_screened_weekly, by = c("rct_facility_name" = "study_site", "week" = "week")) %>%
    mutate(diff = rct_screening - num_screened) %>%
    filter(diff != 0) %>% knitr::kable()

#1.2 PHQ2/GAD2 screening rate by facility ------------
#metho1 - daily rate
#Aggregate PHQ2/GAD2 screening rate per study site and day
screening_rate <- phq2_gad2_abstract %>%
    mutate(day = floor_date(screening_date, "day")) %>%
    group_by(study_site, day) %>%
    summarise(num_screened = n(), .groups = "drop")

# Aggregate total ANC clients from daily closeout data
total_anc_clients <- daily_closeout %>%
    mutate(day = floor_date(rct_dcr_date, "day")) %>%
    group_by(rct_facility_name, day) %>%
    summarise(total_anc_clients = sum(rct_anc_number, na.rm=T), .groups = "drop")

# Merge the two datasets
screening_rate <- full_join(screening_rate, total_anc_clients, by = c("study_site" = "rct_facility_name", "day" = "day"))

#calculate screening rate
screening_rate <- screening_rate %>%
    mutate(screening_rate = num_screened / total_anc_clients * 100)

#method2 - weekly rate
#Aggregate PHQ2/GAD2 screening rate per study site and week
screening_rate_weekly <- phq2_gad2_abstract %>%
    mutate(week = floor_date(screening_date, "week", week_start = 1)) %>%
    group_by(study_site, week) %>%
    summarise(num_screened = n(), .groups = "drop")

# Aggregate total ANC clients from daily closeout data
total_anc_clients_weekly <- daily_closeout %>%
    mutate(week = floor_date(rct_dcr_date, "week", week_start = 1)) %>%
    group_by(rct_facility_name, week) %>%
    summarise(total_anc_clients = sum(rct_anc_number, na.rm=T), .groups = "drop")

# Merge the two datasets
screening_rate_weekly <- full_join(screening_rate_weekly, total_anc_clients_weekly, 
                                   by = c("study_site" = "rct_facility_name", "week" = "week"))

#calculate screening rate
screening_rate_weekly <- screening_rate_weekly %>%
    mutate(screening_rate = num_screened / total_anc_clients * 100)

#method3 - monthly rate
screening_rate_monthly <- screening_rate %>%
    mutate(month = format(day, "%Y-%m")) %>%
    group_by(study_site, month) %>%
    summarise(
        `Monthly screening` = sum(num_screened, na.rm = TRUE),
        `Monthly ANC clients` = sum(total_anc_clients, na.rm = TRUE),
        `PHQ2/GAD2 screening rate` = (`Monthly screening`/ `Monthly ANC clients`) * 100,
        .groups = "drop"
    )
#method 4 - total rate
total_screening_rate <- screening_rate %>%
    group_by(study_site) %>% 
    summarise(
        `Total screening` = sum(num_screened, na.rm = TRUE),
        `Total ANC clients` = sum(total_anc_clients, na.rm = TRUE),
        `PHQ2/GAD2 screening rate` = (`Total screening`/`Total ANC clients`) * 100
    )

#2. PHQ9/GAD7 screening---------------
# filter only intervention sites
rct_ppw_int <- rct_ppw %>% 
    filter(clt_study_site == "Rwambwa Sub-county Hospital"|
               clt_study_site == "Uyawi Sub County Hospital"|
               clt_study_site == "Got Agulu Sub-District Hospital"|
               clt_study_site == "Kabondo Sub County Hospital"|
               clt_study_site == "Miriu Health Centre"|
               clt_study_site == "Ndiru Level 4 Hospital"|
               clt_study_site == "Ober Kamoth Sub County Hospital"|
               clt_study_site == "Usigu Health Centre"|
               clt_study_site == "Ramula Health Centre"|
               clt_study_site == "Airport Health Centre (Kisumu)")

#denomintor should just be the number of participants
n_part <- rct_ppw_int %>%
    filter(!is.na(clt_ptid)) %>%
    group_by(clt_study_site, clt_timestamp) %>%
    summarise(n_part = n(), .groups = "drop")

# numerator is the number of participants who have a PHQ9/GAD7 score
phq9_screening <- rct_ppw_int %>%
    filter(!is.na(abs_phq_tired) | !is.na(abs_gad7_afraid)) %>%
    group_by(clt_study_site, clt_timestamp) %>%
    summarise(num_screened = n(), .groups = "drop")

#merge the two datasets
phq9_screening <- full_join(phq9_screening, n_part, by = c("clt_study_site" = "clt_study_site", "clt_timestamp" = "clt_timestamp"))

#weekly data
phq9_screening_weekly <- phq9_screening %>%
    mutate(week = floor_date(clt_timestamp, "week", week_start = 1)) %>%
    group_by(clt_study_site, week) %>%
    summarise(
        `Weekly PHQ9/GAD7 screening` = sum(num_screened, na.rm = TRUE),
        `Weekly study participants` = sum(n_part, na.rm = TRUE),
        `PHQ9/GAD7 screening rate` = (`Weekly PHQ9/GAD7 screening`/ `Weekly study participants`) * 100,
        .groups = "drop"
    )

#monthly data
#make phq9_screening have one row per day per facility
phq9_screening_monthly <- phq9_screening %>%
    mutate(month = format(clt_timestamp, "%Y-%m")) %>%
    group_by(clt_study_site, month) %>%
    summarise(
        `Monthly PHQ9/GAD7 screening` = sum(num_screened, rm.na = TRUE),
        `Monthly study participants` = sum(n_part, rm.na = TRUE),
        `PHQ9/GAD7 monthly screening rate` = (`Monthly PHQ9/GAD7 screening` / `Monthly study participants`) * 100,
        .groups = "drop"
    )

#total data
phq9_screening_total <- phq9_screening %>%
    group_by(clt_study_site) %>%
    summarise(
        `Total PHQ9/GAD7 screening` = sum(num_screened, na.rm = TRUE),
        `Total study participants` = sum(n_part, na.rm = TRUE),
        `PHQ9/GAD7 total screening rate` = (`Total PHQ9/GAD7 screening` / `Total study participants`) * 100,
        .groups = "drop"
    )

#3. PM+ session referral---------------
#first change the options to numeric
rct_ppw_int <- rct_ppw_int %>%
    mutate(across(c(starts_with("abs_phq")), 
                  ~ case_when(
                      . == "not at all" ~ 0,
                      . == "several days" ~ 1,
                      . == "more than half the days" ~ 2,
                      . == "nearly every day" ~ 3,
                      TRUE ~ NA_real_
                  ), 
                  .names = "{.col}_number")) 

rct_ppw_int <- rct_ppw_int %>%
    mutate(across(c(starts_with("abs_gad")), 
                  ~ case_when(
                      . == "Not at all" ~ 0,
                      . == "Several days" ~ 1,
                      . == "Over half the days" ~ 2,
                      . == "Nearly every day" ~ 3,
                      TRUE ~ NA_real_
                  ), 
                  .names = "{.col}_number")) 

#calculate PHQ9/GAD7 score
rct_ppw_int <- rct_ppw_int %>%
    mutate(
        score_phq9 = rowSums(across(starts_with("abs_phq") & ends_with("_number")), na.rm = TRUE),
        score_gad7 = rowSums(across(starts_with("abs_gad") & ends_with("_number")), na.rm = TRUE)
    )

# Aggregate total women with PHQ9/GAD7 scores > 10 and < 15 or suicidal (Denominator)
phq9_high_scores <- rct_ppw_int %>%
    filter((score_phq9 >= 10 & score_phq9 <15 )| 
               (score_gad7 >= 10 & score_gad7 <15)) %>%
    mutate(day = floor_date(clt_timestamp, "day")) %>%
    group_by(clt_study_site, day) %>%
    summarise(total_high_scores = n(),
              .groups = "drop")

# get the numerator from rct_ppw database
pm_referral <- rct_ppw_int %>% 
    filter(abs_phq_ref_pm == "Yes" | abs_gad7_ref_pm == "Yes") 

pm_referral <- pm_referral %>%
    mutate(day = floor_date(clt_timestamp, "day")) %>%
    group_by(clt_study_site, day) %>%
    summarise(num_referred = n(), .groups = "drop")

# Merge the two datasets
pm_referral <- full_join(pm_referral, phq9_high_scores, 
                         by = c("clt_study_site" = "clt_study_site", "day" = "day"))

#calculate referral rate
pm_referral <- pm_referral %>%
    mutate(referral_rate = num_referred / total_high_scores * 100)
#### based on the abstraction form, there are three participants being referred to PM+ but their score is not high enough to be referred.

#monthly data
pm_referral_monthly <- pm_referral %>%
    mutate(month = format(day, "%Y-%m")) %>%
    group_by(clt_study_site, month) %>%
    summarise(
        `Monthly PM+ referral` = sum(num_referred, na.rm = TRUE),
        `Monthly PHQ9/GAD7 >=10 and <15` = sum(total_high_scores, na.rm = TRUE),
        `Monthly PM+ referral rate` = (`Monthly PM+ referral` / `Monthly PHQ9/GAD7 >=10 and <15`) * 100,
        .groups = "drop"
    )

#total data
pm_referral_total <- pm_referral %>%
    group_by(clt_study_site) %>%
    summarise(
        `Total PM+ referral` = sum(num_referred, na.rm = TRUE),
        `Total PHQ9/GAD7 >=10 and <15` = sum(total_high_scores, na.rm = TRUE),
        `Total PM+ referral rate` = (`Total PM+ referral` / `Total PHQ9/GAD7 >=10 and <15`) * 100,
        .groups = "drop"
    )


#4. Telepsychiatry session referral---------------
# Aggregate total women with PHQ9/GAD7 scores > 15 (Denominator)
phq9_high_scores_tele <- rct_ppw_int %>%
    filter((score_phq9 >= 15)| 
               (score_gad7 >=15)) %>%
    group_by(clt_study_site, clt_timestamp) %>%
    summarise(total_high_scores = n(),
              .groups = "drop")

# get the numerator from rct_ppw database
tele_referral <- rct_ppw_int %>% 
    filter(abs_phq_ref_tele == "Yes" | abs_gad7_ref_tele == "Yes") 

tele_referral <- tele_referral %>%
    group_by(clt_study_site, clt_timestamp) %>%
    summarise(num_referred = n(), .groups = "drop")

# Merge the two datasets
tele_referral <- full_join(tele_referral, phq9_high_scores_tele, 
                           by = c("clt_study_site" = "clt_study_site", "clt_timestamp" = "clt_timestamp"))
#there is one woman who has score more than 15 but she was not referred to telepsychiatry

#calculate referral rate
tele_referral <- tele_referral %>%
    mutate(referral_rate = num_referred / total_high_scores * 100)

#monthly data
monthly_referral_summary_tele <- tele_referral %>%
    mutate(month = format(clt_timestamp, "%Y-%m")) %>%
    group_by(clt_study_site, month) %>%
    summarise(
        `Monthly telepsych referral` = sum(num_referred, na.rm = TRUE),
        `Monthly PHQ9/GAD7 >=15` = sum(total_high_scores, na.rm = TRUE),
        `Monthly telepsych referral rate` = (`Monthly telepsych referral` / `Monthly PHQ9/GAD7 >=15`) * 100,
        .groups = "drop"
    )

#total data
total_referral_summary_tele <- tele_referral %>%
    group_by(clt_study_site) %>%
    summarise(
        `Total telepsych referral` = sum(num_referred, na.rm = TRUE),
        `Total PHQ9/GAD7 >=15` = sum(total_high_scores, na.rm = TRUE),
        `Total telepsych referral rate` = (`Total telepsych referral` / `Total PHQ9/GAD7 >=15`) * 100,
        .groups = "drop"
    )

#5. PM+ session initiation---------------
#5.1 this is per month
#numerator is the number of participants who initiated PM+ [get from pm dataset]
pm_initiation <- pm %>%
    filter(ipmh_participant == "Yes") %>%
    select(pm_facility, pm_date, pm_ptid, pm_session, pm_explain, pm_adv,
           pm_stress, pm_prob, pm_activ, pm_social, pm_stay, pm_nonresponse, pm_psychlops) %>% 
    mutate(month = format(pm_date, "%Y-%m")) 

#calculate the number of participants per facility per month and pm_session == "Session 1"
pm_summary_session1 <- pm_initiation %>%
    filter(pm_session == "Session 1") %>%
    group_by(pm_facility, month) %>%
    summarise(num_initiated = n(), .groups = "drop")

#denominator is the number of participants who are referred to PM+ [utilizing monthly_referral_summary]
pm_summary_session1 <- full_join(pm_summary_session1, pm_referral_monthly, 
                                 by = c("pm_facility" = "clt_study_site", "month" = "month"))

#change num_initiated to 0 if NA
pm_summary_session1[is.na(pm_summary_session1)] <- 0

#calculate initiation rate
pm_summary_session1 <- pm_summary_session1 %>%
    mutate(initiation_rate = num_initiated / `Monthly PM+ referral` * 100)

#5.2 this is across all time
#numerator is the number of participants who initiated PM+ [get from pm dataset]
pm_initiation_all <- pm %>%
    filter(ipmh_participant == "Yes") %>%
    select(pm_facility, pm_date, pm_ptid, pm_session, pm_explain, pm_adv,
           pm_stress, pm_prob, pm_activ, pm_social, pm_stay, pm_nonresponse, pm_psychlops)

#calculate the number of participants per facility and pm_session == "Session 1"
pm_summary_session1_all <- pm_initiation_all %>%
    filter(pm_session == "Session 1") %>%
    group_by(pm_facility) %>%
    summarise(num_initiated = n(), .groups = "drop")

#denominator is the number of participants who are referred to PM+ (get this from the rct_ppw_int database)
pm_referral_all <- rct_ppw_int %>% 
    filter(abs_phq_ref_pm == "Yes" | abs_gad7_ref_pm == "Yes") %>%
    group_by(clt_study_site) %>%
    summarise(num_referred = n(), .groups = "drop")

pm_initiation_all_merge_session1 <- full_join(pm_summary_session1_all, pm_referral_all, by = c("pm_facility" = "clt_study_site"))

#change num_initiated to 0 if NA
pm_initiation_all_merge_session1[is.na(pm_initiation_all_merge_session1)] <- 0

#calculate initiation rate
pm_initiation_all_merge_session1 <- pm_initiation_all_merge_session1 %>%
    mutate(initiation_rate = num_initiated / num_referred * 100)

#6. PM+ session completion---------------
#6.1 this is per month
pm_month <- pm %>%
    filter(ipmh_participant == "Yes") %>%
    select(pm_facility, pm_date, pm_ptid, pm_session, pm_explain, pm_adv,
           pm_stress, pm_prob, pm_activ, pm_social, pm_stay, pm_nonresponse, pm_psychlops) %>% 
    mutate(month = format(pm_date, "%Y-%m"))

#calculate the number of participants per facility per month and pm_session == "Session 1"

#7. Telepsychiatry session initiation---------------
#7.1 this is per month
#numerator is the number of participants who initiated telepsychiatry [get from telepsych dataset]


#denominator is the number of participants who are referred to telepsychiatry




#8. Health talks & flipbook use---------------
# extract the health talk data from the daily closeout dataset
health_talks <- daily_closeout %>%
    select(rct_facility_name, rct_dcr_date, rct_health_talk, rct_flipbook) 
health_talks <- health_talks %>% 
    mutate(rct_health_talk = case_when(
        rct_health_talk == "Yes" ~ 1,
        rct_health_talk == "No" ~ 0,
        TRUE ~ NA_real_
    ),
    rct_flipbook = case_when(
        rct_flipbook == "Yes" ~ 1,
        rct_flipbook == "No" ~ 0,
        TRUE ~ NA_real_
    ))

# Aggregate the number of health talks and flipbook usage per facility per month
health_talks_monthly <- health_talks %>%
    mutate(month = format(rct_dcr_date, "%Y-%m")) %>%
    group_by(rct_facility_name, month) %>%
    summarise(
        `Monthly monthly health talks` = sum(rct_health_talk, na.rm = TRUE),
        `Monthly flipbook usage` = sum(rct_flipbook, na.rm = TRUE),
        `Monthly flipbook usage rate` = (`Monthly flipbook usage` / `Monthly monthly health talks`) * 100,
        .groups = "drop"
    )

#total data
total_health_talks <- health_talks %>%
    group_by(rct_facility_name) %>%
    summarise(
        `Total health talks` = sum(rct_health_talk, na.rm = TRUE),
        `Total flipbook usage` = sum(rct_flipbook, na.rm = TRUE),
        `Total flipbook usage rate` = (`Total flipbook usage` / `Total health talks`) * 100,
        .groups = "drop"
    )
