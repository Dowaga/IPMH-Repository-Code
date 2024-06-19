rm(list = ls())
getwd() 
pacman::p_load(knitr, tidyverse, ggplot2, psych, janitor, dplyr, 
               openxlsx, gtsummary, kableExtra) 

hcw <- demographic_df %>%
  select(record_id, starts_with("hcw_"), demographic_hcws_complete) %>%
  filter(!is.na(hcw_participant_id))

# Look at number of NAs and datatype for each variable
# hcw %>% df_status() %>% select(variable, q_na, type)

hcw <- hcw %>% 
  mutate(hcw_sex = dplyr::recode(hcw_sex, "1" = "Male","2" = "Female")) %>% 
  mutate(hcw_education = dplyr::recode(hcw_education, 
                                       "1" = "No schooling completed",
                                       "2" = "Primary school",
                                       "3" = "Secondary school", 
                                       "4" = "Diploma", 
                                       "5" = "Bachelor's degree or higher degree")) %>%
  mutate(hcw_job = dplyr::recode(hcw_job, "1" = "Medical officer", 
                                 "2" = "Psychiatrists", 
                                 "3" = "Obstetrician/gynaecologist",
                                 "4" = "Nurses", "5" = "Psychologist", 
                                 "6" = "Mental health social worker", 
                                 "7" = "Other social worker", 
                                 "8" = "HTS counsellor", 
                                 "9" = "Community health worker", 
                                 "10" = "Mentor mother", 
                                 "11" = "Other",
                                 "12" = "Clinical Officer")) %>% 
  mutate(hcw_wlwh = dplyr::recode(hcw_wlwh, "0" = "No","1" = "Yes")) %>% 
  mutate(hcw_mh_provision = dplyr::recode(hcw_mh_provision, "0" = "No", 
                                          "1" = "Yes")) %>% 
  mutate(hcw_terms = dplyr::recode(hcw_terms, 
                                   "1" = "Permanent and pensionable", 
                                   "2" = "Contract","3" = "Volunteer", 
                                   "4" = "Other")) %>% 
  mutate(hcw_facility = dplyr::recode(hcw_facility, 
                                   "1" =	"Rwambwa Sub-county Hospital",
                                   "2" = 	"Sigomere Sub County Hospital",
                                   "3" =	"Uyawi Sub County Hospital",
                                   "4" = "Got Agulu Sub-District Hospital",
                                   "5" = "Ukwala Sub County Hospital",
                                   "6" = "Madiany Sub County Hospital",
                                   "7" =	"Kabondo Sub County Hospital",
                                   "8" = "Mbita Sub-County Hospital",
                                   "9" = "Miriu Health Centre",
                                   "10"	= "Pala Health Centre",
                                   "11" =	"Nyandiwa Level IV Hospital",
                                   "12" =	"Magunga Level IV Hospital",
                                   "13" = "Ober Kamoth Sub County Hospital",
                                   "14" = "Gita Sub County Hospital",
                                   "15" =	"Akala Health Centre",
                                   "16" =	"Usigu Health Centre",
                                   "17" = "Ramula Health Centre",
                                   "18" =	"Simenya Health Centre",
                                   "19" =	"Airport Health Centre (Kisumu)",
                                   "20" =	"Nyalenda Health Centre")) %>% 
  mutate(hcw_hrscare = as.numeric(hcw_hrscare)) %>% 
  mutate(hcw_yearjob = hcw_yrsjob/12) %>% 
  mutate(hcw_yearwork = hcw_monthswork/12) %>% 
  mutate(hcw_yearcare = hcw_yrscare/12)

#basic demo table
hcw_tbl1 <- hcw %>% tbl_summary(
  include=c(hcw_age, hcw_sex, hcw_education, hcw_dpt___1, hcw_dpt___2,
            hcw_dpt___3, hcw_dpt___4, hcw_dpt___5, hcw_dpt___6, hcw_job, 
            hcw_yearjob,  hcw_facility, hcw_yearwork, hcw_terms, hcw_yearcare,
            hcw_hrscare, hcw_wlwh, hcw_mh_provision),
  label = list(hcw_age ~ "Age (Years)",
               hcw_sex ~ "Gender",
               hcw_education ~ "Education level",
               hcw_dpt___1 ~ "Working in antenatal care",
               hcw_dpt___2 ~ "Working in postnatal care",
               hcw_dpt___3 ~ "Working in immunization clinic",
               hcw_dpt___4 ~ "Working in PMTCT",
               hcw_dpt___5 ~ "Working in family planning clinic",
               hcw_dpt___6 ~ "Working in other department",
               hcw_job ~ "Current Designation",
               hcw_yearjob ~ "Current job duration (Years)",
               hcw_facility ~ "Current facility",
               hcw_yearwork ~ "Duration in the facility (Years)",
               hcw_terms ~ "Terms of engagement in the facility",
               hcw_yearcare ~ "Duration offering care to PW in the facility (Years)",
               hcw_hrscare ~ "Hours/week taking care of PW in the facility", 
               hcw_wlwh ~ "Providing care to PW living with HIV",
               hcw_mh_provision ~ "Provided MH services to PW"),
  missing = "no",
  digits = list(all_continuous() ~ 1), 
  type = list(hcw_hrscare ~ "continuous"),
  statistic = list(all_continuous() ~ "{mean} Â± {sd}")) %>% 
  modify_caption("**Basic Demographic Summary for HCWs**") %>%
  bold_labels() 

hcw_tbl1

# table 2
hcw_mh <- hcw %>%
  filter(hcw_mh_provision == "Yes") %>% 
  select(hcw_mh_service___1, hcw_mh_service___2, hcw_mh_service___3,
         hcw_mh_service___4,hcw_mh_service___5,hcw_mh_service___6,
         hcw_mh_service___7, hcw_screen, hcw_diag, hcw_refer, hcw_therapy,
         hcw_med, hcw_monitor, hcw_service_other) %>% 
  mutate(hcw_sreen_year = hcw_screen/12,
         hcw_diag_year = hcw_diag/12,
         hcw_refer_year = hcw_refer/12,
         hcw_therapy_year = hcw_therapy/12,
         hcw_med_year = hcw_med/12,
         hcw_monitor_year = hcw_monitor/12,
         hcw_other_year = hcw_service_other/12)

services <- list(
  "Screening" = c("hcw_mh_service___1", "hcw_sreen_year"),
  "Diagnosis" = c("hcw_mh_service___2", "hcw_diag_year"),
  "Talk therapies" = c("hcw_mh_service___3", "hcw_therapy_year"),
  "Prescribing medication" = c("hcw_mh_service___4", "hcw_med_year"),
  "Ongoing monitoring" = c("hcw_mh_service___5", "hcw_monitor_year"),
  "Referral" = c("hcw_mh_service___6", "hcw_refer_year"),
  "Other" = c("hcw_mh_service___7", "hcw_other_year")
)

hcw_tbl2 <- data.frame(Service = character(), 
                      `N (Percentage)` = character(), 
                      `Average Duration (Mean [SD])` = character(), 
                      stringsAsFactors = FALSE)

# Loop through each service to calculate the stats
for (service_name in names(services)) {
  service_info <- services[[service_name]]
  N <- sum(!is.na(hcw_mh[[service_info[1]]]) & hcw_mh[[service_info[1]]] == 1, na.rm = TRUE)
  total_providers <- nrow(hcw_mh)
  percentage_providers <- (N / total_providers) * 100
  N_percentage <- sprintf("%d (%.2f%%)", N, percentage_providers)
  duration_mean <- mean(hcw_mh[[service_info[2]]], na.rm = TRUE)
  duration_sd <- sd(hcw_mh[[service_info[2]]], na.rm = TRUE)
  duration_mean_sd <- sprintf("%.2f (%.2f)", duration_mean, duration_sd)
  hcw_tbl2 <- rbind(hcw_tbl2, 
                   data.frame(Service = service_name, 
                              `N (Percentage)` = N_percentage, 
                              `Average Duration (Mean [SD])` = duration_mean_sd))
}


# Print the final table
kable(hcw_tbl2, col.names = c("Service", 
                             "N (Percentage)", 
                             "Average Duration (Mean [SD]) [Year]"),
      caption = "MH Service Provision")
