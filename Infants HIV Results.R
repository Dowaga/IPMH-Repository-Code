# Header ------------------------------------------------------------------

# Author(s): Dowaga
# Date: September 01, 2025
# This script is used to analyze HIV negative infants from WLHIV not on Prophylaxix. 
# The generated records will be used as QCs to field team to follow up.

# Setup ------------------------------------------------------------------------

rm(list = ls())

# read data
source("Datateam_ipmh.R")
source("Dependencies.R")
source("data_import.R")

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

# WLHIV
ppw_lwhiv <- ppw_rct_df %>% 
    filter(med_pastdiag___2 == "Checked") %>% 
    select(clt_ptid, med_pastdiag___2)

pre_outcome <- ppw_rct_df %>% 
    filter(visit_type == "6 Weeks") %>% 
    select(clt_ptid, starts_with("tpnc_"))


library(labelled)

hiv_proxilaxis <- ppw_lwhiv %>% 
    left_join(pre_outcome, by = "clt_ptid") %>% 
    #filter(tpnc_ihiv == "Negative") %>% 
    set_variable_labels(tpnc_iarv = "Infant given ARV Prophylaxis",
                        tpnc_icorti = "Infant Cotrimoxazole Prophylaxis initiated")


# Infant Test results form
infant_results <- ppw_rct_df %>%
    filter(visit_type == "6 Weeks") %>% 
    select(clt_ptid, nda_pcr_hiv, starts_with("bhiv_"), 
           infant_hiv_results_form_complete) %>% 
    filter(nda_pcr_hiv == "Yes") %>% 
    filter(bhiv_test_result == "Results not available yet")

# 
pre_infant_results <- hiv_proxilaxis %>% 
    filter(tpnc_ended == "Yes" & tpnc_lb == "Yes") %>% 
    select(clt_ptid, tpnc_ihiv, tpnc_ihiv_2) %>% 
    left_join(infant_results %>% 
                  select(clt_ptid, bhiv_test_date, bhiv_test_method, bhiv_test_result),
              by = "clt_ptid")
    
infant_results_QCs <- pre_infant_results %>% 
    filter(tpnc_ihiv %in% c ("Negative", "Positive"))

# Save to Excel in the working directory
write.xlsx(
    infant_results_QCs,
    file = paste0("Infant Results QCs ", Sys.Date(), ".xlsx")
)

# infants HIV results for HIV none reactive women
ppw_noner_hiv <- ppw_rct_df %>% 
    filter(visit_type == "6 Weeks") %>% 
    filter(med_pastdiag___2 == "Unchecked") %>% 
    select(clt_ptid, med_pastdiag___2)

pre_outcome <- ppw_rct_df %>% 
    filter(visit_type == "6 Weeks") %>% 
    select(clt_ptid, starts_with("tpnc_"))


library(labelled)

hiv_proxilaxis <- ppw_noner_hiv %>% 
    left_join(pre_outcome, by = "clt_ptid") %>% 
    #filter(tpnc_ihiv == "Negative") %>% 
    set_variable_labels(tpnc_iarv = "Infant given ARV Prophylaxis",
                        tpnc_icorti = "Infant Cotrimoxazole Prophylaxis initiated")


# Infant Test results form
infant_results <- ppw_rct_df %>%
    filter(visit_type == "6 Weeks") %>% 
    select(clt_ptid, nda_pcr_hiv, starts_with("bhiv_"), 
           infant_hiv_results_form_complete) %>% 
    filter(nda_pcr_hiv == "Yes") %>% 
    filter(bhiv_test_result == "Results not available yet")

# 
pre_infant_results <- hiv_proxilaxis %>% 
    filter(tpnc_ended == "Yes" & tpnc_lb == "Yes") %>% 
    select(clt_ptid, tpnc_ihiv, tpnc_ihiv_2) %>% 
    left_join(infant_results %>% 
                  select(clt_ptid, bhiv_test_date, bhiv_test_method, bhiv_test_result),
              by = "clt_ptid")

infant_results_QCs <- pre_infant_results %>% 
    filter(tpnc_ihiv %in% c ("Negative", "Positive"))

# Save to Excel in the working directory
write.xlsx(
    infant_results_QCs,
    file = paste0("Infant Results QCs ", Sys.Date(), ".xlsx")
)


