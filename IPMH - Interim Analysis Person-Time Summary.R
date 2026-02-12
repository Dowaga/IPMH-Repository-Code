
# Header ------------------------------------------------------------------

# Author(s): D. Owaga
# Date: Feb 11, 2026
# This script is used to estimating total expected person-time, 50% expected person-time, and current total person-time elapsed to inform interim analysis timing. 

# Setup ------------------------------------------------------------------------
rm(list = ls())
# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")

# RCT Data data
ppw_rct_df <- ppw_rct_df %>% 
    mutate(
        visit_type = case_when(
            grepl("Enrollment", redcap_event_name) ~ "Enrollment",
            grepl("6 Weeks", redcap_event_name) ~ "6 Weeks",
            grepl("14 Weeks", redcap_event_name) ~ "14 Weeks", 
            grepl("6 Months", redcap_event_name) ~ "6 Months",
            TRUE ~ NA_character_
        ))

# ---- Estimating total expected person-time ----
 ## Enrollment GA dataset
enrollment_GA <- ppw_rct_df%>% 
    filter(visit_type == "Enrollment") %>% 
    select(clt_ptid,med_pre_gestage_current) %>% 
    rename(enroll_gestage = med_pre_gestage_current)


## Delivery GA dataset
baseline_lmp <- ppw_rct_df %>%
    select(clt_ptid, med_lmp) %>% 
    filter(!is.na(med_lmp))

delivery_GA <- ppw_rct_df %>%
    filter(visit_type == "6 Weeks") %>% 
    select(clt_ptid,tpnc_ended, tpnc_lb, tpnc_date, 
           tpnc_gestage, tpnc_gestfill) %>% 
    filter(!is.na(clt_ptid))

delivery_GA <- delivery_GA %>%
    left_join(baseline_lmp, by = "clt_ptid") %>% 
    mutate(
        tpnc_date = as.Date(tpnc_date),
        med_lmp = as.Date(med_lmp),
        gestage_calculated = case_when(
            !is.na(tpnc_date) & !is.na(med_lmp) ~ 
                as.numeric(tpnc_date - med_lmp) / 7,
            TRUE ~ NA_real_
        ),
        tpnc_gestfill_numeric = as.numeric(tpnc_gestfill),
        # Final gestational age: use calculated if available, otherwise use tpnc_gestfill
        deliv_gestage = case_when(
            !is.na(gestage_calculated) ~ gestage_calculated,
            !is.na(tpnc_gestfill_numeric) ~ tpnc_gestfill_numeric,
            TRUE ~ NA_real_
        )
    ) %>% 
    select(clt_ptid, deliv_gestage)%>%
    filter(!is.na(deliv_gestage))

# Step 1: Mean gestational age at enrollment
mean_enrollment <- enrollment_GA %>%
    summarise(avg_enroll = mean(enroll_gestage, na.rm = TRUE)) %>%
    pull(avg_enroll)

# Step 2: Mean gestational age at delivery
mean_delivery <- delivery_GA %>%
    summarise(avg_delivery = mean(deliv_gestage, na.rm = TRUE)) %>%
    pull(avg_delivery)

# Step 3: Estimated total time from enrollment to delivery
time_enroll_to_delivery <- round(mean_delivery - mean_enrollment, 2)

# Step 4: Add 14 weeks postpartum
time_enroll_to_pp14 <- time_enroll_to_delivery + 14

# Step 5: Multiply by total sample size (2970 participants)
total_expected_person_time <- time_enroll_to_pp14 * 2970

# ---- Estimating 50% expected person-time ----
half_expected_person_time <- 0.5 * total_expected_person_time


# ---- Calculating current total person-time elapsed ----
today <- Sys.Date()

# Step 1: Among those currently enrolled, calculate: today's_date - enrollment_date 

#Step 2: Among those discharged, calculate: discharge_date - enrollment_date 
# Enrollment dates dataset
enroll_date <- ppw_rct_df %>%
    filter(visit_type == "Enrollment") %>% 
    select(clt_ptid, clt_date)

# Discharged dates dataset
discharged <- ppw_rct_df %>% 
    filter(dis_today == "Yes") %>% 
    filter(str_detect(dis_reason, "Study End")) %>% 
    select(clt_ptid, dis_date, dis_reason)

# Merge and replace missing dis_date with today's date
merged_df <- enroll_date %>% 
    left_join(discharged, by = "clt_ptid") %>% 
    mutate(
        # Convert both dates to Date type
        clt_date = as.Date(clt_date),
        dis_date = as.Date(dis_date),
        # Replace missing dis_date with today's date
        dis_date = if_else(is.na(dis_date), today, dis_date),
        # Calculate person-time in weeks
        person_time = as.numeric(dis_date - clt_date) / 7
    )


current_total_person_time <- sum(merged_df$person_time, na.rm = TRUE)

# ---- Compare current vs 50% expected ----

# Build summary table
summary_table <- tibble(
    Metric = c(
        "Mean enrollment GA (weeks)",
        "Mean delivery GA (weeks)",
        "Estimated time from enrollment to delivery (weeks)",
        "Expected total time per participant (weeks)",
        "Total expected person-time (weeks)",
        "50% expected person-time (weeks)",
        "Current total person-time elapsed (weeks)",
        "Interim analysis threshold status"
    ),
    Value = c(
        round(mean_enrollment, 1),
        round(mean_delivery, 1),
        time_enroll_to_delivery,
        time_enroll_to_pp14,
        round(total_expected_person_time),
        round(half_expected_person_time),
        round(current_total_person_time),
        ifelse(current_total_person_time >= half_expected_person_time,
               "**Reached**", "**Not yet reached**") # bold status
    )
)

# Create gt table with heading
pertime_table <- summary_table %>%
    gt() %>%
    tab_header(
        title = "Interim Analysis Person-Time Summary"
    ) %>%
    # Interpret markdown so bolding works
    fmt_markdown(columns = "Value")

pertime_table


