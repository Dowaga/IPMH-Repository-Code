# Header ------------------------------------------------------------------

# Author(s): Dowaga
# Date: March 16, 2026
# This script is used to analyze the primary outcome data from the PPW RCT study. 
# The generated tables will be included in the DSMB report.

# Setup ------------------------------------------------------------------------
rm(list = ls())
# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")


# Load data----
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

# # Primary and secondary clinical outcomes across times--------------
## PHQ9, GAD7, WHOQOL BREF score 
phq_gad_outcomes <- ppw_rct_df %>%
    select(arm, visit_type, clt_ptid, all_of(starts_with("phq_")), 
           all_of(starts_with("gad7_"))) %>% 
    filter(!is.na(clt_ptid)) 

## PHQ9 & GAD7 scores ------
phq_gad_outcomes <- phq_gad_outcomes %>% 
    mutate(across(c(starts_with("phq_")), 
                  ~ case_when(
                      . == "not at all" ~ 0,
                      . == "several days" ~ 1,
                      . == "more than half the days" ~ 2,
                      . == "nearly every day" ~ 3,
                      TRUE ~ NA_real_
                  ), 
                  .names = "{.col}_number")) %>% 
    mutate(across(starts_with("gad7_"), 
                  ~ case_when(
                      . == "Not at all" ~ 0,
                      . == "Several days" ~ 1,
                      . == "Over half the days" ~ 2,
                      . == "Nearly every day" ~ 3,
                      TRUE ~ NA_real_
                  ), 
                  .names = "{.col}_number")) %>% 
    mutate(phq9_total = rowSums(select(., starts_with("phq_")& ends_with("_number")), na.rm = TRUE),
           gad7_total = rowSums(select(., starts_with("gad7_")& ends_with("_number")), na.rm = TRUE)) %>% 
    mutate(phq9_high = ifelse(phq9_total >= 10, "Yes", "No"),
           gad7_high = ifelse(gad7_total >= 10, "Yes", "No"))

# Select only columns of interest
total_outcome_scores <- phq_gad_outcomes %>%
    filter(visit_type %in% c("Enrollment", "14 Weeks")) %>%
    select(arm, visit_type, clt_ptid, phq9_total, gad7_total)

# Pivot wider 
wide_data <- total_outcome_scores %>%
    pivot_wider(
        id_cols = c(clt_ptid, arm),
        names_from = visit_type,
        values_from = c(phq9_total, gad7_total),
        names_glue = "{.value}_{visit_type}",
        values_fn = mean   # collapse duplicates by mean
    ) %>%
    rename(
        phq9_total_14_Weeks = `phq9_total_14 Weeks`,
        gad7_total_14_Weeks = `gad7_total_14 Weeks`
    ) %>%
    mutate(
        phq9_change = phq9_total_Enrollment - phq9_total_14_Weeks,
        gad7_change = gad7_total_Enrollment - gad7_total_14_Weeks  
    ) %>%  # drop rows with NAs in the total columns
    filter(!is.na(phq9_change) & !is.na(gad7_change))


# Step 1: Compute group means and SDs (rounded to 2 decimals)
summary_stats <- wide_data %>%
    group_by(arm) %>%
    summarise(
        phq9_mean = round(mean(phq9_change, na.rm = TRUE), 2),
        phq9_sd   = round(sd(phq9_change, na.rm = TRUE), 2),
        gad7_mean = round(mean(gad7_change, na.rm = TRUE), 2),
        gad7_sd   = round(sd(gad7_change, na.rm = TRUE), 2),
        .groups = "drop"
    )

# Step 2: Run t-tests
phq9_t <- t.test(phq9_change ~ arm, data = wide_data)
gad7_t <- t.test(gad7_change ~ arm, data = wide_data)

# Step 3: Compute Cohen's d
phq9_d <- cohens_d(phq9_change ~ arm, data = wide_data)
gad7_d <- cohens_d(gad7_change ~ arm, data = wide_data)

# Step 4: Build results tibble
results_table <- tibble::tibble(
    Outcome = c("PHQ9 change", "GAD7 change"),
    `Control Mean Change (SD)` = c(
        sprintf("%.2f (%.2f)", summary_stats$phq9_mean[summary_stats$arm=="Control"],
                summary_stats$phq9_sd[summary_stats$arm=="Control"]),
        sprintf("%.2f (%.2f)", summary_stats$gad7_mean[summary_stats$arm=="Control"],
                summary_stats$gad7_sd[summary_stats$arm=="Control"])
    ),
    `Intervention Mean Change (SD)` = c(
        sprintf("%.2f (%.2f)", summary_stats$phq9_mean[summary_stats$arm=="Intervention"],
                summary_stats$phq9_sd[summary_stats$arm=="Intervention"]),
        sprintf("%.2f (%.2f)", summary_stats$gad7_mean[summary_stats$arm=="Intervention"],
                summary_stats$gad7_sd[summary_stats$arm=="Intervention"])
    ),
    `Mean Difference` = c(
        round(diff(rev(phq9_t$estimate)), 2),
        round(diff(rev(gad7_t$estimate)), 2)
    ),
    `Cohen d` = c(
        round(phq9_d$Cohens_d, 2),
        round(gad7_d$Cohens_d, 2)
    ),
    `p-value` = c(
        signif(phq9_t$p.value, 2),
        signif(gad7_t$p.value, 2)
    )
)

# Step 5: Render as gt table with formatting
results_gt <- results_table %>%
    gt() %>%
    fmt_number(
        columns = c(`p-value`),
        decimals = 3
    ) %>%
    tab_header(
        title = "Intervention vs Control: Change Scores"
    )

results_gt
