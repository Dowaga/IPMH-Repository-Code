# Header ------------------------------------------------------------------

# Author(s): Dowaga
# Date: May 6, 2025
# Baseline Demographics

# Setup ------------------------------------------------------------------------
rm(list = ls())     
# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")

# data prep --------------------------------------------------------------------
referral_df <- ppw_rct_df %>% 
    #filter(clt_visit == "Enrollment") %>% 
    select(record_id, clt_study_site, clt_date,starts_with("risk_"), 
           referral_inf, date_infharm,
           referral_depress, referral_anxiety, referral_ipv, referral_type___0,
           referral_type___1, referral_type___2, referral_type___3, 
           referral_type___4, referral_type___5, referral_type___99, 
           referral_notoffered, referral_notofferedothsp, referral_accept, 
           referral_decline, referral_declineothsp, referral_priorsh, 
           referral_prioractsh___1, referral_prioractsh___2, 
           referral_prioractsh___4, referral_prioractsh___4,
           referral_prioractsh___99, referral_prioractshoth, 
           referral_priorinf, referral_prioractions___1, 
           referral_prioractions___2, referral_prioractions___3, 
           referral_prioractions___4, referral_prioractions___5,
           referral_prioractions___99, referral_prioractoth,
           referral_priorvisit, referral_outcome, referral_help,
           referral_experience, referral_notattend, referral_notattendoth,
           psychosocial_support_referrals_complete)  %>%
    mutate(
    screened_depression = as.integer(referral_depress >= 10),
    screened_anxiety = as.integer(referral_anxiety >= 10),
    screened_IPV = as.integer(referral_ipv >= 10),
    referred = case_when(
        referral_type___1 == "Checked" | referral_type___2 == "Checked" | 
            referral_type___3 == "Checked"|referral_type___4 == "Checked" |
            referral_type___5 == "Checked" |referral_type___99 == "Checked" ~ 1,
        TRUE ~ 0)
    ) %>% 
    mutate(facility_code = as.integer(str_extract(clt_study_site, "^[0-9]{2}")),
        clt_study_site = gsub("^[0-9]+,\\s*", "", clt_study_site),
        Arm = case_when(
            facility_code %in% c(2,5,6,8,11,14,15,18,20,21) ~ "control",
            facility_code %in% c(1,3,4,7,9,13,16,17,19,22) ~ "intervention"
        ))


# Psychosocial Support Referrals QCs
control_psy_qcs <- referral_df %>% 
    filter(referral_anxiety >= 10 | referral_ipv >= 10 | referral_depress>=10) %>% 
    select(record_id, clt_study_site, Arm, starts_with("screened_"), referred, 
           referral_accept) %>% 
    filter(referred == 0) %>% 
    filter(Arm == "control") %>% 
    select(-Arm)

# Select PHQ9 and GAD7 Scores
phq9_gad7_scores <- ppw_rct_df %>% 
    select(record_id, starts_with("phq_"), starts_with("gad7_"))

# Join Qcs with PHQ9 and GAD7 Scores
control_psy_qcs <- control_psy_qcs %>% 
    left_join(phq9_gad7_scores, by  = "record_id")


# Save each clt_study_site as a separate CSV file with date in the filename
control_psy_qcs %>%
    group_split(clt_study_site) %>%
    walk(~ write_xlsx(.x, path = paste0("C:/Users/DAMARIS/Desktop/IPMH/QCs/",
                                        "Psychosocial Support Referral_",
                                       unique(.x$clt_study_site), "_", 
                                       format(Sys.Date(), "%Y-%m-%d"), ".xlsx")))



intervention_psy_qcs <- referral_df %>% 
    filter(referral_anxiety >= 10 | referral_ipv >= 10 | referral_depress>=10) %>% 
    select(record_id, clt_study_site, Arm, starts_with("screened_"), referred, 
           referral_accept) %>% 
    filter(referred == 0) %>% 
    filter(Arm == "intervention")


#-------------------------------------------------------------------------------
# 1. Define a lookup for nicer labels 
condition_labels <- tribble(
    ~risk_col,               ~Condition,
    "screened_depression",   "Depression>=10",
    "screened_anxiety",      "Anxiety>=10",
    "screened_IPV",          "IPV>=10"
)

# 2. Pivot longer & flag accepted
df_long <- referral_df %>%
    # keep only the risk_* + referral_accept columns you need
    select(record_id, starts_with("screened_"), referral_accept) %>%
    pivot_longer(
        cols      = starts_with("screened_"),
        names_to  = "risk_col",
        values_to = "screened_pos"
    ) %>%
    # only those who screened positive
    filter(screened_pos == 1) %>%
    # mark who accepted referral
    mutate(accepted = if_else(referral_accept == "Yes", 1L, 0L))

#  3. Summarise per condition 
total_n <- nrow(ppw_rct_df)    # total participants

summary_tbl <- df_long %>%
    group_by(risk_col) %>%
    reframe(
        n                = n(),                                        # positives
        pct_screened     = n / total_n * 100,                          # % of all
        n_accepted       = sum(accepted, na.rm = TRUE),                # accepted count
        pct_accepted     = n_accepted / n * 100                        # % of screened_pos
    ) %>%
    ungroup() %>%
    # join nicer labels
    left_join(condition_labels, by = "risk_col") %>%
    select(Condition, n, pct_screened, n_accepted, pct_accepted) %>%
    # add totals row
    bind_rows(
        reframe(.,
                  Condition      = "Total",
                  n              = sum(n),
                  pct_screened   = sum(n) / total_n * 100,
                  n_accepted     = sum(n_accepted),
                  pct_accepted   = sum(n_accepted) / sum(n) * 100
        )
    )


# Convert percentages and format table
referral_summary <- summary_tbl %>%
    mutate(
        pct_screened = percent(pct_screened / 100, accuracy = 0.1),
        pct_accepted = percent(pct_accepted / 100, accuracy = 0.1)
    ) %>%
    gt() %>%
    tab_header(title = "Sub Cohort Referral Summary") %>%
    cols_label(
        Referral_Condition = "Referral Condition",
        n_screened = "n Screened",
        pct_screened = "% Screened",
        n_accepted = "n Accepted",
        pct_accepted = "% Accepted"
    ) %>%
    tab_spanner(label = "Screened", columns = c(n_screened, pct_screened)) %>%
    tab_spanner(label = "Accepted", columns = c(n_accepted, pct_accepted)) %>%
    fmt_number(columns = c(n_screened, n_accepted), decimals = 0) %>%
    fmt_percent(columns = c(pct_screened, pct_accepted), decimals = 1) %>%
    opt_table_lines()

# Display table
referral_summary

