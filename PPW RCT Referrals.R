# Header ------------------------------------------------------------------

# Author(s): Dowaga
# Date: May 6, 2025
# Baseline Demographics

# Setup ------------------------------------------------------------------------
# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")

# data prep --------------------------------------------------------------------
referral_df <- ppw_rct_df %>% 
    #filter(clt_visit == "Enrollment") %>% 
    select(record_id, clt_study_site, redcap_event_name, clt_visit, clt_date,starts_with("risk_"), 
           referral_inf, date_infharm,
           referral_depress, referral_anxiety, referral_ipv, referral_type___0,
           referral_type___1, referral_type___2, referral_type___3, 
           referral_type___4, referral_type___5, referral_type___99, 
           referral_notoffered, referral_notofferedothsp, referral_accept, 
           referral_decline, referral_declineothsp, referral_priorsh, 
           referral_prioractsh___1, referral_prioractsh___2, 
           referral_prioractsh___4, referral_prioractsh___99, 
           referral_prioractshoth, 
           referral_priorinf, referral_prioractions___1, 
           referral_prioractions___2, referral_prioractions___3, 
           referral_prioractions___4, referral_prioractions___5,
           referral_prioractions___99, referral_prioractoth,
           referral_priorvisit, referral_outcome, referral_help,
           referral_experience, referral_notattend, referral_notattendoth,
           psychosocial_support_referrals_complete)  %>%
    mutate(
        screened_selfharm = as.integer(risk_selfharm == "Yes"),
    screened_depression = as.integer(referral_depress >= 10),
    screened_anxiety = as.integer(referral_anxiety >= 10),
    screened_IPV = as.integer(referral_ipv >= 10),
    referred = case_when(
        referral_type___1 == "Checked" | referral_type___2 == "Checked" | 
            referral_type___3 == "Checked" | referral_type___4 == "Checked" |
            referral_type___5 == "Checked" |referral_type___99 == "Checked" ~ 1|
            risk_selfharm == "Yes",
        TRUE ~ 0)
    ) %>% 
    mutate(facility_code = as.integer(str_extract(clt_study_site, "^[0-9]{2}")),
        clt_study_site = gsub("^[0-9]+,\\s*", "", clt_study_site),
        Arm = case_when(
                str_detect(redcap_event_name, "Arm 2: Control") ~ "control",
                str_detect(redcap_event_name, "Arm 1: Intervention") ~ "intervention")) %>% 
    filter(!clt_visit %in% c("PM+ Session 5 Abstraction")) %>% 
    mutate(
    clt_visit = factor(
        clt_visit,
        levels = c("Enrollment", "6 weeks post-partum", "14 weeks post-partum", "6 months post-partum")
    )
) %>%
    filter(if_any(starts_with("screened_"), ~ .x == 1))


referral_tbl <- referral_df %>%
    filter(!is.na(Arm)) %>%
    group_by(clt_visit) %>%
    filter(n() > 0) %>%
    ungroup() %>%
            select(Arm, screened_depression, screened_anxiety, 
                   screened_IPV, risk_selfharm) %>%
            tbl_summary(
                by = Arm,
                label = list(
                    screened_depression ~ "Screened Depression (???10)",
                    screened_anxiety    ~ "Screened Anxiety (???10)",
                    screened_IPV        ~ "Screened IPV (???10)",
                    risk_selfharm       ~ "Self-harm/Suicidal behavior"
                ),
                statistic = all_categorical() ~ "{n} / {N} ({p}%)",
                missing = "no"
            ) %>%
            bold_labels()

referral_tbl


# # Psychosocial Support Referrals QCs
# psychosocial_qcs <- referral_df %>% 
#     filter(referral_anxiety >= 10 | referral_ipv >= 10 | referral_depress>=10) %>% 
#     select(record_id, clt_study_site, clt_visit, Arm, starts_with("screened_"), referred, 
#            referral_accept) %>% 
#     filter(referred == 0) %>% 
#     filter(Arm == "intervention")
# 
# # Select PHQ9 and GAD7 Scores
# phq9_gad7_scores <- ppw_rct_df %>% 
#     select(record_id, starts_with("phq_"), starts_with("gad7_"))
# 
# endorsed_suicidality <- ppw_rct_df %>% 
#     select(record_id, clt_study_site, phq_dead,  abs_phq_dead, ae_yn) %>% 
#     filter(!phq_dead %in% c("not at all")) %>% 
#     filter(!is.na(phq_dead))
#     
# # Join Qcs with PHQ9 and GAD7 Scores
# control_psy_qcs <- control_psy_qcs %>% 
#     left_join(phq9_gad7_scores, by  = "record_id")


# Save each clt_study_site as a separate CSV file with date in the filename
# control_psy_qcs %>%
#     group_split(clt_study_site) %>%
#     walk(~ write_xlsx(.x, path = paste0("C:/Users/hp/OneDrive/Desktop/IPMH/QCs/",
#                                         "Psychosocial Support Referral_",
#                                        unique(.x$clt_study_site), "_", 
#                                        format(Sys.Date(), "%Y-%m-%d"), ".xlsx")))
# 


#-------------------------------------------------------------------------------
# 1. Define a lookup for nicer labels 
condition_labels <- tribble(
    ~risk_col,               ~Condition,
    "screened_depression",   "Depression>=10",
    "screened_anxiety",      "Anxiety>=10",
    "screened_IPV",          "IPV>=10",
    "screened_selfharm",     "Self-harm"
)

# 2. Pivot longer & flag accepted
df_long <- referral_df %>%
    # keep only the risk_* + referral_accept columns you need
    select(record_id, Arm, starts_with("screened_"), referral_accept) %>%
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
    screened = sprintf("%d (%.1f%%)", n, pct_screened),
    accepted = sprintf("%d (%.1f%%)", n_accepted, pct_accepted)
  ) %>%
  select(Condition, screened, accepted) %>%
  gt() %>%
    tab_header(title = "Referral Summary") %>%
    cols_label(
        Condition = "Referral Condition",
        screened  = "Screened Positive N (%)",
        accepted  = "Accepted Referral N (%)") %>%
    fmt_number(columns = starts_with("n"), decimals = 0) %>%
    tab_options(
        table.font.size = px(11),      # control font size inside the table
        data_row.padding = px(3),      # padding inside cells
        column_labels.font.size = px(12),
        table.width = pct(95))  %>%        # ensure table fits the page
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels()) %>% 
        opt_table_lines()


# Display table
referral_summary

# Referral by Arm
arm_summary_tbl <- df_long %>%
    group_by(risk_col, Arm) %>%
    reframe(
        n = n(),
        n_accepted = sum(accepted, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    left_join(condition_labels, by = "risk_col")

arm_summary_wide <- arm_summary_tbl %>%
    pivot_wider(
        names_from = Arm,
        values_from = c(n, n_accepted),
        names_glue = "{.value}_{Arm}"
    ) %>%
    mutate(
        n_total = coalesce(n_control, 0) + coalesce(n_intervention, 0),
        n_accepted_total = coalesce(n_accepted_control, 0) + coalesce(n_accepted_intervention, 0),
        uptake_control = if_else(n_control > 0, n_accepted_control / n_control * 100, 0),
        uptake_intervention = if_else(n_intervention > 0, n_accepted_intervention / n_intervention * 100, 0),
        uptake_total = if_else(n_total > 0, n_accepted_total / n_total * 100, 0)
    )

arm_referral_summary <- arm_summary_wide %>%
    select(-risk_col) %>%
    mutate(
        accepted_control = sprintf("%d", n_accepted_control),
        accepted_intervention = sprintf("%d", n_accepted_intervention),
        accepted_total = sprintf("%d", n_accepted_total),
        uptake_control_fmt = sprintf("%.1f%%", uptake_control),
        uptake_intervention_fmt = sprintf("%.1f%%", uptake_intervention),
        uptake_total_fmt = sprintf("%.1f%%", uptake_total)
    ) %>%
    select(
        Condition,
        n_control, accepted_control, uptake_control_fmt,
        n_intervention, accepted_intervention, uptake_intervention_fmt,
        n_total, accepted_total, uptake_total_fmt
    ) %>%
    gt() %>%
    tab_header(title = "Referral Summary by Arm") %>%
    cols_label(
        Condition = "Referral Condition",
        n_control = "Screened Positive",
        accepted_control = "Accepted Referral",
        uptake_control_fmt = "Uptake",
        n_intervention = "Screened Positive",
        accepted_intervention = "Accepted Referral",
        uptake_intervention_fmt = "Uptake",
        n_total = "Screened Positive",
        accepted_total = "Accepted Referral",
        uptake_total_fmt = "Uptake"
    ) %>%
    # Add spanners for grouped headers
    tab_spanner(
        label = "Control",
        columns = c(n_control, accepted_control, uptake_control_fmt)
    ) %>%
    tab_spanner(
        label = "Intervention",
        columns = c(n_intervention, accepted_intervention, uptake_intervention_fmt)
    ) %>%
    tab_spanner(
        label = "Overall Referral",
        columns = c(n_total, accepted_total, uptake_total_fmt)
    ) %>%
    fmt_number(columns = starts_with("n"), decimals = 0) %>%
    tab_options(
        table.font.size = px(11),
        data_row.padding = px(3),
        column_labels.font.size = px(12),
        table.width = pct(95)
    ) %>%
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels()
    ) %>%
    opt_table_lines()

arm_referral_summary

