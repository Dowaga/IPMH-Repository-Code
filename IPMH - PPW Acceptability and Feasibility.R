# Header ------------------------------------------------------------------

# Author(s): Dowaga
# Date: September 30, 2025
# Description: This script is used to generate data reports for IPMH Aceptability, Adaptability and Feasibility of IPMH activities

# Setup ------------------------------------------------------------------------
rm(list = ls())

# read data
source("Datateam_ipmh.R")
source("Dependencies.R")
source("data_import.R")


# Pull the required data
accep_feas_dt <- ppw_rct_df %>%
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
    ) %>% 
    select(clt_ptid, visit_type, starts_with("af_") ) %>% 
    filter(visit_type %in% c("6 Weeks", "14 Weeks", "6 Months"))

# Number in each visit
totals <- accep_feas_dt %>% 
    tabyl(visit_type)

# Define level recoding
accep_labels <- c(
    "Completely agree" = 5,
    "Agree" = 4,
    "Neither agree not disagree" = 3,
    "Disagree" = 2,
    "Completely disagree" = 1
)

# Recode variables
accep_dt <- accep_feas_dt %>%
    select(clt_ptid, visit_type, af_meets, af_appeal, af_enjoy, af_welcome, af_use, 
           af_seems, af_doable, af_easy, af_life, af_suitable, 
           af_applicable, af_match) %>% 
    mutate(across(c(af_meets, af_appeal, af_enjoy, af_welcome, af_use, 
                    af_seems, af_doable, af_easy, af_life, af_suitable, 
                    af_applicable, af_match), 
                  ~ recode(., !!!accep_labels)))

long_data <- accep_dt %>%
    pivot_longer(cols = -c(clt_ptid, visit_type), names_to = "item", values_to = "response")

summary_table <- long_data %>%
    group_by(visit_type, item) %>%
    summarise(
        mean = round(mean(response, na.rm = TRUE), 2),
        sd = round(sd(response, na.rm = TRUE), 2),
        .groups = "drop"
    ) %>%
    mutate(mean_sd = paste0(mean, " (", sd, ")"))

summary_table <- summary_table %>%
    mutate(
        Domain = case_when(
            item %in% c("af_meets", "af_appeal", "af_enjoy", "af_welcome") ~ "Acceptability",
            item %in% c("af_applicable", "af_suitable", "af_match") ~ "Appropriateness",
            item %in% c("af_use", "af_seems", "af_doable", "af_easy", "af_life") ~ "Feasibility"
        ),
        Item = case_when(
            item == "af_meets" ~ "IPMH program meets my approval",
            item == "af_appeal" ~ "IPMH program is appealing to me",
            item == "af_enjoy" ~ "I would enjoy/like the IPMH program",
            item == "af_welcome" ~ "I welcome the IPMH program",
            item == "af_applicable" ~ "IPMH seems applicable",
            item == "af_suitable" ~ "IPMH seems suitable",
            item == "af_match" ~ "IPMH seems like a good match",
            item == "af_use"~ "The IPMH program seems like something I could use",
            item == "af_seems" ~ "The IPMH program seems possible",
            item == "af_doable" ~ "The IPMH program seems doable",
            item == "af_easy" ~ "The IPMH program seems easy to use",
            item == "af_life" ~ "The IPMH program is something I can easily fit into my life"
        )
    ) 

summary_wide <- summary_table %>%
    select(Domain, Item, visit_type, mean_sd) %>%
    pivot_wider(names_from = visit_type, values_from = mean_sd)%>% 
    select(Domain, Item, `6 Weeks`, `14 Weeks`, `6 Months`)


# Compute Overall Mean(SD) per Domain----
domain_overall <- long_data %>%
    mutate(
        Domain = case_when(
            item %in% c("af_meets", "af_appeal", "af_enjoy", "af_welcome") ~ "Acceptability",
            item %in% c("af_applicable", "af_suitable", "af_match") ~ "Appropriateness",
            item %in% c("af_use", "af_seems", "af_doable", "af_easy", "af_life") ~ "Feasibility"
        )
    ) %>%
    group_by(Domain, visit_type) %>%
    summarise(
        mean = round(mean(response, na.rm = TRUE), 2),
        sd = round(sd(response, na.rm = TRUE), 2),
        .groups = "drop"
    ) %>%
    mutate(
        Item = "Overall",
        mean_sd = paste0(mean, " (", sd, ")")
    ) %>%
    select(Domain, Item, visit_type, mean_sd)

domain_overall_wide <- domain_overall %>%
    pivot_wider(names_from = visit_type, values_from = mean_sd) %>%
    select(Domain, Item, `6 Weeks`, `14 Weeks`, `6 Months`)

summary_wide_final <- bind_rows(summary_wide, domain_overall_wide)


summary <- summary_wide_final %>%
    gt(groupname_col = "Domain") %>%
    tab_header(
        title = md("**PPW - IPMH Program Ratings by Domain and Time Point**"),
        subtitle = "Ratings collected at 6 Weeks, 14 Weeks, and 6 Months post-enrollment"
    ) %>%
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_row_groups()
    ) %>%
    tab_style(
        style = cell_text(weight = "bold", style = "italic"),
        locations = cells_body(rows = Item == "Overall")
    ) %>%
    tab_footnote(
        footnote = "Ratings based on a 5-point Likert scale: 1 = Completely Disagree to 5 = Completely Agree. 'Overall' rows represent domain-level averages across items.",
        locations = cells_title(groups = "title")
    )

summary


# Subset screening acceptability
screening_acceptability <- accep_feas_dt %>% 
    select(clt_ptid, visit_type, af_screen_yesno, ends_with("_sc")) %>% 
    filter(af_screen_yesno == "Yes")%>% 
    select(-af_screen_yesno) %>%
    mutate(across(c(af_meets_sc, af_appeal_sc, af_enjoy_sc, af_welcome_sc, af_use_sc, 
                    af_seems_sc, af_doable_sc, af_easy_sc, af_life_sc, af_suitable_sc, 
                    af_applicable_sc, af_match_sc), 
                  ~ recode(., !!!accep_labels)))

screen_long_data <- screening_acceptability %>%
    pivot_longer(cols = -c(clt_ptid, visit_type), names_to = "item", values_to = "response")

screen_summary_table <- screen_long_data %>%
    group_by(visit_type, item) %>%
    summarise(
        mean = round(mean(response, na.rm = TRUE), 2),
        sd = round(sd(response, na.rm = TRUE), 2),
        .groups = "drop"
    ) %>%
    mutate(mean_sd = paste0(mean, " (", sd, ")"))

screen_summary_table <- screen_summary_table %>%
    mutate(
        Domain = case_when(
            item %in% c("af_meets_sc", "af_appeal_sc", "af_enjoy_sc", "af_welcome_sc") ~ "Acceptability",
            item %in% c("af_applicable_sc", "af_suitable_sc", "af_match_sc") ~ "Appropriateness",
            item %in% c("af_use_sc", "af_seems_sc", "af_doable_sc", "af_easy_sc", "af_life_sc") ~ "Feasibility"
        ),
        Item = case_when(
            item == "af_meets_sc" ~ "IPMH program MH screening meets my approval",
            item == "af_appeal_sc" ~ "IPMH program MH screening is appealing to me",
            item == "af_enjoy_sc" ~ "I would enjoy/like the IPMH program MH screening",
            item == "af_welcome_sc" ~ "I welcome the IPMH program MH screening",
            item == "af_applicable_sc" ~ "IPMH MH screening seems applicable",
            item == "af_suitable_sc" ~ "IPMH MH screening seems suitable",
            item == "af_match_sc" ~ "IPMH MH screening seems like a good match",
            item == "af_use_sc"~ "IPMH program MH screening seems like something I could use",
            item == "af_seems_sc" ~ " IPMH program MH screening seems possible",
            item == "af_doable_sc" ~ "IPMH program MH screening seems doable",
            item == "af_easy_sc" ~ "IPMH program MH screening seems easy to use",
            item == "af_life_sc" ~ "IPMH program MH screening is something I can easily fit into my life"
        )
    ) 

screen_summary_wide <- screen_summary_table %>%
    select(Domain, Item, visit_type, mean_sd) %>%
    pivot_wider(names_from = visit_type, values_from = mean_sd)%>% 
    select(Domain, Item, `6 Weeks`, `14 Weeks`, `6 Months`)


# Compute Overall Mean(SD) per Domain----
screen_domain_overall <- screen_long_data %>%
    mutate(
        Domain = case_when(
            item %in% c("af_meets_sc", "af_appeal_sc", "af_enjoy_sc", "af_welcome_sc") ~ "Acceptability",
            item %in% c("af_applicable_sc", "af_suitable_sc", "af_match_sc") ~ "Appropriateness",
            item %in% c("af_use_sc", "af_seems_sc", "af_doable_sc", "af_easy_sc", "af_life_sc") ~ "Feasibility"
        )
    ) %>%
    group_by(Domain, visit_type) %>%
    summarise(
        mean = round(mean(response, na.rm = TRUE), 2),
        sd = round(sd(response, na.rm = TRUE), 2),
        .groups = "drop"
    ) %>%
    mutate(
        Item = "Overall",
        mean_sd = paste0(mean, " (", sd, ")")
    ) %>%
    select(Domain, Item, visit_type, mean_sd)

screen_domain_overall_wide <- screen_domain_overall %>%
    pivot_wider(names_from = visit_type, values_from = mean_sd) %>%
    select(Domain, Item, `6 Weeks`, `14 Weeks`, `6 Months`)

screen_summary_wide_final <- bind_rows(screen_summary_wide, screen_domain_overall_wide)


screen_summary <- screen_summary_wide_final %>%
    gt(groupname_col = "Domain") %>%
    tab_header(
        title = md("**PPW - IPMH Screening Ratings by Domain and Time Point**"),
        subtitle = "Ratings collected at 6 Weeks, 14 Weeks, and 6 Months post-enrollment"
    ) %>%
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_row_groups()
    ) %>%
    tab_style(
        style = cell_text(weight = "bold", style = "italic"),
        locations = cells_body(rows = Item == "Overall")
    ) %>%
    tab_footnote(
        footnote = "Ratings based on a 5-point Likert scale: 1 = Completely Disagree to 5 = Completely Agree. 'Overall' rows represent domain-level averages across items.",
        locations = cells_title(groups = "title")
    )

screen_summary

# Subset PM+ acceptability
pm_acceptability <- accep_feas_dt %>% 
    select(clt_ptid, visit_type, af_pm_yesno, ends_with("_pm")) %>% 
    filter(af_pm_yesno == "Yes")%>% 
    select(-af_pm_yesno) %>%
    mutate(across(c(af_meets_pm, af_appeal_pm, af_enjoy_pm, af_welcome_pm, af_use_pm, 
                    af_seems_pm, af_doable_pm, af_easy_pm, af_life_pm, af_suitable_pm, 
                    af_applicable_pm, af_match_pm), 
                  ~ recode(., !!!accep_labels)))

pm_long_data <- pm_acceptability %>%
    pivot_longer(cols = -c(clt_ptid, visit_type), names_to = "item", values_to = "response")

pm_summary_table <- pm_long_data %>%
    group_by(visit_type, item) %>%
    summarise(
        mean = round(mean(response, na.rm = TRUE), 2),
        sd = round(sd(response, na.rm = TRUE), 2),
        .groups = "drop"
    ) %>%
    mutate(mean_sd = paste0(mean, " (", sd, ")"))

pm_summary_table <- pm_summary_table %>%
    mutate(
        Domain = case_when(
            item %in% c("af_meets_pm", "af_appeal_pm", "af_enjoy_pm", "af_welcome_pm") ~ "Acceptability",
            item %in% c("af_applicable_pm", "af_suitable_pm", "af_match_pm") ~ "Appropriateness",
            item %in% c("af_use_pm", "af_seems_pm", "af_doable_pm", "af_easy_pm", "af_life_pm") ~ "Feasibility"
        ),
        Item = case_when(
            item == "af_meets_pm" ~ "IPMH program PM Plus therapy meets my approval",
            item == "af_appeal_pm" ~ "IPMH program PM Plus therapy is appealing to me",
            item == "af_enjoy_pm" ~ "I would enjoy/like the IPMH program PM Plus therapy",
            item == "af_welcome_pm" ~ "I welcome the IPMH program PM Plus therapy",
            item == "af_applicable_pm" ~ "IPMH PM Plus therapy seems applicable",
            item == "af_suitable_pm" ~ "IPMH PM Plus therapy seems suitable",
            item == "af_match_pm" ~ "IPMH PM Plus therapy seems like a good match",
            item == "af_use_pm"~ "IPMH program PM Plus therapy seems like something I could use",
            item == "af_seems_pm" ~ "IPMH program PM Plus therapy seems possible",
            item == "af_doable_pm" ~ "IPMH program PM Plus therapy seems doable",
            item == "af_easy_pm" ~ "IPMH program PM Plus therapy seems easy to use",
            item == "af_life_pm" ~ "IPMH program PM Plus therapy is something I can easily fit into my life"
        )
    ) 

pm_summary_wide <- pm_summary_table %>%
    select(Domain, Item, visit_type, mean_sd) %>%
    pivot_wider(names_from = visit_type, values_from = mean_sd)%>% 
    select(Domain, Item, `6 Weeks`, `14 Weeks`, `6 Months`)


# Compute Overall Mean(SD) per Domain----
pm_domain_overall <- pm_long_data %>%
    mutate(
        Domain = case_when(
            item %in% c("af_meets_pm", "af_appeal_pm", "af_enjoy_pm", "af_welcome_pm") ~ "Acceptability",
            item %in% c("af_applicable_pm", "af_suitable_pm", "af_match_pm") ~ "Appropriateness",
            item %in% c("af_use_pm", "af_seems_pm", "af_doable_pm", "af_easy_pm", "af_life_pm") ~ "Feasibility"
        )
    ) %>%
    group_by(Domain, visit_type) %>%
    summarise(
        mean = round(mean(response, na.rm = TRUE), 2),
        sd = round(sd(response, na.rm = TRUE), 2),
        .groups = "drop"
    ) %>%
    mutate(
        Item = "Overall",
        mean_sd = paste0(mean, " (", sd, ")")
    ) %>%
    select(Domain, Item, visit_type, mean_sd)

pm_domain_overall_wide <- pm_domain_overall %>%
    pivot_wider(names_from = visit_type, values_from = mean_sd) %>%
    select(Domain, Item, `6 Weeks`, `14 Weeks`, `6 Months`)

pm_summary_wide_final <- bind_rows(pm_summary_wide, pm_domain_overall_wide)


pm_summary <- pm_summary_wide_final %>%
    gt(groupname_col = "Domain") %>%
    tab_header(
        title = md("**PPW - IPMH PM Plus Ratings by Domain and Time Point**"),
        subtitle = "Ratings collected at 6 Weeks, 14 Weeks, and 6 Months post-enrollment"
    ) %>%
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_row_groups()
    ) %>%
    tab_style(
        style = cell_text(weight = "bold", style = "italic"),
        locations = cells_body(rows = Item == "Overall")
    ) %>%
    tab_footnote(
        footnote = "Ratings based on a 5-point Likert scale: 1 = Completely Disagree to 5 = Completely Agree. 'Overall' rows represent domain-level averages across items.",
        locations = cells_title(groups = "title")
    )

pm_summary

# Subset Telepsychiatry acceptability
tele_acceptability <- accep_feas_dt %>% 
    select(clt_ptid, visit_type, af_tele_yesno, ends_with("_tele")) %>% 
    filter(af_tele_yesno == "Yes")%>%
    select(-af_tele_yesno) %>% 
    mutate(across(c(af_meets_tele, af_appeal_tele, af_enjoy_tele, af_welcome_tele, af_use_tele, 
                    af_seems_tele, af_doable_tele, af_easy_tele, af_life_tele, af_suitable_tele, 
                    af_applicable_tele, af_match_tele), 
                  ~ recode(., !!!accep_labels)))

tele_long_data <- tele_acceptability %>%
    pivot_longer(cols = -c(clt_ptid, visit_type), names_to = "item", values_to = "response")

tele_summary_table <- tele_long_data %>%
    group_by(visit_type, item) %>%
    summarise(
        mean = round(mean(response, na.rm = TRUE), 2),
        sd = round(sd(response, na.rm = TRUE), 2),
        .groups = "drop"
    ) %>%
    mutate(mean_sd = paste0(mean, " (", sd, ")"))

tele_summary_table <- tele_summary_table %>%
    mutate(
        Domain = case_when(
            item %in% c("af_meets_tele", "af_appeal_tele", "af_enjoy_tele", "af_welcome_tele") ~ "Acceptability",
            item %in% c("af_applicable_tele", "af_suitable_tele", "af_match_tele") ~ "Appropriateness",
            item %in% c("af_use_tele", "af_seems_tele", "af_doable_tele", "af_easy_tele", "af_life_tele") ~ "Feasibility"
        ),
        Item = case_when(
            item == "af_meets_tele" ~ "IPMH program telepsychiatry meets my approval",
            item == "af_appeal_tele" ~ "IPMH program telepsychiatry is appealing to me",
            item == "af_enjoy_tele" ~ "I would enjoy/like the IPMH program telepsychiatry",
            item == "af_welcome_tele" ~ "I welcome the IPMH program telepsychiatry",
            item == "af_applicable_tele" ~ "IPMH telepsychiatry seems applicable",
            item == "af_suitable_tele" ~ "IPMH telepsychiatry seems suitable",
            item == "af_match_tele" ~ "IPMH telepsychiatry seems like a good match",
            item == "af_use_tele"~ "IPMH program telepsychiatry seems like something I could use",
            item == "af_seems_tele" ~ "IPMH program telepsychiatry seems possible",
            item == "af_doable_tele" ~ "IPMH program telepsychiatry seems doable",
            item == "af_easy_tele" ~ "IPMH program telepsychiatry seems easy to use",
            item == "af_life_tele" ~ "IPMH program telepsychiatry is something I can easily fit into my life"
        )
    ) 

tele_summary_wide <- tele_summary_table %>%
    select(Domain, Item, visit_type, mean_sd) %>%
    pivot_wider(names_from = visit_type, values_from = mean_sd)%>% 
    select(Domain, Item, `6 Weeks`, `14 Weeks`)


# Compute Overall Mean(SD) per Domain----
tele_domain_overall <- tele_long_data %>%
    mutate(
        Domain = case_when(
            item %in% c("af_meets_tele", "af_appeal_tele", "af_enjoy_tele", "af_welcome_tele") ~ "Acceptability",
            item %in% c("af_applicable_tele", "af_suitable_tele", "af_match_tele") ~ "Appropriateness",
            item %in% c("af_use_tele", "af_seems_tele", "af_doable_tele", "af_easy_tele", "af_life_tele") ~ "Feasibility"
        )
    ) %>%
    group_by(Domain, visit_type) %>%
    summarise(
        mean = round(mean(response, na.rm = TRUE), 2),
        sd = round(sd(response, na.rm = TRUE), 2),
        .groups = "drop"
    ) %>%
    mutate(
        Item = "Overall",
        mean_sd = paste0(mean, " (", sd, ")")
    ) %>%
    select(Domain, Item, visit_type, mean_sd)

tele_domain_overall_wide <- tele_domain_overall %>%
    pivot_wider(names_from = visit_type, values_from = mean_sd) %>%
    select(Domain, Item, `6 Weeks`, `14 Weeks`)

tele_summary_wide_final <- bind_rows(tele_summary_wide, tele_domain_overall_wide)


tele_summary <- tele_summary_wide_final %>%
    gt(groupname_col = "Domain") %>%
    tab_header(
        title = md("**PPW - IPMH Telepsychiatry Ratings by Domain and Time Point**"),
        subtitle = "Ratings collected at 6 Weeks, 14 Weeks, and 6 Months post-enrollment"
    ) %>%
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_row_groups()
    ) %>%
    tab_style(
        style = cell_text(weight = "bold", style = "italic"),
        locations = cells_body(rows = Item == "Overall")
    ) %>%
    tab_footnote(
        footnote = "Ratings based on a 5-point Likert scale: 1 = Completely Disagree to 5 = Completely Agree. 'Overall' rows represent domain-level averages across items.",
        locations = cells_title(groups = "title")
    )

tele_summary

