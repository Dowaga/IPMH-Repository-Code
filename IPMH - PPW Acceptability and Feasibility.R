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
accep_dt <- ppw_rct_df %>%
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
totals <- accep_dt %>% 
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
accep_dt <- accep_dt %>%
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

item_summary<- summary_wide %>%
    gt(groupname_col = "Domain") %>%
    tab_header(title = "Mean (SD) of Youth Friendship Bench SA Ratings by Time Point") 


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

