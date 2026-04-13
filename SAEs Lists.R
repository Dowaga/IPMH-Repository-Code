# Header ------------------------------------------------------------------

# Author(s): Owaga
# Date: March 11, 2025
# AEs Lists

# Setup ------------------------------------------------------------------------
# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")

ae_df <- ppw_sae_df %>% 
    select(record_id, redcap_repeat_instance, starts_with("ae_"), 
           redcap_event_name) %>% 
    filter(ae_yn == "Yes" & !str_detect(ae_cat, "SAE"))

# Filter those who endorsed self harm to check AEs

self_harm <- ppw_rct_df %>%
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
    filter(phq_dead %in% c("several days","more than half the days",
                           "nearly every day")) %>% 
    select(clt_ptid, clt_study_site, visit_type, phq_dead,risk_date,
           risk_shphq9, risk_thoughts, referral_type___2, ae_yn) %>% 
    filter(!visit_type %in% c("Enrollment"))
    

sae_df <- ppw_sae_df %>% 
    select(record_id, redcap_repeat_instance, starts_with("ae_"), redcap_event_name) %>% 
    filter(ae_yn == "Yes" & str_detect(ae_cat, "SAE"))

# OPEN report -----------
## SAEs Report-------------------------------------------------------------------
clean_sae_df <- sae_df %>%
    mutate(ae_cat = case_when(
        record_id == "21031003" & ae_cat == "New/prolonged hospitalization (SAE)" ~ "Infant Death (SAE)",
        ae_cat == "Miscarriage or stillbirth (loss of pregnancy) (SAE)" ~ "Stillbirth (SAE)",
        ae_type___1 == "Checked" ~ "Maternal Death (SAE)",
        ae_type___2 == "Checked" ~ "Infant Death (SAE)",
        TRUE ~ ae_cat
    )) %>%
    group_by(ae_cat) %>%
    rename(Event = ae_cat) %>%
    mutate(Event = str_remove(Event, fixed("(SAE)")))

sae_summary <- clean_sae_df %>%
    summarise(
        `Number of Events` = n(),
        `SAE Reported` = n(),
        `% Reported` = ifelse(`Number of Events` > 0, round((`Number of Events` / `SAE Reported`) * 100, 1), 0)
    )

sae_total <- sae_summary %>%
    summarise(
        Event = "Total",
        `Number of Events` = sum(`Number of Events`),
        `SAE Reported` = sum(`SAE Reported`),
        `% Reported` = ifelse(`SAE Reported` > 0, round((`Number of Events` / `SAE Reported`) * 100, 1), 0)
    )

sae_report <- bind_rows(sae_summary, sae_total) %>%
    gt() %>%
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(rows = Event == "Total")
    )

sae_report

## AE Report---------------------------------------------------------------------
ae_define_map <- c(
    ae_define___1  = "Kicked out of home",
    ae_define___2  = "Experienced violence or abuse",
    ae_define___3  = "Breach of Confidentiality",
    ae_define___4  = "Self-harm/Suicidal behavior",
    ae_define___5  = "Persistent or significant psychosocial distress",
    ae_define___6  = "Infant harm/behavior",
    ae_define___99 = "Other"
)

ae_social_summary <- ae_df %>%
    filter(ae_cat == "Social harm event (includes: depression, anxiety, IPV, self-harm, infant harm, breach of confidentiality, etc.)") %>%
    filter(redcap_repeat_instance == 2 | !str_detect(redcap_event_name, "Enrollment")) %>%
    select(record_id, names(ae_define_map)) %>%
    pivot_longer(
        cols = names(ae_define_map),
        names_to = "variable",
        values_to = "checked"
    ) %>%
    filter(checked == "Checked") %>%
    mutate(Event = ae_define_map[variable]) %>%
    group_by(Event) %>%
    summarise(`Number of Events` = n())

ae_injury_summary <- ae_df %>%
    filter(ae_cat == "Injury") %>%
    filter(redcap_repeat_instance == 2 | !str_detect(redcap_event_name, "Enrollment")) %>%
    group_by(Event = ae_cat) %>%
    summarise(`Number of Events` = n())

ae_report_df <- bind_rows(ae_injury_summary, ae_social_summary)

total_events <- sum(ae_report_df$`Number of Events`)

ae_report <- ae_report_df %>%
    mutate(Summary = paste0(`Number of Events`, " (", round(`Number of Events` / total_events * 100, 1), "%)")) %>%
    select(Characteristic = Event, Summary) %>%
    bind_rows(
        tibble(Characteristic = "Total", Summary = as.character(total_events))
    ) %>%
    gt() %>%
    tab_header(title = "Table: Summary of Adverse Events") %>%
    cols_label(
        Characteristic = "Characteristic",
        Summary = paste0("N = ", total_events)
    ) %>%
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(rows = Characteristic == "Total")
    ) %>%
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels()
    ) %>%
    tab_footnote(footnote = "n (%)")

ae_report

# # CLOSED report -------------
# #### SAEs by ARM 
clean_sae_df <- clean_sae_df %>%
    mutate(Arm = case_when(
        grepl("Arm 2: Control", redcap_event_name) ~ "Control",
        grepl("Arm 1: Intervention", redcap_event_name) ~ "Intervention",
        TRUE ~ NA_character_
    ),
    dummy_arm = case_when(
        Arm == "Control" ~ "Arm X",
        Arm == "Intervention" ~ "Arm Y")
    )%>%
    mutate(
        death_type = case_when(
            ae_type___1 == "Checked" ~ "Maternal Death",
            ae_type___2 == "Checked" ~ "Infant Death",
            TRUE ~ NA_character_
        )
    )

deaths <- clean_sae_df %>%
    filter(ae_type___1 == "Checked"|ae_type___2 == "Checked") %>%
    select(record_id, Event, death_type, ae_narrative)

# Convert to flextable
death_tbl <- flextable(deaths)

# --- IMPORTANT FIXES FOR LONG TEXT ----
# Wrap text
# Apply to all columns by names

death_tbl <- valign(death_tbl, j = seq_along(death_tbl$col_keys), valign = "top")
death_tbl <- align(death_tbl, j = seq_along(death_tbl$col_keys), align = "left")
death_tbl <- height_all(death_tbl, height = 1)
death_tbl <- autofit(death_tbl)

# Set specific max widths (A4 width after margins is approx 6.5 inches)
death_tbl <- width(death_tbl, j = "record_id", width = 1)
death_tbl <- width(death_tbl, j = "Event", width = 1.5)
death_tbl <- width(death_tbl, j = "death_type", width = 1.3)
death_tbl <- width(death_tbl, j = "ae_narrative", width = 4.5)  # main text column


# Create Word document
# doc <- read_docx()
# 
# doc <- body_add_par(doc, "Death Cases Narrative Summary", style = "heading 1")
# doc <- body_add_par(doc, "For Physician Review and Classification", style = "heading 2")
# 
# # Add table
# doc <- body_add_flextable(doc, death_tbl)
# 
# # Save
# print(doc, target = "Death_Narratives_For_Review.docx")

clean_sae_df <- clean_sae_df %>%
    mutate(
        infant_cause_category =  case_when(
            (death_type == "Infant Death") & str_detect(ae_narrative, regex("milk.*nose|milk.*mouth|foam.*mouth|foam.*nose|frothing|choking|aspiration", ignore_case = TRUE)) ~ "Respiratory failure / Aspiration",
            (death_type == "Infant Death") & str_detect(ae_narrative, regex("vomiting blood|gastrointestinal bleeding|peptic ulcer", ignore_case = TRUE)) ~ "Gastrointestinal bleeding / Peptic ulcer complications",
            (death_type == "Infant Death") & str_detect(ae_narrative, regex("breathing|respiratory|asphyxia", ignore_case = TRUE)) ~ "Respiratory complications",
            (death_type == "Infant Death") & str_detect(ae_narrative, regex("infection|sepsis", ignore_case = TRUE)) ~ "Infection / Sepsis",
            (death_type == "Infant Death") & str_detect(ae_narrative, regex("hemorrhage|bleeding", ignore_case = TRUE)) ~ "Hemorrhage",
            (death_type == "Infant Death") & str_detect(ae_narrative, regex("fever|vomiting", ignore_case = TRUE)) ~ "Fever/Vomiting",
            (death_type == "Infant Death") & str_detect(ae_narrative, regex("congenital|malformation|birth defect|imperforate anus|hand anomaly", ignore_case = TRUE)) ~ "Congenital malformations / Birth defects",
            (death_type == "Infant Death") & str_detect(ae_narrative, regex("without signs of active movements| nor did she cry at birth|no movements|did not cry|apnea at birth|stillborn", ignore_case = TRUE)) ~ "Birth asphyxia / Intrapartum hypoxia",
            (death_type == "Infant Death") & str_detect(ae_narrative, regex("due to prolonged labour|well specialised care and admitted to the NBU", ignore_case = TRUE)) ~ "Birth asphyxia secondary to prolonged labor",
            (death_type == "Infant Death") & str_detect(ae_narrative, regex("low birth weight|had a pre term birth", ignore_case = TRUE)) ~ "Prematurity",
            (death_type == "Infant Death") & str_detect(ae_narrative, regex("gestation period of 39 weeks at 4.00am", ignore_case = TRUE)) ~ "Still birth",
            (death_type == "Infant Death") ~ "Other / Unknown",
            TRUE ~ NA_character_   # keep cause_of_death as NA if death_type is NA
        ),
        maternal_cause_category = case_when(
            death_type == "Maternal Death" &
                str_detect(ae_narrative, regex("peptic ulcer|peptic ulcers|peptic ulcer disease|gastrointestinal bleeding|GI bleed|hematemesis", ignore_case = TRUE)) ~ "Gastrointestinal bleeding",

            death_type == "Maternal Death" &
                str_detect(ae_narrative, regex("abdominal pain.*(emergency|urgent)|intraoperative|cesarean", ignore_case = TRUE)) ~
                "Intraoperative complication during CS",
            TRUE ~ NA_character_
        )
    )

uknown_deaths <- clean_sae_df %>%
    filter(infant_cause_category == "Other / Unknown")

# Step 2: Create a binary variable for each Event category
sae_wide <- clean_sae_df %>%
    mutate(event_flag = 1) %>%  # to allow counting
    select(record_id, death_type, Arm, Event, event_flag, infant_cause_category,
           maternal_cause_category) %>%
    tidyr::pivot_wider(names_from = Event, values_from = event_flag, values_fill = 0) %>%
    select(-record_id)

sae_wide <- sae_wide %>%
    relocate(death_type, .after = "Infant Death ")

# Step 3: Summarize using tbl_summary by arm
sae_summary <- sae_wide %>%
    tbl_summary(by = Arm,
                include = c(
                    "Maternal Death ",
                    "Infant Death ",
                    "death_type",
                    "infant_cause_category",
                    "maternal_cause_category",
                    "Stillbirth ",
                    "New/prolonged hospitalization "
                ),
    type = all_continuous() ~ "continuous",
    statistic = all_continuous() ~ "{sum}",
    percent = "cell",
    missing = "no",
    digits = list(
        all_continuous() ~ 1,       # continuous variables ??? 1 d.p.
        all_categorical() ~ c(0, 1) # categorical ??? 0 decimals for n, 1 d.p. for %
    ),
    label = list(
        death_type = "Death Classification",
        infant_cause_category = "Infants Death Cause",
        maternal_cause_category = "Maternal Death Cause"),
    sort = list(all_categorical() ~ "frequency")# Sort categorical levels by frequency in descending order
) %>%
    bold_labels() %>%
    #add_p() %>%
    add_overall() %>%
    add_n() %>%
    # convert from gtsummary object to gt object
    gtsummary::as_gt() %>%
    # modify with gt functions
    gt::tab_header("Summary of Serious Adverse Events by Study Arm") %>%
    gt::tab_options(
        table.font.size = "medium",
        data_row.padding = gt::px(1)) %>%
    tab_options(
        table.font.size = px(14))

sae_summary


# Step 4: Summarize overall SAEs using tbl_summary----
sae_overall <- sae_wide %>%
    tbl_summary(
        include = c(
            "Maternal Death ",
            "Infant Death ",
            "death_type",
            "infant_cause_category",
            "maternal_cause_category",
            "Stillbirth ",
            "New/prolonged hospitalization "
        ),
        type = all_continuous() ~ "continuous",
        statistic = all_continuous() ~ "{sum}",
        percent = "cell",
        missing = "no",
        digits = list(
            all_continuous() ~ 1,       # continuous variables ??? 1 d.p.
            all_categorical() ~ c(0, 1) # categorical ??? 0 decimals for n, 1 d.p. for %
        ),
        label = list(
            death_type = "Death Classification",
            infant_cause_category = "Infants Death Cause",
            maternal_cause_category = "Maternal Death Cause"),
        sort = list(all_categorical() ~ "frequency")# Sort categorical levels by frequency in descending order
    ) %>%
    bold_labels() %>%
    add_n() %>%
    # convert from gtsummary object to gt object
    gtsummary::as_gt() %>%
    # modify with gt functions
    gt::tab_header("Summary of Reported Serious Adverse Events") %>%
    gt::tab_options(
        table.font.size = "medium",
        data_row.padding = gt::px(1)) %>%
    tab_options(
        table.font.size = px(14))


sae_overall

# Summary of Adverse events-----------------------------------------------------
# Select age and site from main df
ae_ages <- ppw_rct_df %>%
    mutate(dem_age = if_else(
        dem_dob_uk == "Yes",
        floor(time_length(interval(dem_dob, clt_date), "years")),
        dem_age
    )) %>%
    select(record_id, clt_study_site,redcap_event_name, dem_age, clt_date)

# Extract baseline age at enrollment for each participant
baseline_age <- ae_ages %>%
    filter(grepl("^Enrollment", redcap_event_name)) %>%
    select(record_id, baseline_age = dem_age)

# left joing with aes df
ae_df <- ae_df %>%
    mutate(
        dummy_arm = case_when(
            grepl("Control", redcap_event_name, ignore.case = TRUE) ~ "Arm X",
            grepl("Intervention", redcap_event_name, ignore.case = TRUE) ~ "Arm Y",
            TRUE ~ NA_character_
        ))%>%
    select(-redcap_event_name) %>%
    left_join(ae_ages, by = "record_id") %>%
    filter(!is.na(dem_age)) %>%
    mutate(ae_datereport = as.Date(ae_datereport),
           clt_date = as.Date(clt_date),
           reported_days = as.numeric(ae_datereport - clt_date))

ae_summary <- ae_df %>%
    select(record_id, clt_study_site, dem_age,ae_datereport,ae_cat,ae_dateonset, reported_days) %>%
    # Remove the facility code
    mutate(clt_study_site = gsub("^[0-9]+,\\s*", "", clt_study_site)) %>%
    rename(PTID = record_id,
           `Study Site` = "clt_study_site",
           `Date Averse Event was reported` = ae_datereport,
           `Adverse Event reported` = ae_cat,
           `Start date of adverse event` = ae_dateonset,
           `Age of participant` = dem_age,
           `Reported since enrollment` = reported_days)

ae_summary <- ae_summary %>%
    gt()

ae_summary

# Adverse Events summary by Arm----
arm_ae <- ae_df %>%
    mutate(
        dummy_arm = case_when(
            grepl("Control", redcap_event_name, ignore.case = TRUE) ~ "Arm X",
            grepl("Intervention", redcap_event_name, ignore.case = TRUE) ~ "Arm Y",
            TRUE ~ NA_character_
        )
    ) |>
    mutate(Arm = case_when(dummy_arm=="Arm X"~  "Control",
                           dummy_arm=="Arm Y" ~ "Intervention",
                           TRUE ~ NA))

# PHQ-9 suicidality - formatted to match ae_long structure
suicide_phq_long <- ppw_rct_df %>%
    filter(phq_dead %in% c("several days", "more than half the days", "nearly every day")) %>%
    mutate(
        dem_age = if_else(
            dem_dob_uk == "Yes",
            floor(time_length(interval(dem_dob, clt_date), "years")),
            dem_age
        ),
        Arm = case_when(
            grepl("Control", redcap_event_name, ignore.case = TRUE) ~ "Control",
            grepl("Intervention", redcap_event_name, ignore.case = TRUE) ~ "Intervention",
            TRUE ~ NA_character_
        ),
        ae_type = "Suicidal Ideation (PHQ-9)",
        clt_date = as.Date(clt_date),
        ae_datereport = as.Date(ae_dateonset),
        ae_dateonset = as.Date(ae_dateonset),
        days_to_report = as.integer(ae_datereport - ae_dateonset),
        days_since_enrollment = as.numeric(ae_dateonset - clt_date),
        ae_relation = NA_character_,
        ae_narrative = risk_thoughts,
        clt_study_site = gsub("^[0-9]+,\\s*", "", clt_study_site)
    ) %>%
    select(record_id = clt_ptid, dem_age, Arm, ae_type, ae_dateonset,
           ae_datereport, days_to_report, ae_relation, ae_narrative)


suicide_phq_long <- suicide_phq_long %>%
    right_join(baseline_age, by = "record_id") %>%
    mutate(dem_age = coalesce(dem_age, baseline_age),
           ae_dateonset  = as_date(ae_dateonset),
           ae_datereport = as_date(ae_datereport)
           )

ae_bin <- arm_ae %>%
    mutate(
        across(
            .cols = starts_with("ae_define"),     # <-- choose the columns to convert
            .fns  = ~ if_else(.x == "Checked", 1, 0),
            .names = "{.col}_bin"           # optional: saves new columns instead of overwriting
        ))

# AEs summary
ae_tbl <- ae_bin %>%
    tbl_summary(
        by = Arm,
        include = c(ae_define___1_bin, ae_define___2_bin, ae_define___3_bin,
                    ae_define___4_bin, ae_define___5_bin, ae_define___6_bin,
                    ae_define___99_bin),
        label = list(
            ae_define___1_bin  ~ "Kicked out of home",
            ae_define___2_bin  ~ "Experienced violence or abuse",
            ae_define___3_bin  ~ "Breach of Confidentiality",
            ae_define___4_bin  ~ "Self-harm/Suicidal behavior (AE Form)",
            ae_define___5_bin  ~ "Persistent or significant psychosocial distress",
            ae_define___6_bin  ~ "Infant harm/behavior",
            ae_define___99_bin ~ "Other"
        ),
        percent = "cell",
        missing = "no",
        digits = list(
            all_continuous() ~ 1,
            all_categorical() ~ c(0, 1)
        )
    ) %>%
    bold_labels() %>%
    add_overall() %>%
    gtsummary::as_gt() %>%
    gt::tab_header("Summary of Adverse Events by Arm") %>%
    gt::tab_source_note(gt::md("A single participant may experience multiple AEs")) %>%
    gt::tab_options(table.font.size = "medium", data_row.padding = gt::px(1)) %>%
    tab_options(table.font.size = px(14))

# List of AEs
ae_list <- arm_ae %>%
    select(
        record_id, dem_age, Arm,
        ae_cat, ae_define___1, ae_define___2, ae_define___3,
        ae_define___4, ae_define___5, ae_define___6, ae_define___99,
        ae_relation, ae_narrative, ae_dateonset, ae_datereport, reported_days
    )

# Define AE labels
ae_labels <- tibble(
    ae_code = c(paste0("ae_define___", 1:6), "ae_define___99"),
    ae_type = c(
        "Kicked out of home",
        "Experienced violence or abuse",
        "Breach of Confidentiality",
        "Self-harm/Suicidal behavior (AE Form)",
        "Persistent or significant psychosocial distress",
        "Infant harm/behavior",
        "Other"
    )
)

# Pivot AE columns longer and filter for selected AEs
ae_long <- ae_list %>%
    pivot_longer(
        cols = starts_with("ae_define___"),
        names_to = "ae_code",
        values_to = "ae_flag"
    ) %>%
    filter(ae_flag == "Checked") %>%
    left_join(ae_labels, by = "ae_code") %>%
    mutate(
        ae_dateonset  = as_date(ae_dateonset),
        ae_datereport = as_date(ae_datereport),
        days_to_report = as.integer(ae_datereport - ae_dateonset)
    ) %>%
    select(record_id, dem_age, Arm, ae_type, ae_dateonset,
           ae_datereport, reported_days, ae_relation, ae_narrative) %>%
    arrange(Arm, record_id, ae_dateonset) %>%
    # Add PHQ-9 suicidality rows
    bind_rows(suicide_phq_long) %>%
    filter(!is.na(ae_type))

# Check iff same participants has Suicidal and reported on AEs
subset_data <- ae_long[grepl("Self-harm|Suicidal", ae_long$ae_type, ignore.case = TRUE), ]

# Step 2: View unique participants
participants_with_events <- unique(subset_data$record_id)


# Overall AEs summary
total_aes <- ae_long %>%
    tbl_summary(
        sort = list(all_categorical() ~ "frequency"),
        include = c(ae_type),
        label = list(ae_type ~ "Adverse Event Type"),
        missing = "no",
        digits = list(
            all_continuous() ~ 1,
            all_categorical() ~ c(0, 1)
        )
    ) %>%
    add_n() %>%
    bold_labels() %>%
    italicize_levels() %>%
    gtsummary::as_gt() %>%
    gt::tab_header("Summary of Adverse Events") %>%
    gt::tab_options(table.font.size = "medium", data_row.padding = gt::px(1)) %>%
    tab_options(table.font.size = px(14)) %>%
    opt_table_lines()%>%
        gt::tab_source_note(
        source_note = "Note: Pre-existing conditions are excluded at enrollment; Self-harm is only reported in follow-up, so counts are lower than suicidal ideation."
    )


total_aes

overal_aes <- ae_long %>%
    tbl_summary(
        sort = list(all_categorical() ~ "frequency"),
        by = Arm,
        include = c(ae_type),
        label = list(ae_type ~ "AE Type"),
        missing = "no",
        digits = list(
            all_continuous() ~ 1,
            all_categorical() ~ c(0, 1)
        )
    ) %>%
    add_n() %>%
    bold_labels() %>%
    italicize_levels() %>%
    gtsummary::as_gt() %>%
    gt::tab_header("Summary of Adverse Events by Arm") %>%
    gt::tab_options(table.font.size = "medium", data_row.padding = gt::px(1)) %>%
    tab_options(table.font.size = px(14)) %>%
    opt_table_lines()

overal_aes

# Arm X (Control) AE list
armx_aes <- ae_long %>%
    filter(Arm == "Control") %>%
    select(-c(Arm, ae_narrative, ae_datereport, baseline_age)) %>%
    rename(
        Record_ID              = record_id,
        `AE Type`              = ae_type,
        `Date Onset`           = ae_dateonset,
        `Days since Enrollment` = days_to_report,
        Age                    = dem_age,
        `Relatedness to study` = ae_relation
    )

armx_aes_gt <- armx_aes %>%
    gt() %>%
    tab_header(title = "Summary of Adverse Events for Control Arm")

# Arm Y (Intervention) AE list
army_aes <- ae_long %>%
    filter(Arm == "Intervention") %>%
    select(-c(Arm, ae_narrative, ae_datereport, baseline_age)) %>%
    rename(
        Record_ID              = record_id,
        `AE Type`              = ae_type,
        `Date Onset`           = ae_dateonset,
        `Days since Enrollment` = days_to_report,
        Age                    = dem_age,
        `Relatedness to study` = ae_relation
    )

army_aes_gt <- army_aes %>%
    gt() %>%
    tab_header(title = "Summary of Adverse Events for Intervention Arm")

# ## Leaving for now: need to fix several things
# #1) do we only include suicide ideation during follow-up visits or all visits?
# #2) currently, for some with suicidality, their age is NA, and their date onset/days since enrollment need to be double checked.
# #3) for those, they don't have a direct relatedness to the study. need to see how to address this.
