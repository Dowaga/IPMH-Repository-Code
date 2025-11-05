# Header ------------------------------------------------------------------

# Author(s): Owaga
# Date: March 11, 2025
# AEs Lists

# Setup ------------------------------------------------------------------------
rm(list = ls())
# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")

ae_df <- ppw_sae_df%>% 
    select(record_id, redcap_repeat_instance, starts_with("ae_"), 
           redcap_event_name) %>% 
    filter(ae_yn == "Yes" & !str_detect(ae_cat, "SAE"))



sae_df <- ppw_sae_df %>% 
    select(record_id, redcap_repeat_instance, starts_with("ae_"), redcap_event_name) %>% 
    filter(ae_yn == "Yes" & str_detect(ae_cat, "SAE"))

# SAEs Report-------------------------------------------------------------------
clean_sae_df <- sae_df %>% 
    group_by(ae_cat) %>% 
    mutate(ae_cat = if_else(record_id == "21031003" & ae_cat == "New/prolonged hospitalization (SAE)",
                            "Death (Infant or Maternal) (SAE)",
                            ae_cat)) %>% 
    rename(Event = ae_cat) 

sae_summary <- clean_sae_df %>% 
    summarise(`Number of Events` = n(),
              `SAE Reported` = n(),
              `% Reported` = ifelse(`Number of Events` > 0, round((`Number of Events` / `SAE Reported`)) * 100, 1))

sae_total <- sae_summary %>%
    summarise(
        Event = "Total",
        `Number of Events` = sum(`Number of Events`),
        `SAE Reported` = sum(`SAE Reported`),
        `% Reported` = ifelse(`SAE Reported` > 0, round((`Number of Events` / `SAE Reported`) * 100, 1), 0)
    )

sae_report <- bind_rows(sae_summary, sae_total) %>%
    gt()%>%
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(
            rows = Event == "Total"
        )
    )


sae_report

# AE Report---------------------------------------------------------------------
ae_report <- ae_df %>% 
    filter(redcap_repeat_instance == 2|!str_detect(redcap_event_name, "Enrollment")) %>% 
    group_by(ae_cat) %>% 
    mutate(ae_cat = if_else(record_id == "21031003" & ae_cat == "New/prolonged hospitalization (SAE)",
                            "Death (Infant or Maternal) (SAE)",ae_cat)) %>% 
    rename(Event = ae_cat) %>% 
    summarise(`Number of Events` = n()) %>% 
    gt()

ae_report


#### SAEs by ARM
clean_sae_df <- clean_sae_df %>%
    mutate(Arm = case_when(
        grepl("Arm 2: Control", redcap_event_name) ~ "Control",
        grepl("Arm 1: Intervention", redcap_event_name) ~ "Intervention",
        TRUE ~ NA_character_
    ),
    dummy_arm = case_when(
        Arm == "Control" ~ "Arm X",
        Arm == "Intervention" ~ "Arm Y")
    )

# Step 2: Create a binary variable for each Event category

sae_wide <- clean_sae_df %>%
    mutate(event_flag = 1) %>%  # to allow counting
    select(record_id, dummy_arm, Event, event_flag) %>%
    tidyr::pivot_wider(names_from = Event, values_from = event_flag, values_fill = 0) %>% 
    select(-record_id)

# Step 3: Summarize using tbl_summary by arm
sae_summary <- tbl_summary(
    data = sae_wide,
    by = dummy_arm,
    type = all_continuous() ~ "continuous",
    statistic = all_continuous() ~ "{sum}",
    missing = "no"
) %>%
    modify_header(label = "**SAE Category**") %>%
    modify_caption("**Summary of Serious Adverse Events by Study Arm**")


sae_summary  

# Summary of Adverse events-----------------------------------------------------
# Select age and site from main df
ae_ages <- ppw_rct_df %>% 
    mutate(dem_age = if_else(
        dem_dob_uk == "Yes",
        floor(time_length(interval(dem_dob, clt_date), "years")),
        dem_age
    )) %>% 
    select(record_id, clt_study_site,redcap_event_name, dem_age, clt_date)

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


# Adverse Events summary by Arm
arm_ae <- ae_df %>% 
    mutate(
        dummy_arm = case_when(
            grepl("Control", redcap_event_name, ignore.case = TRUE) ~ "Arm X",
            grepl("Intervention", redcap_event_name, ignore.case = TRUE) ~ "Arm Y",
            TRUE ~ NA_character_
        )
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
    tbl_summary(by = dummy_arm,
                include = c(ae_define___1_bin, ae_define___2_bin, 
                            ae_define___5_bin),
                label = list(
                    ae_define___1_bin ~ "Kicked out of home", 
                    ae_define___2_bin ~ "Experienced violence or abuse", 
                    ae_define___5_bin ~ "Persistent or significant psychosocial distress"
                )) %>% 
    bold_labels() %>%
    #add_p() %>% 
    add_overall() %>% 
    # convert from gtsummary object to gt object
    as_gt() %>%
    # modify with gt functions
    gt::tab_header("Summary of Adverse Events") %>% 
    gt::tab_source_note(gt::md("A single participant may experience multiple AEs")) %>% 
    gt::tab_options(
        table.font.size = "medium",
        data_row.padding = gt::px(1)) %>%
    tab_options(
        table.font.size = px(14))

# List of AEs
ae_list <- ae_df %>%
    select(
        record_id, dem_age, dummy_arm,
        ae_cat,ae_define___1, ae_define___2,
        ae_define___3, ae_define___4, ae_define___5,
        ae_define___6, ae_relation, ae_narrative,
        ae_dateonset,ae_datereport
    )

# Define AE labels
ae_labels <- tibble(
    ae_code = paste0("ae_define___", 1:6),
    ae_type = c(
        "Kicked out of home",
        "Experienced violence or abuse",
        "Lost job or income",
        "Relationship breakdown",
        "Psychosocial distress",
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
        ae_dateonset = as_date(ae_dateonset),
        ae_datereport = as_date(ae_datereport),
        days_to_report = as.integer(ae_datereport - ae_dateonset)
    ) %>%
    select(
        record_id, dem_age, dummy_arm,
        ae_type, ae_dateonset, ae_datereport,
        days_to_report, ae_relation, ae_narrative
    ) %>%
    arrange(dummy_arm, record_id, ae_dateonset)

# Arm X AE list
armx_aes <- ae_long %>% 
    filter(dummy_arm == "Arm X") %>% 
    select(-c(dummy_arm, ae_narrative, ae_datereport)) %>% 
    rename(
        Record_ID             = record_id,
        `AE Type`         = ae_type,
        `Date Onset`          = ae_dateonset,
        `Days since Enrollment` = days_to_report,
        Age                   = dem_age,
        `Relatedness to study` = ae_relation
    )


# Create gt table with heading
armx_aes_gt <- armx_aes %>%
    gt() %>%
    tab_header(
        title = "Summary of Adverse Events for Arm X"
    )

# Arm Y AE list
army_aes <- ae_long %>% 
    filter(dummy_arm == "Arm Y") %>% 
    select(-c(dummy_arm, ae_narrative, ae_datereport)) %>% 
    rename(
        Record_ID             = record_id,
        `AE Type`         = ae_type,
        `Date Onset`          = ae_dateonset,
        `Days since Enrollment` = days_to_report,
        Age                   = dem_age,
        `Relatedness to study` = ae_relation
    )


# Create gt table with heading
army_aes_gt <- army_aes %>%
    gt() %>%
    tab_header(
        title = "Summary of Adverse Events for Arm Y"
    )



