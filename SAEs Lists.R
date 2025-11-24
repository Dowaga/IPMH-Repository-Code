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
    rename(Event = ae_cat)%>%
    mutate(Event = str_remove(Event, fixed("(SAE)")))

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
death_tbl <- valign(death_tbl, j = everything(), valign = "top")
death_tbl <- align(death_tbl, j = everything(), align = "left")
death_tbl <- height_all(death_tbl, height = NA)
death_tbl <- autofit(death_tbl)

# Set specific max widths (A4 width after margins is approx 6.5 inches)
death_tbl <- width(death_tbl, j = "record_id", width = 1)
death_tbl <- width(death_tbl, j = "Event", width = 1.5)
death_tbl <- width(death_tbl, j = "death_type", width = 1.3)
death_tbl <- width(death_tbl, j = "ae_narrative", width = 4.5)  # main text column

# Allow word wrapping
death_tbl <- wordwrap(death_tbl)

# Create Word document
doc <- read_docx()

doc <- body_add_par(doc, "Death Cases Narrative Summary", style = "heading 1")
doc <- body_add_par(doc, "For Physician Review and Classification", style = "heading 2")

# Add table
doc <- body_add_flextable(doc, death_tbl)

# Save
print(doc, target = "Death_Narratives_For_Review.docx")

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
            (death_type == "Infant Death") ~ "Other / Unknown",
            TRUE ~ NA_character_   # keep cause_of_death as NA if death_type is NA
        ),
        maternal_cause_category =  case_when(
                (death_type == "Maternal Death") & str_detect(ae_narrative, regex("hemorrhage|bleeding", ignore_case = TRUE)) ~ "Hemorrhage",
                (death_type == "Maternal Death") ~ "Other / Unknown",
                TRUE ~ NA_character_   # keep cause_of_death as NA if death_type is NA
        )
    )


# Step 2: Create a binary variable for each Event category

sae_wide <- clean_sae_df %>%
    mutate(event_flag = 1) %>%  # to allow counting
    select(record_id, death_type, Arm, Event, event_flag, infant_cause_category,
           maternal_cause_category) %>%
    tidyr::pivot_wider(names_from = Event, values_from = event_flag, values_fill = 0) %>% 
    select(-record_id)

sae_wide <- sae_wide %>%
    relocate(death_type, .after = "Death (Infant or Maternal) ")


# Step 3: Summarize using tbl_summary by arm
sae_summary <- sae_wide %>% 
    tbl_summary(by = Arm,
        include = c(
            "Death (Infant or Maternal) ",
            "death_type",
            "infant_cause_category",
            "maternal_cause_category",
            "Miscarriage or stillbirth (loss of pregnancy) ",
            "New/prolonged hospitalization " ),
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
    as_gt() %>%
    # modify with gt functions
    gt::tab_header("Summary of Serious Adverse Events by Study Arm") %>% 
    gt::tab_options(
        table.font.size = "medium",
        data_row.padding = gt::px(1)) %>%
    tab_options(
        table.font.size = px(14))


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
    ) |>
    mutate(Arm = case_when(dummy_arm=="Arm X"~  "Control",
                           dummy_arm=="Arm Y" ~ "Intervention",
                           TRUE ~ NA))

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
        include = c(ae_define___1_bin, ae_define___2_bin, ae_define___5_bin),
        label = list(
            ae_define___1_bin ~ "Kicked out of home", 
            ae_define___2_bin ~ "Experienced violence or abuse", 
            ae_define___5_bin ~ "Persistent or significant psychosocial distress"
        ),
        percent = "cell",
        digits = list(
            all_continuous() ~ 1,       # continuous variables ??? 1 d.p.
            all_categorical() ~ c(0, 1) # categorical ??? 0 decimals for n, 1 d.p. for %
        )
    ) %>% 
    bold_labels() %>%
    #add_p() %>% 
    add_overall() %>% 
    # convert from gtsummary object to gt object
    as_gt() %>%
    # modify with gt functions
    gt::tab_header("Summary of Adverse Events by Arm") %>% 
    gt::tab_source_note(gt::md("A single participant may experience multiple AEs")) %>% 
    gt::tab_options(
        table.font.size = "medium",
        data_row.padding = gt::px(1)) %>%
    tab_options(
        table.font.size = px(14))

# List of AEs
ae_list <- arm_ae %>%
    select(
        record_id, dem_age, Arm,
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
        record_id, dem_age, Arm,
        ae_type, ae_dateonset, ae_datereport,
        days_to_report, ae_relation, ae_narrative
    ) %>%
    arrange(Arm, record_id, ae_dateonset)

# Overall AEs summary
total_aes <- ae_long %>% 
    tbl_summary(
        include = c(ae_type),
                label = list(ae_type ~ "AE Type"),
        digits = list(
            all_continuous() ~ 1,       # continuous variables ??? 1 d.p.
            all_categorical() ~ c(0, 1) # categorical ??? 0 decimals for n, 1 d.p. for %
        )
    ) %>%
    add_n() %>%
    bold_labels() %>% 
    italicize_levels() %>% 
    # convert from gtsummary object to gt object
    as_gt() %>%
    # modify with gt functions
    gt::tab_header("Summary of Adverse Events") %>% 
    gt::tab_options(
        table.font.size = "medium",
        data_row.padding = gt::px(1)) %>%
    tab_options(
        table.font.size = px(14))%>%
    opt_table_lines()

total_aes

overal_aes <- ae_long %>% 
    tbl_summary(Arm,
        include = c(ae_type),
        label = list(ae_type ~ "AE Type"),
        digits = list(
            all_continuous() ~ 1,       # continuous variables ??? 1 d.p.
            all_categorical() ~ c(0, 1) # categorical ??? 0 decimals for n, 1 d.p. for %
        )
    ) %>%
    add_n() %>%
    bold_labels() %>% 
    italicize_levels() %>% 
    # convert from gtsummary object to gt object
    as_gt() %>%
    # modify with gt functions
    gt::tab_header("Summary of Adverse Events by Arm") %>% 
    gt::tab_options(
        table.font.size = "medium",
        data_row.padding = gt::px(1)) %>%
    tab_options(
        table.font.size = px(14))%>%
    opt_table_lines()

overal_aes

# Arm X AE list
armx_aes <- ae_long %>% 
    filter(Arm == "Control") %>% 
    select(-c(Arm, ae_narrative, ae_datereport)) %>% 
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
        title = "Summary of Adverse Events for Control Arm"
    )

# Arm Y AE list
army_aes <- ae_long %>% 
    filter(Arm == "Intervention") %>% 
    select(-c(Arm, ae_narrative, ae_datereport)) %>% 
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
        title = "Summary of Adverse Events for Intervention Arm"
    )


