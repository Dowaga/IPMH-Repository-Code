#______________________________________________________________

# Loading Participants Scheduling and Follow-up

# Load Packages---------------------------------------

#rm(list = ls())
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")

#=========================================================
# 1. Import Google Sheets
#=========================================================

# Authenticate if the Google Sheet is restricted
#gs4_auth()

#Otherwise use
gs4_deauth()

# Full Google Sheet URL
sheet_id <- "https://docs.google.com/spreadsheets/d/1qOQTI62qFtLMGHWrsiI0OsbWSDbiQBXK-EJ_RBrf6iw/edit"

# Get all sheet names
sheet_names <- sheet_properties(sheet_id)$name

# Read all sheets using the second row as column headers
sheet_list <- map(
    set_names(sheet_names, sheet_names),
    ~ read_sheet(
        sheet_id,
        sheet = .x,
        skip = 1
    )
)

#=========================================================
# 2. Rename columns 4:15
#=========================================================

new_names <- c(
    "reminder1_6wks",
    "reminder2_6wks",
    "due_date_6wks",
    "actual_visit_6wks",
    "reminder1_14wks",
    "reminder2_14wks",
    "due_date_14wks",
    "actual_visit_14wks",
    "reminder1_6mths",
    "reminder2_6mths",
    "due_date_6mths",
    "actual_visit_6mths"
)

sheet_list_clean <- map(sheet_list, function(df) {
    
    if (ncol(df) >= 15) {
        colnames(df)[4:15] <- new_names
    } else {
        message("Skipping column renaming: insufficient columns.")
    }
    
    df
})

# Export individual facility datasets
list2env(sheet_list_clean, envir = .GlobalEnv)

#=========================================================
# 3. Extract delivery datasets per facility
#=========================================================

required_cols <- c(
    "Participant ID",
    "Delivery Date",
    "actual_visit_6wks",
    "actual_visit_14wks",
    "actual_visit_6mths"
)

walk2(
    sheet_list_clean,
    names(sheet_list_clean),
    function(df, name) {
        
        if (all(required_cols %in% names(df))) {
            
            df_clean <- df %>%
                select(all_of(required_cols)) %>%
                filter(!is.na(`Participant ID`))
            
            assign(
                paste0(
                    str_replace_all(
                        tolower(name),
                        "[^a-z0-9]+",
                        "_"
                    ),
                    "_deliveries"
                ),
                df_clean,
                envir = .GlobalEnv
            )
            
        } else {
            
            message(
                "Skipping '", name,
                "' - missing required columns."
            )
        }
    }
)

#=========================================================
# 4. Collect delivery datasets
#=========================================================

delivery_dfs <- ls(pattern = "_deliveries$") %>%
    setdiff(c("all_deliveries", "sheet1_deliveries")) %>%
    mget(envir = .GlobalEnv)

#=========================================================
# 5. Combine and clean delivery datasets
#=========================================================

all_deliveries <- imap_dfr(
    delivery_dfs,
    ~ .x %>%
        mutate(
            # Convert follow-up dates
            actual_visit_6wks = ymd(actual_visit_6wks),
            actual_visit_14wks = ymd(actual_visit_14wks),
            actual_visit_6mths = ymd(actual_visit_6mths),
            
            # Convert delivery date
            delivery_date = ymd(`Delivery Date`)
        ) %>%
        filter(!is.na(delivery_date))
) %>%
    select(
        ptid = `Participant ID`,
        delivery_date,
        actual_visit_6wks,
        actual_visit_14wks,
        actual_visit_6mths
    ) %>%
    mutate(
        FacilityCode = substr(ptid, 3, 4),
        
        Facility = recode(
            FacilityCode,
            
            "01" = "Rwambwa Sub-county Hospital",
            "02" = "Sigomere Sub County Hospital",
            "03" = "Uyawi Sub County Hospital",
            "04" = "Got Agulu Sub-District Hospital",
            "05" = "Ukwala Sub County Hospital",
            "06" = "Madiany Sub County Hospital",
            "07" = "Kabondo Sub County Hospital",
            "08" = "Mbita Sub-County Hospital",
            "09" = "Miriu Health Centre",
            "11" = "Nyandiwa Level IV Hospital",
            "13" = "Ober Kamoth Sub County Hospital",
            "14" = "Gita Sub County Hospital",
            "15" = "Akala Health Centre",
            "16" = "Usigu Health Centre",
            "17" = "Ramula Health Centre",
            "18" = "Simenya Health Centre",
            "19" = "Airport Health Centre (Kisumu)",
            "20" = "Nyalenda Health Centre",
            "21" = "Mirogi Health Centre",
            "22" = "Ndiru Level 4 Hospital",
            
            .default = FacilityCode
        )
    ) %>%
    select(
        ptid,
        delivery_date,
        actual_visit_6wks,
        actual_visit_14wks,
        actual_visit_6mths,
        Facility
    )
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
tracker_delivery <- all_deliveries %>% 
    select(ptid, tracker_delivery_date = delivery_date)

# Check iff all PTIDS in the tracker are capture as in the PPW RCT
#IDs in the survey database
survey_id <- ppw_rct_df %>% 
    select(clt_ptid) %>% 
    distinct()

#compare -------------------------------------------------------------------
### Those that are in the Tracker, but not in the RCT database
setdiff(tracker_delivery$ptid, survey_id$clt_ptid)

### Those that are in the RCT, and haven't been updated in the tracker
setdiff(survey_id$clt_ptid, tracker_delivery$ptid) 

# ------------------------------------------------------------
# 6. Extract delivery date from the 6-week CRF
# ------------------------------------------------------------

six_week_delivery <- ppw_rct_df %>%
    filter(
        grepl("^6 Weeks", redcap_event_name),
        is.na(redcap_repeat_instance),
        clt_visit == "6 weeks post-partum"
    ) %>%
    select(
        record_id,
        delivery_date_crf = tpnc_date,
        clt_date,
        mv_visit
    ) %>%
    mutate(
        record_id = as.integer(record_id),
        delivery_date_crf = as.Date(delivery_date_crf),
        clt_date = as.Date(clt_date)
    ) %>%
    distinct(record_id, .keep_all = TRUE)

# Verify six-week follow-up participants not in tracker

setdiff(six_week_delivery$record_id, all_deliveries$ptid)


# ------------------------------------------------------------
# 7. Compare tracker and 6-week CRF delivery dates
# ------------------------------------------------------------

delivery_dates <- all_deliveries %>%
    select(
        ptid,tracker_delivery_date = delivery_date, Facility
    ) %>%
    full_join(
        six_week_delivery,
        by = c("ptid" = "record_id")
    ) %>%
    mutate(
        
        # --------------------------------------------------------
        # Delivery-date discrepancy
        # --------------------------------------------------------
        delivery_date_discrepancy =
            !is.na(tracker_delivery_date) &
            !is.na(delivery_date_crf) &
            tracker_delivery_date != delivery_date_crf,        
        # --------------------------------------------------------
        # Final delivery date
        #
        # PRIMARY SOURCE:
        # 6-week CRF delivery date when available.
        #
        # If the Tracker and 6-week CRF dates disagree,
        # use the 6-week CRF delivery date.
        #
        # If the 6-week CRF date is missing, use the
        # Tracker delivery date.
        # --------------------------------------------------------
        retention_delivery_date = case_when(
            
            !is.na(tracker_delivery_date) &
                !is.na(delivery_date_crf) &
                tracker_delivery_date != delivery_date_crf ~
                delivery_date_crf,
            
            !is.na(delivery_date_crf) ~
                delivery_date_crf,
            
            !is.na(tracker_delivery_date) ~
                tracker_delivery_date,
            
            TRUE ~ as.Date(NA)
        ),
        
        # --------------------------------------------------------
        # Source of final delivery date
        # --------------------------------------------------------
        delivery_date_source = case_when(
            
            !is.na(tracker_delivery_date) &
                !is.na(delivery_date_crf) &
                tracker_delivery_date != delivery_date_crf ~
                "6-week CRF - discrepancy with Tracker",
            
            !is.na(delivery_date_crf) ~
                "6-week CRF",
            
            !is.na(tracker_delivery_date) ~
                "Tracker",
            
            TRUE ~
                "Missing"
        )) 


# ------------------------------------------------------------
# 8. Delivery-date QC dataset
# ------------------------------------------------------------

delivery_date_qc <- delivery_dates %>%
    filter(delivery_date_discrepancy) %>%
    select(
        ptid,
        tracker_delivery_date,
        delivery_date_crf,
        retention_delivery_date,
        delivery_date_source
    ) %>%
    arrange(ptid)


# Participants without a final delivery date
missing_delivery_dates <- delivery_dates %>%
    filter(is.na(retention_delivery_date)) %>%
    select(
        ptid,
        tracker_delivery_date,
        delivery_date_crf,
        delivery_date_source
    )


# ------------------------------------------------------------
# 9. Calculate protocol follow-up windows
# ------------------------------------------------------------
#
# CURRENT DEFINITIONS:
#
# 6 Weeks:
#   Open  = 6 weeks
#   Close = 10 weeks
#
# 14 Weeks:
#   Open  = 10 weeks + 1 day
#   Close = 20 weeks
#
# 6 Months:
#   Open  = 20 weeks + 1 day
#   Close = 30 weeks
#
# Confirm these against the approved IPMH protocol/SOP.
# ------------------------------------------------------------

retention_windows <- delivery_dates %>%
    filter(!is.na(retention_delivery_date)) %>%
    select(
        ptid,
        retention_delivery_date,
        delivery_date_source,
        delivery_date_discrepancy
    ) %>%
    mutate(
        
        # --------------------------------------------------------
        # 6 Weeks
        # --------------------------------------------------------
        wk6_window_open =
            retention_delivery_date + weeks(6),
        
        wk6_window_close =
            retention_delivery_date + weeks(10),
        
        # --------------------------------------------------------
        # 14 Weeks
        # --------------------------------------------------------
        wk14_window_open =
            retention_delivery_date + weeks(10) + days(1),
        
        wk14_window_close =
            retention_delivery_date + weeks(20),
        
        # --------------------------------------------------------
        # 6 Months
        # --------------------------------------------------------
        mo6_window_open =
            retention_delivery_date + weeks(20) + days(1),
        
        mo6_window_close =
            retention_delivery_date + weeks(30)
    )


# ------------------------------------------------------------
# 10. Extract follow-up visit records from PPW RCT database
# ------------------------------------------------------------

visits <- ppw_rct_df %>%
    mutate(arm = case_when(
        grepl("Arm 1: Intervention", redcap_event_name) ~ "Intervention",
        grepl("Arm 2: Control", redcap_event_name) ~ "Control",
        TRUE ~ "Unknown"
    )) %>% 
    filter(
        grepl(
            "^(6 Weeks|14 Weeks|6 Months)",
            redcap_event_name
        ),
        is.na(redcap_repeat_instance),
        clt_visit %in% c(
            "6 weeks post-partum",
            "14 weeks post-partum",
            "6 months post-partum"
        )
    ) %>%
    select(
        record_id, clt_study_site, redcap_event_name,
        clt_visit, clt_date, mv_visit, arm
    ) %>%
    mutate(
        record_id = as.integer(record_id),
        
        clt_date = as.Date(clt_date),
        
        # Clean non-breaking spaces
        mv_visit = gsub("\u00A0", " ", mv_visit),
        
        # Remove leading/trailing spaces
        mv_visit = trimws(mv_visit),
        
        # Treat blank strings as missing
        mv_visit = na_if(mv_visit, "")
    )


# ------------------------------------------------------------
# 11. Check for duplicate follow-up records
# ------------------------------------------------------------

duplicate_followup_records <- visits %>%
    count(
        record_id,
        clt_visit
    ) %>%
    filter(n > 1) %>%
    arrange(
        record_id,
        clt_visit
    )


# ------------------------------------------------------------
# 12. Derive participant-level attendance
# ------------------------------------------------------------

attendance <- visits %>%
    group_by(
        ptid = record_id, clt_study_site, arm
    ) %>%
    summarise(
        
        # ========================================================
        # 6 WEEKS
        # ========================================================
        
        six_weeks_date = {
            x <- clt_date[
                clt_visit == "6 weeks post-partum" &
                    is.na(mv_visit) &
                    !is.na(clt_date)
            ]
            
            if (length(x) == 0) {
                as.Date(NA)
            } else {
                max(x)
            }
        },
        
        six_weeks_missed = any(
            clt_visit == "6 weeks post-partum" &
                !is.na(mv_visit)
        ),
        
        # ========================================================
        # 14 WEEKS
        # ========================================================
        
        fourteen_weeks_date = {
            x <- clt_date[
                clt_visit == "14 weeks post-partum" &
                    is.na(mv_visit) &
                    !is.na(clt_date)
            ]
            
            if (length(x) == 0) {
                as.Date(NA)
            } else {
                max(x)
            }
        },
        
        fourteen_weeks_missed = any(
            clt_visit == "14 weeks post-partum" &
                !is.na(mv_visit)
        ),
        
        # ========================================================
        # 6 MONTHS
        # ========================================================
        
        six_months_date = {
            x <- clt_date[
                clt_visit == "6 months post-partum" &
                    is.na(mv_visit) &
                    !is.na(clt_date)
            ]
            
            if (length(x) == 0) {
                as.Date(NA)
            } else {
                max(x)
            }
        },
        
        six_months_missed = any(
            clt_visit == "6 months post-partum" &
                !is.na(mv_visit)
        ),
        
        .groups = "drop"
    )


# ------------------------------------------------------------
# 13. Combine delivery windows and attendance
# ------------------------------------------------------------

retention_data <- retention_windows %>%
    full_join(
        attendance,
        by = "ptid"
    )%>%
    distinct(ptid, .keep_all = TRUE)


# ------------------------------------------------------------
# 14. Determine today's date
# ------------------------------------------------------------

# today <- Sys.Date()
# Use data freeze as the reference date for retention
today <- as.Date(data_freeze)

# ------------------------------------------------------------
# 15. Create retention status
# ------------------------------------------------------------
#
# IMPORTANT:
#
# "Attended" means:
#   Actual visit date is within the protocol window.
#
# "Attended Outside Window" means:
#   A visit was recorded, but it occurred outside the
#   protocol-defined window.
#
# "Missed" means:
#   Explicit missed-visit record OR
#   follow-up window has closed without an attended visit.
#
# "Window Open":
#   Participant is currently within their follow-up window.
#
# "Not Due":
#   Follow-up window has not yet opened.
# ------------------------------------------------------------

retention_status <- retention_data %>%
    mutate(
        
        # 6 Weeks
        six_weeks_status = case_when(
            !is.na(six_weeks_date) ~ "Attended",
            today > wk6_window_close ~ "Missed",
            today >= wk6_window_open ~ "Window Open",
            TRUE ~ "Not Due"
        ),
        
        # 14 Weeks
        fourteen_weeks_status = case_when(
            !is.na(fourteen_weeks_date) ~ "Attended",
            today > wk14_window_close ~ "Missed",
            today >= wk14_window_open ~ "Window Open",
            TRUE ~ "Not Due"
        ),
        
        # 6 Months
        six_months_status = case_when(
            !is.na(six_months_date) ~ "Attended",
            today > mo6_window_close ~ "Missed",
            today >= mo6_window_open ~ "Window Open",
            TRUE ~ "Not Due"
        )
    ) %>% 
    filter(!is.na(clt_study_site))

# ============================================================
# 16. RETENTION SUMMARY FUNCTION
# ============================================================
#
# This function creates a consistent summary for each visit.
#
# Retention denominator:
#
#   Attended + Missed
#
# Window Open is NOT included.
#
# Attended Outside Window is reported separately and is NOT
# counted as successful retention within the protocol window.
# ============================================================

calculate_retention <- function(
        data,
        status_variable,
        visit_label
) {
    
    status <- data[[status_variable]]
    
    attended <- sum(
        status == "Attended",
        na.rm = TRUE
    )
    
    attended_outside_window <- sum(
        status == "Attended Outside Window",
        na.rm = TRUE
    )
    
    missed <- sum(
        status == "Missed",
        na.rm = TRUE
    )
    
    window_open <- sum(
        status == "Window Open",
        na.rm = TRUE
    )
    
    not_due <- sum(
        status == "Not Due",
        na.rm = TRUE
    )
    
    # Primary retention denominator
    eligible_for_retention <- attended + missed
    
    retention <- ifelse(
        eligible_for_retention > 0,
        attended / eligible_for_retention,
        NA_real_
    )
    
    tibble(
        Visit = visit_label,
        
        `Participants Due` =
            eligible_for_retention,
        
        Attended =
            attended,
        
        Missed =
            missed,
        
        `Attended Outside Window` =
            attended_outside_window,
        
        `Window Open` =
            window_open,
        
        `Not Due` =
            not_due,
        
        `Percentage Attended` =
            round(retention * 100, 1)
    )
}


# ============================================================
# 17. OVERALL RETENTION
# ============================================================

retention_summary <- bind_rows(
    
    # 6 Weeks
    retention_status %>%
        filter(six_weeks_status != "Not Due") %>%
        summarise(
            Visit = "6 Weeks",
            `Participants Due` = n(),
            Attended = sum(
                six_weeks_status == "Attended"
            ),
            `Window Open` = sum(
                six_weeks_status == "Window Open"
            ),
            Missed = sum(
                six_weeks_status == "Missed"
            )
        ),
    
    # 14 Weeks
    retention_status %>%
        filter(fourteen_weeks_status != "Not Due") %>%
        summarise(
            Visit = "14 Weeks",
            `Participants Due` = n(),
            Attended = sum(
                fourteen_weeks_status == "Attended"
            ),
            `Window Open` = sum(
                fourteen_weeks_status == "Window Open"
            ),
            Missed = sum(
                fourteen_weeks_status == "Missed"
            )
        ),
    
    # 6 Months
    retention_status %>%
        filter(six_months_status != "Not Due") %>%
        summarise(
            Visit = "6 Months",
            `Participants Due` = n(),
            Attended = sum(
                six_months_status == "Attended"
            ),
            `Window Open` = sum(
                six_months_status == "Window Open"
            ),
            Missed = sum(
                six_months_status == "Missed"
            )
        )
) %>%
    mutate(
        Retention = Attended / (`Participants Due` - `Window Open`),
        Retention = scales::percent(
            Retention,
            accuracy = 0.1
        )
    )



overall_tbl <- gt(retention_summary) %>%
    
    # Table title
    tab_header(
        title = md("**Overall Retention Across Study Follow-up**"),
        subtitle = md("All facilities combined")
    ) %>%
    
    # Professional, consistent font
    opt_table_font(
        font = list(
            google_font("Arial"),
            default_fonts()
        )
    ) %>%
    
    # Format column headers
    tab_style(
        style = list(
            cell_text(
                weight = "bold",
                color = "#24364B",
                size = px(11)
            ),
            cell_fill(color = "#EAF0F6"),
            cell_borders(
                sides = "bottom",
                color = "#65758B",
                weight = px(1.5)
            )
        ),
        locations = cells_column_labels(
            columns = everything()
        )
    ) %>%
    
    # Format table title
    tab_style(
        style = cell_text(
            weight = "bold",
            size = px(15),
            color = "#24364B"
        ),
        locations = cells_title(groups = "title")
    ) %>%
    
    # Format subtitle
    tab_style(
        style = cell_text(
            size = px(10),
            color = "#606B78"
        ),
        locations = cells_title(groups = "subtitle")
    ) %>%
    
    # Align columns according to content
    cols_align(
        align = "left",
        columns = where(is.character)
    ) %>%
    cols_align(
        align = "center",
        columns = where(is.numeric)
    ) %>%
    
    # Table-wide formatting
    tab_options(
        table.font.size = px(10),
        table.background.color = "white",
        table.border.top.style = "none",
        table.border.bottom.style = "solid",
        table.border.bottom.color = "#AAB4C0",
        table.border.bottom.width = px(1),
        column_labels.border.top.style = "none",
        column_labels.border.bottom.width = px(1.5),
        table_body.border.top.style = "none",
        table_body.hlines.style = "solid",
        table_body.hlines.color = "#E2E7ED",
        table_body.hlines.width = px(0.5),
        data_row.padding = px(5),
        heading.padding = px(6),
        footnotes.font.size = px(9),
        footnotes.padding = px(4)
    ) %>%
    
    # Explain the retention definition
    tab_footnote(
        footnote = md(
            "Retention = (Attended + Window Open) / Participants Due. Participants whose visit windows remain open are included in the retention numerator."
        ),
        locations = cells_title(groups = "title")
    )
# ============================================================
# 18. FACILITY-LEVEL RETENTION FUNCTION
# ============================================================

facility_retention_summary <- bind_rows(
    
    # 6 Weeks
    retention_status %>%
        filter(six_weeks_status != "Not Due") %>%
        group_by(clt_study_site) %>%
        summarise(
            Visit = "6 Weeks",
            `Participants Due` = n(),
            Attended = sum(
                six_weeks_status == "Attended"
            ),
            Missed = sum(
                six_weeks_status == "Missed"
            ),
            `Window Open` = sum(
                six_weeks_status == "Window Open"
            ),
            .groups = "drop"
        ),
    
    # 14 Weeks
    retention_status %>%
        filter(fourteen_weeks_status != "Not Due") %>%
        group_by(clt_study_site) %>%
        summarise(
            Visit = "14 Weeks",
            `Participants Due` = n(),
            Attended = sum(
                fourteen_weeks_status == "Attended"
            ),
            Missed = sum(
                fourteen_weeks_status == "Missed"
            ),
            `Window Open` = sum(
                fourteen_weeks_status == "Window Open"
            ),
            .groups = "drop"
        ),
    
    # 6 Months
    retention_status %>%
        filter(six_months_status != "Not Due") %>%
        group_by(clt_study_site) %>%
        summarise(
            Visit = "6 Months",
            `Participants Due` = n(),
            Attended = sum(
                six_months_status == "Attended"
            ),
            Missed = sum(
                six_months_status == "Missed"
            ),
            `Window Open` = sum(
                six_months_status == "Window Open"
            ),
            .groups = "drop"
        )
) %>%
    mutate(
        Retention = Attended / (`Participants Due` - `Window Open`),
        Retention = scales::percent(
            Retention,
            accuracy = 0.1
        )
    ) %>%
    arrange(
        Visit,
        desc(Retention)
    )


# ============================================================
# 19. FACILITY RETENTION
# ============================================================

# 6 Weeks Summary
wk6_facility_retention <- retention_status %>%
    filter(six_weeks_status != "Not Due") %>%
    group_by(clt_study_site) %>%
    summarise(
        `Participants Due` = n(),
        Attended = sum(six_weeks_status == "Attended"),
        `Window Open` = sum(six_weeks_status == "Window Open"),
        Missed = sum(six_weeks_status == "Missed"),
        .groups = "drop"
    ) %>%
    mutate(
        retention_numeric = Attended / (`Participants Due` - `Window Open`)
    ) %>%
    arrange(desc(retention_numeric)) %>%
    mutate(
        Retention = scales::percent(
            retention_numeric,
            accuracy = 0.1
        )
    ) %>%
    select(-retention_numeric)

wk6_tbl <- gt(wk6_facility_retention) %>%
    tab_header(
        title = md("**Six Weeks Retention by Facility**")
    ) %>%
    opt_table_font(
        font = list(
            google_font("Chivo"),
            default_fonts()
        )
    ) %>%
    tab_style(
        locations = cells_column_labels(columns = everything()),
        style = list(
            cell_borders(sides = "bottom", weight = px(2)),
            cell_text(weight = "bold")
        )
    ) %>%
    tab_options(
        table.font.size = px(12),
        table.border.top.style = "none",
        column_labels.border.bottom.width = 2,
        table_body.border.top.style = "none",
        data_row.padding = px(4)
    )



# 14 Weeks Summary
wk14_facility_retention <- retention_status %>%
    filter(fourteen_weeks_status != "Not Due") %>%
    group_by(clt_study_site) %>%
    summarise(
        `Participants Due` = n(),
        Attended = sum(fourteen_weeks_status == "Attended"),
        `Window Open` = sum(fourteen_weeks_status == "Window Open"),
        Missed = sum(fourteen_weeks_status == "Missed"),
        .groups = "drop"
    ) %>%
    mutate(
        retention_numeric = Attended / (`Participants Due` - `Window Open`)
    ) %>%
    arrange(desc(retention_numeric)) %>%
    mutate(
        Retention = scales::percent(
            retention_numeric,
            accuracy = 0.1
        )
    ) %>%
    select(-retention_numeric)

wk14_tbl <- gt(wk14_facility_retention) %>%
    tab_header(
        title = md("**Fourteen Weeks Retention by Facility**")
    ) %>%
    opt_table_font(
        font = list(
            google_font("Chivo"),
            default_fonts()
        )
    ) %>%
    tab_style(
        locations = cells_column_labels(columns = everything()),
        style = list(
            cell_borders(sides = "bottom", weight = px(2)),
            cell_text(weight = "bold")
        )
    ) %>%
    tab_options(
        table.font.size = px(12),
        table.border.top.style = "none",
        column_labels.border.bottom.width = 2,
        table_body.border.top.style = "none",
        data_row.padding = px(4)
    )



# 6 Months Summary
mo6_facility_retention <- retention_status %>%
    filter(six_months_status != "Not Due") %>%
    group_by(clt_study_site) %>%
    summarise(
        `Participants Due` = n(),
        Attended = sum(six_months_status == "Attended"),
        `Window Open` = sum(six_months_status == "Window Open"),
        Missed = sum(six_months_status == "Missed"),
        
        .groups = "drop"
    ) %>%
    mutate(
        retention_numeric = Attended / (`Participants Due` - `Window Open`)
    ) %>%
    arrange(desc(retention_numeric)) %>%
    mutate(
        Retention = scales::percent(
            retention_numeric,
            accuracy = 0.1
        )
    ) %>%
    select(-retention_numeric)
   
mo6_tbl <- gt(mo6_facility_retention) %>%
    tab_header(
        title = md("**Six Months Retention by Facility**")
    ) %>%
    opt_table_font(
        font = list(
            google_font("Chivo"),
            default_fonts()
        )
    ) %>%
    tab_style(
        locations = cells_column_labels(columns = everything()),
        style = list(
            cell_borders(sides = "bottom", weight = px(2)),
            cell_text(weight = "bold")
        )
    ) %>%
    tab_options(
        table.font.size = px(12),
        table.border.top.style = "none",
        column_labels.border.bottom.width = 2,
        table_body.border.top.style = "none",
        data_row.padding = px(4)
    )

 

# ============================================================
# 20. Missed visits
# ============================================================
#
# Only participants whose window has closed are classified
# as missed.
# ============================================================

missed_6wks <- retention_status %>%
    filter(
        six_weeks_status == "Missed"
    ) %>%
    select(
        clt_study_site,
        ptid,
        retention_delivery_date,
        wk6_window_open,
        wk6_window_close,
        six_weeks_date,
        six_weeks_missed,
        six_weeks_status
    )


missed_14wks <- retention_status %>%
    filter(
        fourteen_weeks_status == "Missed"
    ) %>%
    select(
        clt_study_site,
        ptid,
        retention_delivery_date,
        wk14_window_open,
        wk14_window_close,
        fourteen_weeks_date,
        fourteen_weeks_missed,
        fourteen_weeks_status
    )


missed_6months <- retention_status %>%
    filter(
        six_months_status == "Missed"
    ) %>%
    select(
        clt_study_site,
        ptid,
        retention_delivery_date,
        mo6_window_open,
        mo6_window_close,
        six_months_date,
        six_months_missed,
        six_months_status
    )



# ============================================================
# 21. Follow-up windows closing soon
# ============================================================
#
# Set the period you want to monitor.
#
# Example: next 7 days
# ============================================================

window_start <- today
window_end <- today + days(7)


followups_closing_soon <- bind_rows(
    
    retention_status %>%
        filter(
            six_weeks_status == "Window Open",
            wk6_window_close >= window_start,
            wk6_window_close <= window_end
        ) %>%
        transmute(
            clt_study_site,
            ptid,
            Visit = "6 Weeks",
            Window_Open = wk6_window_open,
            Window_Close = wk6_window_close,
            Days_Remaining =
                as.integer(wk6_window_close - today)
        ),
    
    retention_status %>%
        filter(
            fourteen_weeks_status == "Window Open",
            wk14_window_close >= window_start,
            wk14_window_close <= window_end
        ) %>%
        transmute(
            clt_study_site,
            ptid,
            Visit = "14 Weeks",
            Window_Open = wk14_window_open,
            Window_Close = wk14_window_close,
            Days_Remaining =
                as.integer(wk14_window_close - today)
        ),
    
    retention_status %>%
        filter(
            six_months_status == "Window Open",
            mo6_window_close >= window_start,
            mo6_window_close <= window_end
        ) %>%
        transmute(
            clt_study_site,
            ptid,
            Visit = "6 Months",
            Window_Open = mo6_window_open,
            Window_Close = mo6_window_close,
            Days_Remaining =
                as.integer(mo6_window_close - today)
        )
) %>%
    arrange(
        Window_Close,
        clt_study_site,
        ptid
    )


# ============================================================
# 22. Follow-ups closing by facility
# ============================================================

followups_closing_by_facility <- followups_closing_soon %>%
    group_by(
        clt_study_site,
        Visit
    ) %>%
    summarise(
        `Follow-ups Closing` = n(),
        .groups = "drop"
    )



# ============================================================
# 23. Final QC summary
# ============================================================

retention_qc_summary <- tibble(
    
    `Total Participants` =
        nrow(retention_status),
    
    `Missing Delivery Date` =
        sum(
            is.na(retention_status$retention_delivery_date)
        ),
    
    `Delivery Date Discrepancies` =
        sum(
            retention_status$delivery_date_discrepancy,
            na.rm = TRUE
        ),
    
    `6 Week Duplicate Records` =
        sum(
            duplicate_followup_records$clt_visit ==
                "6 weeks post-partum",
            na.rm = TRUE
        ),
    
    `14 Week Duplicate Records` =
        sum(
            duplicate_followup_records$clt_visit ==
                "14 weeks post-partum",
            na.rm = TRUE
        ),
    
    `6 Month Duplicate Records` =
        sum(
            duplicate_followup_records$clt_visit ==
                "6 months post-partum",
            na.rm = TRUE
        ),
    
    `6 Week Outside Window` =
        sum(
            retention_status$six_weeks_status ==
                "Attended Outside Window",
            na.rm = TRUE
        ),
    
    `14 Week Outside Window` =
        sum(
            retention_status$fourteen_weeks_status ==
                "Attended Outside Window",
            na.rm = TRUE
        ),
    
    `6 Month Outside Window` =
        sum(
            retention_status$six_months_status ==
                "Attended Outside Window",
            na.rm = TRUE
        )
)

# RETENTION OF WLHIV----
# ------------------------------------------------------------
# Identify WLHIV participants at enrollment
# ------------------------------------------------------------

wlhiv_ids <- ppw_rct_df %>%
    filter(
        med_pastdiag___2 == "Checked",
        !is.na(clt_ptid)
    ) %>%
    transmute(
        ptid = as.integer(clt_ptid)
    ) %>%
    distinct(ptid)

# Check unique WLHIV participants
nrow(wlhiv_ids)


# ------------------------------------------------------------
# WLHIV cohort retention status
# ------------------------------------------------------------

retention_status_wlhiv <- retention_status %>%
    semi_join(
        wlhiv_ids,
        by = "ptid"
    ) %>%
    distinct(ptid, .keep_all = TRUE)

# Verify cohort size
n_distinct(retention_status_wlhiv$ptid)

# ============================================================
# WLHIV OVERALL RETENTION
# ============================================================

retention_summary_wlhiv <- bind_rows(
    
    # 6 Weeks
    retention_status_wlhiv %>%
        filter(six_weeks_status != "Not Due") %>%
        summarise(
            Visit = "6 Weeks",
            `Participants Due` = n(),
            Attended = sum(six_weeks_status == "Attended"),
            `Window Open` = sum(six_weeks_status == "Window Open"),
            Missed = sum(six_weeks_status == "Missed")
        ),
    
    # 14 Weeks
    retention_status_wlhiv %>%
        filter(fourteen_weeks_status != "Not Due") %>%
        summarise(
            Visit = "14 Weeks",
            `Participants Due` = n(),
            Attended = sum(fourteen_weeks_status == "Attended"),
            `Window Open` = sum(fourteen_weeks_status == "Window Open"),
            Missed = sum(fourteen_weeks_status == "Missed")
        ),
    
    # 6 Months
    retention_status_wlhiv %>%
        filter(six_months_status != "Not Due") %>%
        summarise(
            Visit = "6 Months",
            `Participants Due` = n(),
            Attended = sum(six_months_status == "Attended"),
            `Window Open` = sum(six_months_status == "Window Open"),
            Missed = sum(six_months_status == "Missed")
        )
    
) %>%
    mutate(
        Retention = scales::percent(
            Attended / (`Participants Due` - `Window Open`),
            accuracy = 0.1
        )
    )

overall_wlhiv_tbl <- gt(retention_summary_wlhiv) %>%
    
    tab_header(
        title = md("**Retention Among Women Living with HIV**"),
        subtitle = md("All facilities combined")
    ) %>%
    opt_table_font(
        font = list(
            google_font("Arial"),
            default_fonts()
        )
    ) %>%
    
    tab_style(
        style = list(
            cell_text(
                weight = "bold",
                color = "#24364B",
                size = px(11)
            ),
            cell_fill(color = "#EAF0F6"),
            cell_borders(
                sides = "bottom",
                color = "#65758B",
                weight = px(1.5)
            )
        ),
        locations = cells_column_labels(everything())
    ) %>%
    
    tab_style(
        style = cell_text(
            weight = "bold",
            size = px(15),
            color = "#24364B"
        ),
        locations = cells_title(groups = "title")
    ) %>%
    
    cols_align(
        align = "left",
        columns = Visit
    ) %>%
    
    cols_align(
        align = "center",
        columns = -Visit
    ) %>%
    
    tab_options(
        table.font.size = px(10),
        data_row.padding = px(5),
        table.align = "center"
    ) %>%
    
    tab_footnote(
        footnote = md(
            "Retention = (Attended + Window Open) / Participants Due."
        ),
        locations = cells_title(groups = "title")
    )

overall_wlhiv_tbl
