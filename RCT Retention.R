#______________________________________________________________

# Loading Participants Scheduling and Follow-up

# Load Packages---------------------------------------
rm(list = ls())
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")

#----------------------------------------------------------------

# Authenticate if needed
gs4_auth()

# Your Google Sheet ID or URL
sheet_id <- "https://docs.google.com/spreadsheets/d/1qOQTI62qFtLMGHWrsiI0OsbWSDbiQBXK-EJ_RBrf6iw/edit?gid=1170966687#gid=1170966687"  # or use full URL

# Get all sheet names
sheet_names <- sheet_properties(sheet_id)$name

# Read each sheet into a named list of dataframes
sheet_list <- map(set_names(sheet_names, sheet_names), ~ read_sheet(sheet_id, sheet = .x))


# Read each sheet, using second row as column names
sheet_list <- map(set_names(sheet_names, sheet_names), function(sheet) {
   read_sheet(sheet_id, sheet = sheet, skip = 1)
})

# Drop the first row, making second row column headers
sheet_list_clean <- map(sheet_list, ~ .x %>% 
                          slice(-1))


# Rename column 4:15
## new column names for positions 4 to 15
new_names <- c("reminder1_6wks", "reminder2_6wks", "due_date_6wks", "actual_visit_6wks", "reminder1_14wks", "reminder2_14wks", "due_date_14wks", "actual_visit_14wks", "reminder1_6mths", "reminder2_6mths",  "due_date_6mths", "actual_visit_6mths")

sheet_list_clean <- map(sheet_list, function(df) {
   if (ncol(df) >= 15) {
      colnames(df)[4:15] <- new_names
   }
   df
})

# List all the facilities as dataframes 
list2env(sheet_list_clean, envir = .GlobalEnv)

#### Deliveries per Facility
#--------------------------------------------------------------------

walk2(sheet_list_clean, names(sheet_list_clean), function(df, name) {
   # Only proceed if required columns exist
   required_cols <- c("Participant ID", "Delivery Date", "actual_visit_6wks", "actual_visit_14wks", "actual_visit_6mths")
   
   if (all(required_cols %in% names(df))) {
      df_clean <- df %>%
         select(all_of(required_cols)) %>%
         filter(!is.na(`Participant ID`))
      
      assign(
         paste0(str_replace_all(tolower(name), "[^a-z0-9]+", "_"), "_deliveries"),
         df_clean,
         envir = .GlobalEnv
      )
   } else {
      message("Skipping '", name, "' - missing required columns.")
   }
})

ls(pattern = "_deliveries$")


# Get only delivery datasets (excluding 'all_deliveries' itself)
delivery_dfs <- ls(pattern = "_deliveries$") |>
   setdiff(c("all_deliveries", "sheet1_deliveries")) |>
   mget()


# Bind and clean
all_deliveries <- imap_dfr(delivery_dfs, ~ .x %>%
                              mutate(
                                  `Participant ID` = as.integer(`Participant ID`),
                                 actual_visit_6wks = ymd(actual_visit_6wks),
                                 actual_visit_14wks = ymd(actual_visit_14wks),
                                 actual_visit_6mths = ymd(actual_visit_6mths),
                                 delivery_date = ymd(`Delivery Date`)
                              ) %>%
                              filter(!is.na(`delivery_date`))
) %>%
   select(ptid = `Participant ID`, delivery_date, actual_visit_6wks,
          actual_visit_14wks, actual_visit_6mths) %>%
   mutate(Facility = substr(ptid, 3, 4),
          Facility = dplyr::recode(Facility,
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
                               "22" = "Ndiru Level 4 Hospital"))


# # Generate target dates and visit window
# all_deliveries <- all_deliveries %>% 
#     mutate(
#       # 6 Weeks PNC
#       wk6_window_open  = delivery_date + weeks(6),
#       wk6_window_close = delivery_date + weeks(10),
#       
#       # 14 Weeks PNC
#       wk14_window_open  = delivery_date + weeks(10) + days(1),
#       wk14_window_close = delivery_date + weeks(20),
#       
#       # 6 Months PNC (~26 weeks)
#       mo6_window_open  = delivery_date + weeks(20) + days(1),
#       mo6_window_close = delivery_date + weeks(30)
#    )
# 
# 
# 
# # Read PPW RCT Database and extracted those who attended their visits
# visits <- ppw_rct_df %>% 
#     filter(
#         grepl("^6 Weeks|^14 Weeks|^6 Months", redcap_event_name),
#         is.na(redcap_repeat_instance),
#         clt_visit %in% c("6 weeks post-partum", 
#                          "14 weeks post-partum", 
#                          "6 months post-partum")
#     ) %>% 
#     select(record_id, redcap_event_name, clt_date, clt_visit, mv_visit )
# 
# attendance_df <- visits %>%
#     mutate(
#         six_weeks_flag = if_else(grepl("^6 weeks", clt_visit), 1, 0),
#         fourteen_weeks_flag = if_else(grepl("^14 weeks", clt_visit), 1, 0),
#         six_months_flag = if_else(grepl("^6 months post", clt_visit), 1, 0),
#         six_weeks_missed = if_else(grepl("^6 weeks", mv_visit), 1, 0),
#         fourteen_weeks_missed = if_else(grepl("^14 weeks post", mv_visit), 1, 0),
#         six_months_missed = if_else(grepl("^6 months post", mv_visit), 1, 0),
#         ) %>%
#     group_by(record_id) %>%
#     summarise(
#         six_weeks_flag = max(six_weeks_flag, na.rm = TRUE),
#         fourteen_weeks_flag = max(fourteen_weeks_flag, na.rm = TRUE),
#         six_months_flag = max(six_months_flag, na.rm = TRUE),
#         six_weeks_missed = max(six_weeks_missed, na.rm = TRUE),
#         fourteen_weeks_missed = max(fourteen_weeks_missed, na.rm = TRUE),
#         six_months_missed = max(six_months_missed, na.rm = TRUE),
#         .groups = "drop"
#     ) %>%
#     mutate(
#         six_weeks_flag = if_else(six_weeks_missed == 1, 0, six_weeks_flag),
#         fourteen_weeks_flag = if_else(fourteen_weeks_missed == 1, 0, fourteen_weeks_flag),
#         six_months_flag = if_else(six_months_missed == 1, 0, six_months_flag)
#     )
# 
# 
# all_deliveries <- all_deliveries %>% 
#     mutate(ptid = as.integer(ptid)) %>% 
#     left_join(attendance_df, by = c("ptid" = "record_id"))
# 
# 
# # 6Wks Visit:----
# ## NA, window closed; no data, missed visit not filled.
# missing_6wks_data <- all_deliveries %>% 
#     filter(is.na(six_weeks_flag) & wk6_window_close < today())
# 
# ## Missed 6 Weeks visit|Missed visit filled
# missed_6wks_visit <- all_deliveries %>% 
#     filter(six_weeks_missed == 1 & wk6_window_close < today())
# 
# beyond_window <- all_deliveries %>% 
#     #filter(Facility == "Usigu Health Centre") %>% 
#     filter(wk6_window_close < today() & six_weeks_flag == 0) %>% 
#     filter(six_weeks_missed == 0)
# 
# # 14Wks Visit:----
# ## NA, window closed; no data, missed visit not filled.
# missing_14wks_data <- all_deliveries %>% 
#     filter(is.na(fourteen_weeks_flag) & wk14_window_close < today())
# 
# ## Missed visit
# missed_14wks_visit <- all_deliveries %>% 
#     filter(wk14_window_close < today() & fourteen_weeks_missed == 1)
# 
# beyond_window <- all_deliveries %>% 
#     #filter(Facility == "Usigu Health Centre") %>% 
#     filter(wk14_window_close < today() & fourteen_weeks_flag == 0) %>% 
#     filter(fourteen_weeks_missed == 0)
# 
# # 6 Months Visit:----
# ## ## NA, window closed; no data, missed visit not filled.
# missing_6mths_data <- all_deliveries %>% 
#     filter(is.na(six_months_flag) & mo6_window_close < today())
# 
# ## Missed visit
# missed_6mths_visit <- all_deliveries %>% 
#     filter(mo6_window_close < today() & six_months_missed == 1)
# 
# beyond_window <- all_deliveries %>% 
#     #filter(Facility == "Usigu Health Centre") %>% 
#     filter(mo6_window_close < today() & six_months_flag == 0) %>% 
#     filter(six_months_missed == 0)
# 
# 
# # Retention----
# 
# #### 6 Wks Overall retention
# wk6_overall_retention <- all_deliveries %>%
#     #group_by(Facility) %>% 
#     reframe(
#         `Window not Closed` = sum(wk6_window_open > today()),
#         Expected = sum(wk6_window_close < today()|six_weeks_flag == 1, na.rm = TRUE),
#         Attended = sum(six_weeks_flag == 1, na.rm = TRUE),
#         `Percentage Attended` = round(Attended / Expected * 100, 1)
#     )#%>%
# #gt() %>%
# #tab_header(
# #title = "Six Weeks Follow-Up Retention Summary")
# 
# wk6_overall_retention
# 
# #### 6 Wks retention by Facility
# wk6_facility_retention <- all_deliveries %>%
#     group_by(Facility) %>% 
#     reframe(
#         `Window not Closed` = sum(wk6_window_open > today()),
#         Expected = sum(wk6_window_close < today()|six_weeks_flag == 1, na.rm = TRUE),
#         Attended = sum(six_weeks_flag == 1, na.rm = TRUE),
#         `Percentage Attended` = round(Attended / Expected * 100, 1)
#     ) #%>%
# #gt() %>%
# # tab_header(
# # title = "Six Weeks Follow-Up Retention Summary"
# #)
# 
# #### 14 Wks Retention
# wk14_overall_retention <- all_deliveries %>%
#     #group_by(Facility) %>% 
#     reframe(
#         `Window not Closed` = sum(wk14_window_open > today()),
#         Expected = sum(wk14_window_close < today()|fourteen_weeks_flag == 1, na.rm = TRUE),
#         Attended = sum(fourteen_weeks_flag == 1, na.rm = TRUE),
#         `Percentage Attended` = round(Attended / Expected * 100, 1)
#     ) #%>%
# #gt() %>%
# #tab_header(
# #title = "Fourteen Weeks Follow-Up Retention Summary")
# 
# 
# wk14_facility_retention <- all_deliveries %>%
#     group_by(Facility) %>% 
#     reframe(
#         `Window not Closed` = sum(wk14_window_open > today()),
#         Expected = sum(wk14_window_close < today()|fourteen_weeks_flag == 1, na.rm = TRUE),
#         Attended = sum(fourteen_weeks_flag == 1, na.rm = TRUE),
#         `Percentage Attended` = round(Attended / Expected * 100, 1)
#     )# %>%
#     #gt() %>%
#     #tab_header(
#        # title = "Fourteen Weeks Follow-Up Retention Summary")
# 
# six_mths_facility_retention <- all_deliveries %>%
#     group_by(Facility) %>% 
#     reframe(
#         `Window not Closed` = sum(mo6_window_open > today()),
#         Expected = sum(mo6_window_close < today()|six_months_flag == 1, na.rm = TRUE),
#         Attended = sum(six_months_flag == 1, na.rm = TRUE),
#         `Percentage Attended` = round(Attended / Expected * 100, 1)
#     )# %>%
# #gt() %>%
# #tab_header(
# # title = "Fourteen Weeks Follow-Up Retention Summary")
# 
# # Missed Visits----
# missed_6wks <- all_deliveries %>% 
#     select(Facility, ptid, six_weeks_flag) %>% 
#     filter(six_weeks_flag == "0")
# 
# missed_14wks <- all_deliveries %>% 
#     select(Facility, ptid, fourteen_weeks_flag, wk14_window_close) %>% 
#     filter(fourteen_weeks_flag == "0" & wk14_window_close <= Sys.Date())
# 
# missed_6months <- all_deliveries %>% 
#     select(Facility, ptid, six_months_flag, mo6_window_close) %>% 
#     filter(six_months_flag == "0" & mo6_window_close <= Sys.Date())
# 
# # Create Overall Retention Summaries
# overall_retention_tbl <- all_deliveries %>%
#     reframe(
#         wk6_window_not_closed   = sum(wk6_window_open > today(), na.rm = TRUE),
#         wk6_expected            = sum(wk6_window_close < today() | six_weeks_flag == 1, na.rm = TRUE),
#         wk6_attended            = sum(six_weeks_flag == 1, na.rm = TRUE),
#         wk14_window_not_closed  = sum(wk14_window_open > today(), na.rm = TRUE),
#         wk14_expected           = sum(wk14_window_close < today() | fourteen_weeks_flag == 1, na.rm = TRUE),
#         wk14_attended           = sum(fourteen_weeks_flag == 1, na.rm = TRUE),
#         mo6_window_not_closed   = sum(mo6_window_open > today(), na.rm = TRUE),
#         mo6_expected            = sum(mo6_window_close < today() | six_months_flag == 1, na.rm = TRUE),
#         mo6_attended            = sum(six_months_flag == 1, na.rm = TRUE)
#     ) %>%
#     pivot_longer(
#         everything(),
#         names_to = c("Visit", ".value"),
#         names_pattern = "(wk6|wk14|mo6)_(.*)"
#     ) %>%
#     mutate(
#         Visit = case_when(
#             Visit == "wk6"  ~ "6 Weeks",
#             Visit == "wk14" ~ "14 Weeks",
#             Visit == "mo6"  ~ "6 Months"
#         ),
#         percentage_attended = round(attended / expected * 100, 1)
#     )%>%
#     rename_with(~ str_to_title(.x))   # Capitalize first letter of each word
# 
# # Convert both to flextables
# ft_overall <- flextable(overall_retention_tbl) %>%
#     set_header_labels(
#         Visit = "Visit",
#         Window_not_closed = "Window Not Closed",
#         Expected = "Expected",
#         Attended = "Attended",
#         Percentage_attended = "Percentage Attended (%)"
#     )
# 
# 
# 
# # Change to flextables
# ft_6_overall <- flextable(wk6_overall_retention)
# ft_14_overall  <- flextable(wk14_overall_retention)
# ft_6 <- flextable(wk6_facility_retention)
# ft_14  <- flextable(wk14_facility_retention)
# ft_6m <- flextable(six_mths_facility_retention)
# 
# 
# # Create Word doc and add both
# 
# gt_overall <- overall_retention_tbl %>% 
#     gt() %>%
#     cols_label(
#         Visit = "Visit",
#         Window_not_closed = "Window Not Closed",
#         Expected = "Expected",
#         Attended = "Attended",
#         Percentage_attended = "Percentage Attended (%)"
#     ) %>%
#     tab_header(
#         title = "Overall Retention Summary"
#     )
# 
# 
# gt_6 <- wk6_facility_retention %>%
#     gt() %>%
#     tab_header(title = "Six Weeks Retention Summary")
# 
# gt_14 <- wk14_facility_retention %>%
#     gt() %>%
#     tab_header(title = "Fourteen Weeks Retention Summary")
# 
# gt_6m <- six_mths_facility_retention %>%
#     gt() %>%
#     tab_header(title = "Six Months Retention Summary")
# 



############################################################################

# ============================================================
# IPMH FOLLOW-UP RETENTION ANALYSIS
# ============================================================
# Purpose:
#   1. Establish the best delivery date for each participant
#   2. Calculate protocol-defined follow-up windows
#   3. Identify actual follow-up attendance
#   4. Classify participants by retention status
#   5. Produce overall and facility-level retention summaries
#
# Primary retention definition:
#
#   Retention =
#       Attended within window /
#       (Attended within window + Missed)
#
# Participants with an open follow-up window are NOT included
# in the retention denominator.
# ============================================================
###
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
# 1. Prepare delivery tracker data
# ------------------------------------------------------------

all_deliveries <- imap_dfr(
    delivery_dfs,
    ~ .x %>%
        mutate(
            ptid = as.integer(`Participant ID`),
            
            delivery_date = ymd(`Delivery Date`),
            
            actual_visit_6wks  = ymd(actual_visit_6wks),
            actual_visit_14wks = ymd(actual_visit_14wks),
            actual_visit_6mths = ymd(actual_visit_6mths)
        ) %>%
        filter(!is.na(delivery_date))
) %>%
    select(
        ptid,
        delivery_date,
        actual_visit_6wks,
        actual_visit_14wks,
        actual_visit_6mths
    ) %>%
    mutate(
        Facility = substr(as.character(ptid), 3, 4),
        
        Facility = dplyr::recode(
            Facility,
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
            "22" = "Ndiru Level 4 Hospital"
        )
    )


# ------------------------------------------------------------
# 2. Extract delivery date from the 6-week CRF
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
# 3. Compare tracker and 6-week CRF delivery dates
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
# 4. Delivery-date QC dataset
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
# 5. Calculate protocol follow-up windows
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
# 6. Extract follow-up visit records from PPW RCT database
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
# 7. Check for duplicate follow-up records
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
# 8. Derive participant-level attendance
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
# 9. Combine delivery windows and attendance
# ------------------------------------------------------------

retention_data <- retention_windows %>%
    full_join(
        attendance,
        by = "ptid"
    )


# ------------------------------------------------------------
# 10. Determine today's date
# ------------------------------------------------------------

today <- Sys.Date()


# ------------------------------------------------------------
# 11. Create retention status
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
# 12. RETENTION SUMMARY FUNCTION
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
# 13. OVERALL RETENTION
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
            Missed = sum(
                six_weeks_status == "Missed"
            ),
            `Window Open` = sum(
                six_weeks_status == "Window Open"
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
            Missed = sum(
                fourteen_weeks_status == "Missed"
            ),
            `Window Open` = sum(
                fourteen_weeks_status == "Window Open"
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
            Missed = sum(
                six_months_status == "Missed"
            ),
            `Window Open` = sum(
                six_months_status == "Window Open"
            )
        )
) %>%
    mutate(
        Retention = Attended / `Participants Due`,
        Retention = scales::percent(
            Retention,
            accuracy = 0.1
        )
    )

overall_tbl <- gt(retention_summary) %>%
    tab_header(
        title = md("**Overall Retention Across Study Follow-up**"),
        subtitle = md("All facilities combined")
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
            cell_borders(
                sides = "bottom",
                weight = px(2)
            ),
            cell_text(weight = "bold")
        )
    ) %>%
    tab_options(
        table.font.size = px(12),
        table.border.top.style = "none",
        column_labels.border.bottom.width = 2,
        table_body.border.top.style = "none",
        data_row.padding = px(3)
    )
# ============================================================
# 14. FACILITY-LEVEL RETENTION FUNCTION
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
        Retention = Attended / `Participants Due`,
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
# 15. FACILITY RETENTION
# ============================================================

# 6 Weeks Summary
wk6_facility_retention <- retention_status %>%
    filter(six_weeks_status != "Not Due") %>%
    group_by(clt_study_site) %>%
    summarise(
        `Participants Due` = n(),
        Attended = sum(six_weeks_status == "Attended"),
        Missed = sum(six_weeks_status == "Missed"),
        `Window Open` = sum(six_weeks_status == "Window Open"),
        .groups = "drop"
    ) %>%
    mutate(
        Retention = scales::percent(Attended / `Participants Due`, accuracy = 0.1)
    ) %>%
    arrange(desc(Retention))

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
        Attended = sum(six_weeks_status == "Attended"),
        Missed = sum(six_weeks_status == "Missed"),
        `Window Open` = sum(six_weeks_status == "Window Open"),
        .groups = "drop"
    ) %>%
    mutate(
        Retention = scales::percent(Attended / `Participants Due`, accuracy = 0.1)
    ) %>%
    arrange(desc(Retention))

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
        Missed = sum(six_months_status == "Missed"),
        `Window Open` = sum(six_months_status == "Window Open"),
        .groups = "drop"
    ) %>%
    mutate(
        Retention = scales::percent(Attended / `Participants Due`, accuracy = 0.1)
    ) %>%
    arrange(desc(Retention))

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
# 18. Missed visits
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
# 23. Follow-up windows closing soon
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
# 24. Follow-ups closing by facility
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
# 26. Final QC summary
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

