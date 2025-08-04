#______________________________________________________________

# Loading Participants Scheduling and Follow-up

# Load Packages---------------------------------------
rm(list = ls())
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")
pacman::p_load(googledrive, googlesheets4, readxl,dplyr,purrr, officer)

#----------------------------------------------------------------

# Authenticate if needed
gs4_auth()

# Your Google Sheet ID or URL
sheet_id <- "https://docs.google.com/spreadsheets/d/1-XXBxis7YzTIMGzHJGCGI24wFsL_lcFv7LAwCw-8Qq8/edit?gid=1699758718#gid=1699758718"  # or use full URL

# Get all sheet names
sheet_names <- sheet_properties(sheet_id)$name

# Read each sheet into a named list of dataframes
sheet_list <- map(set_names(sheet_names), ~ read_sheet(sheet_id, sheet = .x))

# Read each sheet, using second row as column names
sheet_list <- map(set_names(sheet_names), function(sheet) {
   read_sheet(sheet_id, sheet = sheet, skip = 1)
})

# Drop the first row, making second row column headers
sheet_list_clean <- map(sheet_list, ~ .x %>% 
                           slice(-1))


# Rename column 4:15
## new column names for positions 4 to 15
new_names <- c("reminder1_6wks", "reminder2_6wks", "due_date_6wks", "actual_visit_6wks", "reminder1_14wks", "reminder2_14wks", "due_date_14wks", "actual_visit_14wks", "reminder1_6mths", "reminder2_6mths",  "due_date_6mths", "actual_visit_6mths")

sheet_list_clean <- map(sheet_list_clean, function(df) {
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
   required_cols <- c("Participant ID", "Delivery Date", "actual_visit_6wks", "actual_visit_14wks")
   
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
      message("Skipping '", name, "' â missing required columns.")
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
                                 actual_visit_6wks = ymd(actual_visit_6wks),
                                 actual_visit_14wks = ymd(actual_visit_14wks),
                                 delivery_date = ymd(`Delivery Date`)
                              ) %>%
                              filter(!is.na(`delivery_date`))
) %>% 
   select(ptid = `Participant ID`, delivery_date, actual_visit_6wks, 
          actual_visit_14wks) %>% 
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


# Generate target dates and visit window
all_deliveries <- all_deliveries %>% 
   mutate(
      attended_6wks = if_else(!is.na(actual_visit_6wks), 1, 0),
      attended_14wks = if_else(!is.na(actual_visit_14wks), 1, 0)) %>% 
   mutate(
      # 6 Weeks PNC
      wk6_window_open  = delivery_date + weeks(6),
      wk6_window_close = delivery_date + weeks(10),
      
      # 14 Weeks PNC
      wk14_window_open  = delivery_date + weeks(10) + days(1),
      wk14_window_close = delivery_date + weeks(20),
      
      # 6 Months PNC (~26 weeks)
      mo6_window_open  = delivery_date + weeks(20) + days(1),
      mo6_window_close = delivery_date + weeks(30)
   )

wk6_retention_overall<- all_deliveries %>%
    reframe(
        `Window Open` = sum(wk6_window_open > today()),
        Expected = sum(wk6_window_close < today()|attended_6wks == 1),
        Attended = sum(attended_6wks == 1),
        `Percentage Attended` = round(Attended / Expected * 100, 1)
    )


wk6_retention_summary <- all_deliveries %>%
   group_by(Facility) %>% 
   reframe(
      `Window Open` = sum(wk6_window_open > today()),
      Expected = sum(wk6_window_close < today()|attended_6wks == 1),
      Attended = sum(attended_6wks == 1),
      `Percentage Attended` = round(Attended / Expected * 100, 1)
   )

# 14 Wks visit Retention
wk14_retention_overall <- all_deliveries %>%
    reframe(
        `Window Open` = sum(wk14_window_open > today()),
        Expected = sum(wk14_window_close < today()|attended_14wks == 1),
        Attended = sum(attended_14wks == 1),
        `Percentage Attended` = round(Attended / Expected * 100, 1)
    )

wk14_retention_summary <- all_deliveries %>%
   group_by(Facility) %>% 
   reframe(
      `Window Open` = sum(wk14_window_open > today()),
      Expected = sum(wk14_window_close < today()|attended_14wks == 1),
      Attended = sum(attended_14wks == 1),
      `Percentage Attended` = round(Attended / Expected * 100, 1)
   )


# Convert both to flextables
ft_6_overall <- flextable(wk6_retention_overall)
ft_14_overall  <- flextable(wk14_retention_overall)
ft_6 <- flextable(wk6_retention_summary)
ft_14  <- flextable(wk14_retention_summary)

# Create Word doc and add both
doc <- read_docx() %>%
    body_add_par("6-week Overall Retention", style = "heading 1") %>%
    body_add_flextable(ft_6_overall) %>%
    body_add_par("") %>%  # Spacer
    body_add_par("14-week Overall Retention", style = "heading 1") %>%
    body_add_flextable(ft_14_overall)%>%
    body_add_par("") %>%  # Spacer
    body_add_par("6-week Retention Summary", style = "heading 1") %>%
    body_add_flextable(ft_6) %>%
    body_add_par("") %>%  # Spacer
    body_add_par("14-week Retention Summary", style = "heading 1") %>%
    body_add_flextable(ft_14)

# Save Word file
print(doc, target = "retention_summary.docx")
print(doc, target = paste0("retention_summary_", 
                           format(Sys.time(), 
                                  "%Y-%m-%d_%H%M%S"), ".docx"))

View(all_deliveries)
# -----------------------------------------
# Filter participants whose 6-weeks or 14-weeks window closes in a week
# Define the date window
today <- as.Date("2025-08-04")
next_friday <- as.Date("2025-08-08")

closing_soon <- all_deliveries %>%
    filter(
        (wk6_window_close >= today & wk6_window_close <= next_friday) |
            (wk14_window_close >= today & wk14_window_close <= next_friday)
    )


# Summarise count of participants within range
window_summary <- all_deliveries %>%
    group_by(Facility) %>% 
    summarise(
        `Week 6 Visits` = sum(wk6_window_close >= today & wk6_window_close <= next_friday, na.rm = TRUE),
        `Week 14 Visits` = sum(wk14_window_close >= today & wk14_window_close <= next_friday, na.rm = TRUE),
        `Month 6 Visits` = sum(mo6_window_open <= next_friday & mo6_window_close <= next_friday, na.rm = TRUE)    
        ) %>%
    mutate(`Total Visits` = `Week 6 Visits` + `Week 14 Visits` + `Month 6 Visits`) %>% 
    adorn_totals(name = "Total")

# Convert to flextable
ft <- flextable(window_summary)
ft <- theme_vanilla(ft)
ft <- bold(ft, i = which(window_summary$Facility == "Total"), bold = TRUE)
ft <- autofit(ft)
ft <- set_caption(ft, "Follow-ups Due Between August 4 and August 8, 2025")

# Create Word document
doc <- read_docx() %>%
    body_add_par("Follow-ups Due Between August 4 and August 8, 2025", style = "heading 1") %>%
    body_add_par("This summary includes participants whose follow-up windows (Week 6, Week 14, or Month 6) fall between August 4 and August 8, 2025.", style = "Normal") %>%
    body_add_flextable(ft)

# Save document
print(doc, target = "Follow-up Summary for this Week.docx")



# Summary of retentions to be affected by study activities hault
start_date <- as.Date("2025-08-04")
end_date <- as.Date("2025-08-08")

pending_followups <- all_deliveries %>%
    filter(attended_6wks == 0) %>%
    filter(
        between(wk6_window_close, start_date, end_date) |
            between(wk14_window_close, start_date, end_date)
    ) %>%
    select(ptid, wk6_window_close, wk14_window_close)

###
# Tracking follow-up visits ------------------
ppw_date_track <- ppw_rct_df %>%
    mutate(
        clt_date = as.Date(clt_date),
        med_pre_edd = as.Date(med_pre_edd),
        tpnc_date = as.Date(tpnc_date)
    ) %>%
    # Create participant ID if needed
    group_by(clt_study_site) %>%
    ungroup()

