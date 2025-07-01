#______________________________________________________________

# Loading Participants Scheduling and Follow-up

# Load Packages---------------------------------------
rm(list = ls())
pacman::p_load(googledrive, googlesheets4, readxl,dplyr,purrr, stringr, lubridate)

#----------------------------------------------------------------

# Authenticate if needed
gs4_auth()

# Your Google Sheet ID or URL
sheet_id <- "https://docs.google.com/spreadsheets/d/1E1spWNY_hoRoTzQG9D0YWXC-jG8LAk7eoPCfw_l2pGw/edit?gid=1699758718#gid=1699758718"  # or use full URL

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
      message("Skipping '", name, "' — missing required columns.")
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


wk6_retention_summary <- all_deliveries %>%
   group_by(Facility) %>% 
   reframe(
      `Window not Closed` = sum(wk6_window_open > today()),
      Expected = sum(wk6_window_close < today()|attended_6wks == 1),
      Attended = sum(attended_6wks == 1),
      `Percentage Attended` = round(Attended / Expected * 100, 1)
   )

wk14_retention_summary <- all_deliveries %>%
   group_by(Facility) %>% 
   reframe(
      `Window not Closed` = sum(wk14_window_open > today()),
      Expected = sum(wk14_window_close < today()|attended_14wks == 1),
      Attended = sum(attended_14wks == 1),
      `Percentage Attended` = round(Attended / Expected * 100, 1)
   )

