# Header ------------------------------------------------------------------

# Author(s): David Owaga & Yuwei Wang
# Date: March 25, 2025
# PM+ Follow-Up for weekly report

# Setup ------------------------------------------------------------------------

rm(list = ls())             

# Reference source codes & other dependencies: Use this section to reference other scripts and dependencies
source("DataTeam_ipmh.R")
source("Dependencies.R")
#source("REDCap_datapull.R")
source("data_import.R")

pm_follow_up <- pm_survey_df %>% 
    select(pm_ptid, pm_facility, pm_date, pm_session) %>%
    right_join(ppw_rct_df %>% 
                   select(record_id, clt_date), by = c("pm_ptid" = "record_id")) %>% 
    select(pm_ptid, clt_date, pm_date, pm_session) %>% 
    filter(!is.na(pm_date)) %>% 
    arrange(pm_ptid)

#David's code-------------
# Function to generate weekly visit status
generate_weekly_visits <- function(pm_follow_up) {
    pm_follow_up %>%
        mutate(clt_date = as.Date(clt_date),  # Ensure clt_date is Date
               pm_date = as.Date(pm_date)) %>%  # Ensure pm_date is Date
        group_by(pm_ptid) %>%
        summarise(clt_date = first(clt_date), 
                  pm_dates = list(na.omit(pm_date)), 
                  .groups = "drop") %>%
        rowwise() %>%
        mutate(
            week_1 = as.integer(any(pm_dates > clt_date & pm_dates <= clt_date + 7)),
            week_2 = as.integer(any(pm_dates > clt_date + 7 & pm_dates <= clt_date + 14)),
            week_3 = as.integer(any(pm_dates > clt_date + 14 & pm_dates <= clt_date + 21)),
            week_4 = as.integer(any(pm_dates > clt_date + 21 & pm_dates <= clt_date + 28)),
            week_5 = as.integer(any(pm_dates > clt_date + 28 & pm_dates <= clt_date + 35)),
            week_6 = as.integer(any(pm_dates > clt_date + 35 & pm_dates <= clt_date + 42))
        ) %>%
        select(pm_ptid, clt_date, week_1, week_2, week_3, week_4, week_5, week_6)
}
# Apply function
weekly_visits <- generate_weekly_visits(pm_follow_up)

library(gt)
# View result
weekly_visits %>% 
    gt() %>%
    tab_header(
        title = "Summary of PM+ Enrollments",
        subtitle = "Showing Participants Enrolled in PM+ and Follow-up"
    ) %>%
    tab_options(
        table.font.size = px(12)
    )


library(dplyr)
library(tidyr)

pm_retention <- weekly_visits %>% 
    select(pm_ptid, clt_date)

#### Generate target dates and visit window ####################################
     ### Week 1
pm_retention <- pm_retention %>%
    mutate(
        clt_date = ymd(clt_date),
        week1_open = clt_date + days(7),
        week1_closer = clt_date + days(14),
        week2_open = clt_date + days(14),
        week2_closer = clt_date + days(21),
        week3_open = clt_date + days(21),
        week3_closer = clt_date + days(28),
        week4_open = clt_date + days(28),
        week4_closer = clt_date + days(35),
        week5_open = clt_date + days(35),
        week5_closer = clt_date + days(42))

pm_visits <- pm_survey_df %>% 
    select(pm_facility, pm_ptid, pm_date, pm_session)

retention <- left_join(pm_visits, pm_retention, by='pm_ptid')

################################################################################
cv <- retention %>% 
    mutate(
        week_category = case_when(
            pm_date <= week1_open ~ "Session 0",
            pm_date >= week1_open & pm_date <= week1_closer ~ "Week 1",
            pm_date >= week2_open & pm_date <= week2_closer ~ "Week 2",
            pm_date >= week3_open & pm_date <= week3_closer ~ "Week 3",
            pm_date >= week4_open & pm_date <= week4_closer ~ "Week 4",
            pm_date >= week5_open & pm_date <= week5_closer ~ "Week 5",
            TRUE ~ "Outside Expected Range"))

attendance_summary <- cv %>%
    group_by(pm_ptid) %>%
    summarise(
        expected = n_distinct(pm_ptid),  # Unique participants expected
        attended = n(),  # Total sessions attended
        not_yet_closed = sum(pm_date <= week5_closer)  # Still within expected period
    ) %>%
    arrange(week_category)

print(attendance_summary)
################################################################################

## Duplicate Session ####
duplicates <- retention %>% 
    janitor::get_dupes(c(pm_ptid, pm_session))

V_week1 <- retention %>%
    mutate(across(c(week1_closer, pm_date, week1_open), ymd)) %>%
    #filter(pm_session == "Session 1") %>% 
    group_by(pm_facility) %>%
    summarise(Week_not_closed = sum(week1_closer > today()),
              Expected = sum(week1_open <= today() & week1_closer > today()),
              attended = sum(pm_date <= week1_closer & pm_session == "Session 1"),
              percentage_attended = attended / Expected * 100) %>% 
    adorn_totals("row", fill = "Total")
    


V_week2 <- retention %>%
    mutate(across(c(week2_closer, pm_date, week2_open), ymd)) %>%
    group_by(pm_facility) %>%
    summarise(Week_not_closed = sum(week2_closer > today()),
              Expected = sum(week2_open <= today_date & week2_closer > today_date),
              Attended = sum(pm_date <= week2_closer & pm_session == "Session 2"),
              percentage_attended = Attended / Expected * 100)

V_week3 <- retention %>%
    mutate(across(c(week3_closer, pm_date, week3_open), ymd)) %>%
    group_by(pm_facility) %>%
    summarise(Week_not_closed = sum(week3_closer > today()),
              Expected = sum(week3_closer < today()),
              Attended = sum(pm_date <= week3_closer & pm_session == "Session 3"),
              percentage_attended = Attended / Expected * 100)

#Yuwei's code--------------
pm_intervals <- pm_follow_up %>%
    mutate(
        clt_date = as.Date(clt_date),
        pm_date = as.Date(pm_date)
    ) %>%
    arrange(pm_ptid, pm_session) %>%
    group_by(pm_ptid) %>%
    mutate(
        session_order = row_number(),
        ref_date = if_else(session_order == 1, clt_date, lag(pm_date)),
        days_since_previous = as.numeric(pm_date - ref_date)
    ) %>%
    ungroup()

pm_intervals <- pm_intervals %>%
    mutate(attendance_timing = case_when(
        days_since_previous < 0              ~ "early attendance",
        days_since_previous >= 0 & days_since_previous <= 14 ~ "on time",
        days_since_previous > 14             ~ "delay",
        TRUE                                 ~ NA_character_
    ))

#table for attendance timing (type)
attendance_table <- pm_intervals %>%
    filter(!is.na(attendance_timing)) %>%
    group_by(pm_session, attendance_timing) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(pm_session) %>%
    mutate(
        total = sum(n),
        percent = round(100 * n / total, 1),
        label = paste0(n, " (", percent, "%)")
    ) %>%
    select(pm_session, attendance_timing, label) %>%
    pivot_wider(
        names_from = attendance_timing,
        values_from = label,
        values_fill = "-"
    )
kable(attendance_table)

#table for average interval (day)
average_interval <- pm_intervals %>%
    filter(!is.na(days_since_previous)) %>%
    group_by(pm_session) %>%
    summarise(
        n = n(),
        mean_days = round(mean(days_since_previous, na.rm = TRUE), 1),
        sd_days = round(sd(days_since_previous, na.rm = TRUE), 1),
        .groups = "drop"
    ) %>%
    mutate(
        label = paste0(mean_days, " (±", sd_days, ")")
    ) %>%
    select(pm_session, n, average_interval = label)

average_interval %>% kable()
