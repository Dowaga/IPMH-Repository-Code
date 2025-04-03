# Header ------------------------------------------------------------------

# Author(s): Owaga
# Date: March 11, 2025
# PM+ Follow-Up for weekly report

# Setup ------------------------------------------------------------------------

# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")

pm_follow_up <- pm_survey_df %>% 
    select(pm_ptid, pm_facility, pm_date, pm_session) %>%
    right_join(pm_telep_df %>% 
                   select(record_id, clt_date), by = c("pm_ptid" = "record_id")) %>% 
    select(pm_ptid, clt_date, pm_date, pm_session) %>% 
    filter(!is.na(pm_date)) %>% 
    arrange(pm_ptid)


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


#### Generate target dates and visit window ####################################

pm_enrollment_dates <- weekly_visits %>% 
    select(pm_ptid, clt_date)

pm_visit_dates <- pm_enrollment_dates %>%
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
    filter(ipmh_participant == "Yes") %>%
    select(pm_facility,pm_ptid,pm_date, pm_session) %>% 
    filter(pm_ptid == "21166521")

# Duplicates Sessions
duplicates_visit <- pm_visits %>%
    group_by(pm_ptid, pm_session) %>%
    summarise(count = n(), .groups = "drop") %>%
    filter(count > 1)  # Filter those with more than one attendance per session


# Transforming data: One column for ptid, each session as a separate column
pm_visits_wide <- pm_visits %>%
    distinct(pm_ptid, pm_session) %>%  # Remove duplicate sessions per ptid
    mutate(attended = 1) %>%  # Assign 1 for attended
    pivot_wider(names_from = pm_session, values_from = attended, values_fill = list(attended = 0)) %>%   # Fill non-attended sessions with 0
    mutate(facility = str_sub(pm_ptid, 3, 4)) %>% 
    mutate(facility = recode(facility,
                             "01" = "Rwambwa SCH",
                             "03" = "Uyawi SCH",
                             "04" = "Got Agulu SCH",
                             "07" = "Kabondo SCH",
                             "09" = "Miriu HC",
                             "13" = "Ober Kamoth SCH",
                             "16" = "Usigu HC",
                             "17" = "Ramula HC",
                             "19" = "Airport HC (Kisumu)",
                             "22" = "Ndiru Level 4"))

retention <- left_join(pm_session_wide, pm_visit_dates, by='pm_ptid')


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
            TRUE ~ "Outside Expected Range")) %>% 
    filter(!is.na(pm_ptid))#filter out none study participants


################################################################################

## Duplicate Session ####
duplicates <- retention %>% 
    janitor::get_dupes(c(pm_ptid, pm_session))

V_week1 <- retention %>%
     mutate(across(c(week1_closer, clt_date, week1_open), ymd)) %>%
    group_by(facility) %>%
    summarise(Week_not_closed = sum(week1_closer >= Sys.Date()),
              Expected = sum(week1_closer < today()),
              Attended = sum(`Session 1 content` == "1"),
              percentage_attended = Attended / Expected * 100) %>% 
    adorn_totals("row", fill = "Total")
    

V_week2 <- retention %>%
    mutate(across(c(week2_closer, clt_date, week2_open), ymd)) %>%
    group_by(facility) %>%
    summarise(Week_not_closed = sum(week2_closer >= today()),
              Expected = sum(week2_closer < today()),
              Attended = sum(`Session 2 content` == "1"),
              percentage_attended = ifelse(Expected > 0, Attended / Expected * 100, NA))



V_week3 <- retention %>%
    mutate(across(c(week3_closer, clt_date, week3_open), ymd)) %>%
    group_by(facility) %>%
    summarise(Week_not_closed = sum(week3_closer > today()),
              Expected = sum(week3_closer <= today()),
              Attended = sum(`Session 3 content` == "1"),
              percentage_attended = Attended / Expected * 100)

V_week4 <- retention %>%
    mutate(across(c(week4_closer, clt_date, week4_open), ymd)) %>%
    group_by(facility) %>%
    summarise(Week_not_closed = sum(week4_closer >= today()),
              Expected = sum(week4_closer < today()),
              Attended = sum(`Session 4 content` == "1"),
              percentage_attended = Attended / Expected * 100)

V_week5 <- cv %>%
    mutate(across(c(week5_closer, clt_date, week5_open), ymd)) %>%
    group_by(pm_facility) %>%
    summarise(Week_not_closed = sum(week5_closer > today()),
              Expected = sum(week5_closer < today()),
              Attended = sum(`Session 5 content` == "1"),
              percentage_attended = Attended / Expected * 100)
