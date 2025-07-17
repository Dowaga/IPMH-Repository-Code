# Header ------------------------------------------------------------------

# Author(s): Yuwei
# Date: July 14, 2025
# This is a script that analyzes the costing data.

# Setup ------------------------------------------------------------------------
# Reference source codes & other dependencies:
source("dependencies.R")
source("DataTeam_ipmh.R")
source("REDCap_datapull.R")
library(hms)

# load data
rm(list = setdiff(ls(), c("costing")))

# screening & enrollment - int ------------
screening_int_costing <- costing %>% 
    filter(redcap_event_name == "Event 1 (Arm 1: Intervention)" & 
               study_visit == "Initial screening and enrollment") %>% 
    filter(!is.na(pt_id)) %>% 
    select(date, study_site, pt_id, enter_desig, enter_time_in,
           triage_desig, triage_time_in, triage_time_out,
           screen_desig, screen_time_in, screen_time_out,
           room_type, room_desig, phq2, gad2, score_time_in,
           score_time_out, preg, info_time_in, info_time_out,
           phq9, gad7, phq9_time_in, phq9_time_out,
           refer_service, refer_time_in, pm_desig, pmass_time_in,
           pmass_time_out,pmsch_time_in, pmsch_time_out, telesch_time_in, telesch_time_out,
           harm_time_in, harm_time_out, eli_yn, eligi_time_in,
           eligi_time_out, enrol_yn, base_time_in,
           base_time_out, visit_time_out)

## data collection table --------
# Total N as a tibble row
total_summary <- tibble(
    study_site = "Overall",
    N = nrow(screening_int_costing)
)

# N per facility
facility_summary <- screening_int_costing %>%
    group_by(study_site) %>%
    summarise(N = n(), .groups = "drop")

# Combine both into one table
summary_table <- bind_rows(total_summary, facility_summary)

# Print result
summary_table %>% 
    kable(caption = "Summary of Initial Screening and Enrollment") 

screening_int_costing <- screening_int_costing %>%
    mutate(across(where(~ is.character(.) || is.factor(.)), 
                  ~ droplevels(as.factor(.))))

## step1: triage to information & reassurance ------------
screening_int_costing <- screening_int_costing %>%
    mutate(across(
        c(triage_time_in, triage_time_out,
          screen_time_in, screen_time_out,
          score_time_in, score_time_out,
          info_time_in, info_time_out),
        ~ hms::as_hms(as.character(.))
    ))

screening_int_costing <- screening_int_costing %>%
    mutate(
        triage_duration = as.numeric(triage_time_out - triage_time_in, units = "mins"),
        screen_duration = as.numeric(screen_time_out - screen_time_in, units = "mins"),
        score_duration  = as.numeric(score_time_out - score_time_in, units = "mins"),
        info_duration   = as.numeric(info_time_out - info_time_in, units = "mins")
    )

step1_table <- screening_int_costing %>%
    select(#study_site, 
           enter_desig,
           triage_desig, triage_duration,
           screen_desig, screen_duration,
           room_type, room_desig, phq2, gad2,
           score_duration, info_duration) %>%
    tbl_summary(
       # by = study_site,
        statistic = list(
            all_continuous() ~ "{mean} ({sd})",
            all_categorical() ~ "{n} ({p}%)"
        ),
        type = list(
            enter_desig = "categorical",
            triage_desig = "categorical",
            screen_desig = "categorical",
            room_type = "categorical",
            room_desig = "categorical",
            phq2 = "continuous",
            gad2 = "continuous",
            score_duration = "continuous",
            info_duration = "continuous",
            triage_duration = "continuous",
            screen_duration = "continuous"
        ),
        label = list(
            enter_desig      = "Entry staff",
            triage_desig     = "Triage staff",
            triage_duration  = "Triage time (min)",
            screen_desig     = "Screening staff",
            screen_duration  = "Screening time (min)",
            room_type        = "Room type",
            room_desig       = "Room staff",
            phq2             = "PHQ-2 score",
            gad2             = "GAD-2 score",
            score_duration   = "Scoring time (min)",
            info_duration    = "Information & reassurance time (min)"
        ),
        digits = all_continuous() ~ 1,
        missing = "no"
    ) %>%
  #  add_overall() %>%
    bold_labels()

## step2: advanced screening ------------
screening_int_costing <- screening_int_costing %>%
    mutate(across(
        c(phq9_time_in, phq9_time_out),
        ~ hms::as_hms(as.character(.))
    ))

screening_int_costing <- screening_int_costing %>%
    mutate(
        phq9_duration = as.numeric(phq9_time_out - phq9_time_in, units = "mins")
    )

step2_table <- screening_int_costing %>%
    select(#study_site,
           phq9, gad7, phq9_duration, refer_service) %>%
    tbl_summary(
       # by = study_site,
        statistic = list(
            all_continuous() ~ "{mean} ({sd})",
            all_categorical() ~ "{n} ({p}%)"
        ),
        type = list(
            phq9 = "continuous",
            phq9_duration = "continuous",
            gad7 = "continuous",
            refer_service = "categorical"
        ),
        label = list(
            phq9             = "PHQ-9 score",
            phq9_duration    = "PHQ-9 time (min)",
            gad7             = "GAD-7 score",
            refer_service    = "Referral service(s)"),
        digits = all_continuous() ~ 1,
        missing = "no"
    ) %>%
  #  add_overall() %>%
    bold_labels()

## Step 3: PM+ and Telepsychiatry ------------
screening_int_costing <- screening_int_costing %>%
    mutate(across(
        c(pmass_time_in, pmass_time_out,
          pmsch_time_in, pmsch_time_out,
          #telesch_time_in, telesch_time_out
          ),
        ~ hms::as_hms(as.character(.))
    ))

screening_int_costing <- screening_int_costing %>%
    mutate(
        pmass_duration = as.numeric(pmass_time_out - pmass_time_in, units = "mins"),
        pmsch_duration = as.numeric(pmsch_time_out - pmsch_time_in, units = "mins"),
        #telesch_duration = as.numeric(telesch_time_out - telesch_time_in, units = "mins")
    )



## Step 4: Harm, Eligibility and Enrollment ------------

# screening & enrollment - con ------------
screening_ctrl_costing <- costing %>%
    filter(redcap_event_name == "Event 1 (Arm 2: Control)" & 
               study_visit == "Initial screening and enrollment") %>%
    filter(!is.na(pt_id_con)) %>%
    select(date, study_site, c(96, 98:122), -visit_initial_con)

# PM+ ---------------
pm_plus_costing <- costing %>%
    filter(redcap_event_name == "Event 1 (Arm 1: Intervention)" & 
               study_visit == "PM+") %>%
    filter(!is.na(pt_id_pm)) %>%
    select(date, study_site, starts_with("pm"), -pm_desig, -pmass_time_in,
           -pmass_time_out, -pmsch_time_in, -pmsch_time_out, -pm_int_complete)


# telepsychiatry ------------------
telepsychiatry_costing <- costing %>%
    filter(redcap_event_name == "Event 1 (Arm 1: Intervention)" & 
               study_visit == "Telepsychiatry session") %>%
    filter(!is.na(pt_id_tele)) %>%
    select(date, study_site, starts_with("tele"), -telesch_time_in, -telesch_time_out,
           -tele_initial, -tele_end_initial, -telepsychiatry_int_complete)

# Audit and Feedback ------------------
audit_feedback_costing <- costing %>%
    filter(study_visit == "Audit and Feedback") 

