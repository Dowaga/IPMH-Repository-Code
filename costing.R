# Header ------------------------------------------------------------------

# Author(s): Yuwei
# Date: July 14, 2025
# This is a script that analyzes the costing data.

# Setup ------------------------------------------------------------------------
# Reference source codes & other dependencies:
source("dependencies.R")
source("DataTeam_ipmh.R")
source("REDCap_datapull.R")

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
    kable(caption = "Summary of Initial Screening and Enrollment (Intervention Sites)") 

## time table---------
screening_int_costing <- screening_int_costing %>%
    mutate(across(where(~ is.character(.) || is.factor(.)), 
                  ~ droplevels(as.factor(.))))

screening_int_costing <- screening_int_costing %>%
    mutate(
        triage_duration    = as.numeric(triage_time_out- triage_time_in)*1440,
        screen_duration    = as.numeric(screen_time_out- screen_time_in)*1440,
        score_duration     = as.numeric(score_time_out- score_time_in)*1440,
        info_duration      = as.numeric(info_time_out- info_time_in)*1440,
        phq9_duration      = as.numeric(phq9_time_out- phq9_time_in)*1440,
        pmass_duration     = as.numeric(pmass_time_out- pmass_time_in)*1440,
        pmsch_duration     = as.numeric(pmsch_time_out- pmsch_time_in)*1440,
        telesch_duration   = as.numeric(telesch_time_out- telesch_time_in)*1440,
        harm_duration      = as.numeric(harm_time_out- harm_time_in)*1440,
        eligi_duration     = as.numeric(eligi_time_out- eligi_time_in)*1440,
        baseline_duration  = as.numeric(base_time_out- base_time_in)*1440,
        total_duration     = as.numeric(visit_time_out- enter_time_in)*1440
    )

screening_int_table <- screening_int_costing %>%
    select(
        study_site,
        enter_desig,
        triage_desig, triage_duration,
        screen_desig, screen_duration,
        room_type, room_desig,
        phq2, gad2,
        score_duration, info_duration,
        phq9, phq9_duration,
        gad7, refer_service,
        pm_desig, pmass_duration, pmsch_duration,
        telesch_duration, harm_duration,
        eligi_duration, baseline_duration, total_duration
    ) %>%
    tbl_summary(
        by = study_site,
        statistic = list(
            all_continuous() ~ "{mean} ({sd})",
            all_categorical() ~ "{n} ({p}%)"
        ),
        type = list(
            enter_desig        = "categorical",
            triage_desig       = "categorical",
            screen_desig       = "categorical",
            room_type          = "categorical",
            room_desig         = "categorical",
            refer_service      = "categorical",
            phq2               = "continuous",
            gad2               = "continuous",
            phq9               = "continuous",
            gad7               = "continuous",
            triage_duration    = "continuous",
            screen_duration    = "continuous",
            score_duration     = "continuous",
            info_duration      = "continuous",
            phq9_duration      = "continuous",
            pmass_duration     = "continuous",
            pmsch_duration     = "continuous",
            telesch_duration   = "continuous",
            harm_duration      = "continuous",
            eligi_duration     = "continuous",
            baseline_duration  = "continuous",
            total_duration     = "continuous"
        ),
        label = list(
            enter_desig        = "Entry staff",
            triage_desig       = "Triage staff",
            triage_duration    = "Triage time (min)",
            screen_desig       = "Screening staff",
            screen_duration    = "Screening time (min)",
            room_type          = "Room type",
            room_desig         = "Room staff",
            phq2               = "PHQ-2 score",
            gad2               = "GAD-2 score",
            score_duration     = "Scoring time (min)",
            info_duration      = "Information & reassurance time (min)",
            phq9               = "PHQ-9 score",
            phq9_duration      = "PHQ-9 time (min)",
            gad7               = "GAD-7 score",
            refer_service      = "Referral service(s)",
            pm_desig           = "Pre-PM+ assessment staff",
            pmass_duration     = "PM+ assessment time (min)",
            pmsch_duration     = "PM+ scheduling time (min)",
            telesch_duration   = "Telepsychiatry time (min)",
            harm_duration      = "Self-harm assessment time (min)",
            eligi_duration     = "Eligibility assessment time (min)",
            baseline_duration  = "Baseline assessment time (min)",
            total_duration     = "Total visit time (min)"
        ),
        digits = all_continuous() ~ 1,
        missing = "no"
    ) %>%
    add_n() %>% add_overall() %>% 
    bold_labels() %>% 
    modify_caption("Summary of Time Used for Initial Screening and Enrollment (Intervention Sites)")

screening_int_table

# screening & enrollment - con ------------
screening_ctrl_costing <- costing %>%
    filter(redcap_event_name == "Event 1 (Arm 2: Control)" & 
               study_visit == "Initial screening and enrollment") %>%
    filter(!is.na(pt_id_con)) %>%
    select(date, study_site, c(96, 98:122), -visit_initial_con)

## data collection table ---------
# Total N as a tibble row
total_summary_con <- tibble(
    study_site = "Overall",
    N = nrow(screening_ctrl_costing)
)

# N per facility
facility_summary_con <- screening_ctrl_costing %>%
    group_by(study_site) %>%
    summarise(N = n(), .groups = "drop")

# Combine both into one table
summary_table_con <- bind_rows(total_summary_con, facility_summary_con)

# Print result
summary_table_con %>% 
    kable(caption = "Summary of Initial Screening and Enrollment (Control Sites)") 

## Time table ---------
screening_ctrl_costing <- screening_ctrl_costing %>%
    mutate(across(where(~ is.character(.) || is.factor(.)), 
                  ~ droplevels(as.factor(.))))

screening_ctrl_costing <- screening_ctrl_costing %>%
    mutate(
        triage_duration_con   = as.numeric(triage_time_out_con - triage_time_in_con)*1440,
        screen_duration_con   = as.numeric(screen_time_out_con - screen_time_in_con)*1440,
        score_duration_con    = as.numeric(score_time_out_con - score_time_in_con)*1440,
        clinic_duration_con   = as.numeric(clinic_time_out_con - clinic_time_in_con)*1440,
        refer_duration_con    = as.numeric(visit_time_out_con - refer_time_in_con)*1440,
        eligi_duration_con    = as.numeric(eligi_time_out_con - eligi_time_in_con)*1440,
        baseline_duration_con = as.numeric(base_time_out_con - base_time_in_con)*1440,
        total_duration_con    = as.numeric(visit_time_out_con - enter_time_in_con)*1440
    )

screening_ctrl_table <- screening_ctrl_costing %>%
    select(study_site,
        triage_desig_con, triage_duration_con,
        screen_desig_con, screen_duration_con,
        room_type_con, phq2_cont, gad2_cont,
        score_duration_con,
        clinic_duration_con,
        eli_yn_con,
        refer_desig, refer_duration_con,
        eligi_duration_con,
        baseline_duration_con,
        total_duration_con
    ) %>%
    tbl_summary(
        by= study_site,
        statistic = list(
            all_continuous() ~ "{mean} ({sd})",
            all_categorical() ~ "{n} ({p}%)"
        ),
        type = list(
            triage_desig_con     = "categorical",
            screen_desig_con     = "categorical",
            room_type_con        = "categorical",
            refer_desig          = "categorical",
            eli_yn_con           = "categorical",
            phq2_cont            = "continuous",
            gad2_cont            = "continuous",
            triage_duration_con  = "continuous",
            screen_duration_con  = "continuous",
            score_duration_con   = "continuous",
            clinic_duration_con  = "continuous",
            refer_duration_con   = "continuous",
            eligi_duration_con   = "continuous",
            baseline_duration_con= "continuous",
            total_duration_con   = "continuous"
        ),
        label = list(
            triage_desig_con      = "Triage staff",
            triage_duration_con   = "Triage time (min)",
            screen_desig_con      = "Screening staff",
            screen_duration_con   = "Screening time (min)",
            room_type_con         = "Room type",
            phq2_cont             = "PHQ-2 score",
            gad2_cont             = "GAD-2 score",
            score_duration_con    = "PHQ2/GAD2 scoring time (min)",
            clinic_duration_con   = "Clinic consultation time (min)",
            eli_yn_con            = "Eligibility",
            refer_desig           = "Referral staff",
            refer_duration_con    = "Referral time (min)",
            eligi_duration_con    = "Eligibility assessment time (min)",
            baseline_duration_con = "Baseline assessment time (min)",
            total_duration_con    = "Total screening & enrollment time (min)"
        ),
        digits = all_continuous() ~ 1,
        missing = "no"
    ) %>%
    add_n() %>% add_overall() %>% 
    bold_labels() %>% 
    modify_caption("Summary of Time Used for Initial Screening and Enrollment (Control Sites)")

screening_ctrl_table

# PM+ ---------------
pm_plus_costing <- costing %>%
    filter(redcap_event_name == "Event 1 (Arm 1: Intervention)" & 
               study_visit == "PM+") %>%
    filter(!is.na(pt_id_pm)) %>%
    select(date, study_site, pt_id_pm, starts_with("pm"), -pm_desig, -pmass_time_in,
           -pmass_time_out, -pmsch_time_in, -pmsch_time_out, -pm_int_complete)

## Data collection table --------
# Total N as a tibble row
total_summary_pm <- tibble(
    study_site = "Overall",
    N = nrow(pm_plus_costing)
)

# N per facility
facility_summary_pm <- pm_plus_costing %>%
    group_by(study_site) %>%
    summarise(N = n(), .groups = "drop")

# Combine both into one table
summary_table_pm <- bind_rows(total_summary_pm, facility_summary_pm)

# Print result
summary_table_pm %>% 
    kable(caption = "Summary of PM+ Sessions (Intervention Sites)")

## Time table ---------
pm_plus_costing <- pm_plus_costing %>%
    mutate(across(where(~ is.character(.) || is.factor(.)), 
                  ~ droplevels(as.factor(.))))

pm_plus_costing <- pm_plus_costing %>%
    mutate(
        pm1_duration = as.numeric(pm1_end_time_out - pm1_time_in)*1440,
        pm1_total_duration = as.numeric(pm1_end_time_out - pm_enter_time_in)*1440,
        
        pm2_duration = as.numeric(pm2_end_time_out - pm2_time_in)*1440,
        pm2_total_duration = as.numeric(pm2_end_time_out - pm_enter_time_in)*1440,
        
        pm3_duration = as.numeric(pm3_end_time_out - pm3_time_in)*1440,
        pm3_total_duration = as.numeric(pm3_end_time_out - pm_enter_time_in)*1440,
        
        pm4_duration = as.numeric(pm4_end_time_out - pm4_time_in)*1440,
        pm4_total_duration = as.numeric(pm4_end_time_out - pm_enter_time_in)*1440,
        
        pm5_duration = as.numeric(pm5_end_time_out - pm5_time_in)*1440,
        pm5_total_duration = as.numeric(pm5_end_time_out - pm_enter_time_in)*1440
    )

pm_summary_table <- pm_plus_costing %>%
    select(
        pm_number, study_site,
        pm1_desig, pm1_duration, pm1_total_duration,
        pm2_desig, pm2_duration, pm2_total_duration,
        pm3_desig, pm3_duration, pm3_total_duration,
        pm4_desig, pm4_duration, pm4_total_duration,
        pm5_desig, pm5_duration, pm5_total_duration
    ) %>%
    tbl_summary(
        by = study_site,
        statistic = list(
            all_continuous() ~ "{mean} ({sd})",
            all_categorical() ~ "{n} ({p}%)"
        ),
        type = list(
            pm1_desig = "categorical",
            pm2_desig = "categorical",
            pm3_desig = "categorical",
            pm4_desig = "categorical",
            pm5_desig = "categorical",
            pm1_duration = "continuous",
            pm2_duration = "continuous",
            pm3_duration = "continuous",
            pm4_duration = "continuous",
            pm5_duration = "continuous",
            pm1_total_duration = "continuous",
            pm2_total_duration = "continuous",
            pm3_total_duration = "continuous",
            pm4_total_duration = "continuous",
            pm5_total_duration = "continuous"
        ),
        label = list(
            pm1_desig = "PM+ Session 1 Staff",
            pm1_duration = "Session 1 Component Duration (min)",
            pm1_total_duration = "Session 1 Total Duration (min)",
            
            pm2_desig = "PM+ Session 2 Staff",
            pm2_duration = "Session 2 Component Duration (min)",
            pm2_total_duration = "Session 2 Total Duration (min)",
            
            pm3_desig = "PM+ Session 3 Staff",
            pm3_duration = "Session 3 Component Duration (min)",
            pm3_total_duration = "Session 3 Total Duration (min)",
            
            pm4_desig = "PM+ Session 4 Staff",
            pm4_duration = "Session 4 Component Duration (min)",
            pm4_total_duration = "Session 4 Total Duration (min)",
            
            pm5_desig = "PM+ Session 5 Staff",
            pm5_duration = "Session 5 Component Duration (min)",
            pm5_total_duration = "Session 5 Total Duration (min)"
        ),
        digits = all_continuous() ~ 1,
        missing = "no"
    ) %>%
    add_n() %>% add_overall() %>%
    bold_labels() %>% 
    modify_caption("Summary of PM+ Sessions (Intervention Sites)")

pm_summary_table

# telepsychiatry ------------------
telepsychiatry_costing <- costing %>%
    filter(redcap_event_name == "Event 1 (Arm 1: Intervention)" & 
               study_visit == "Telepsychiatry session") %>%
    filter(!is.na(pt_id_tele)) %>%
    select(date, study_site, starts_with("tele"), -telesch_time_in, -telesch_time_out,
           -tele_initial, -tele_end_initial, -telepsychiatry_int_complete)

## Data collection table --------
# Total N as a tibble row
total_summary_tele <- tibble(
    study_site = "Overall",
    N = nrow(telepsychiatry_costing)
)

# N per facility
facility_summary_tele <- telepsychiatry_costing %>%
    group_by(study_site) %>%
    summarise(N = n(), .groups = "drop")

# Combine both into one table
summary_table_tele <- bind_rows(total_summary_tele, facility_summary_tele)

# Print result
summary_table_tele %>% 
    kable(caption = "Summary of Telepsychiatry Sessions (Intervention Sites)") 

## Time table ---------
telepsychiatry_costing <- telepsychiatry_costing %>%
    mutate(across(where(~ is.character(.) || is.factor(.)), 
                  ~ droplevels(as.factor(.))))

# List of all telepsychiatry time columns
telepsychiatry_costing <- telepsychiatry_costing %>%
    mutate(
        tele_info_duration = as.numeric(tele_info_time_out - tele_info_time_in)*1440,
        tele_docu_duration = as.numeric(tele_docu_time_out - tele_docu_time_in)*1440,
        tele_total_duration = as.numeric(tele_end_time_out - tele_enter_time_in)*1440
    )

telepsychiatry_table <- telepsychiatry_costing %>%
    select(
        tele_number, tele_info_duration, tele_docu_duration, tele_total_duration
    ) %>%
    tbl_summary(
        statistic = list(
            all_continuous() ~ "{mean} ({sd})",
            all_categorical() ~ "{n} ({p}%)"
        ),
        type = list(
            tele_number = "categorical",
            tele_info_duration = "continuous",
            tele_docu_duration = "continuous",
            tele_total_duration = "continuous"
        ),
        label = list(
            tele_number = "Telepsychiatry Session Number",
            tele_info_duration = "Information & Reassurance Duration (min)",
            tele_docu_duration = "Documentation Duration (min)",
            tele_total_duration = "Total Telepsychiatry Duration (min)"
        ),
        digits = all_continuous() ~ 1,
        missing = "no"
    ) %>%
    add_n() %>%
    bold_labels()

telepsychiatry_table

#did not break down due to very small sample size.

# Audit and Feedback ------------------
audit_feedback_costing <- costing %>%
    filter(study_visit == "Audit and Feedback") 

