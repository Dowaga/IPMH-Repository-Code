# Header ------------------------------------------------------------------

# Author(s): Yuwei
# Date: July 14, 2025
# This is a script that analyzes the costing data.

# Setup ------------------------------------------------------------------------
rm(list = ls())
# Reference source codes & other dependencies:
source("dependencies.R")
source("DataTeam_ipmh.R")
source("REDCap_datapull.R")

# load data
rm(list = setdiff(ls(), c("costing")))

visit_na <- costing %>% 
    filter(is.na(study_visit))

# screening & enrollment------------

## Intervention sites -----------
screening_int_costing <- costing %>% 
    filter(redcap_event_name == "Event 1 (Arm 1: Intervention)" & 
               study_visit == "Initial screening and enrollment") %>% 
    filter(!is.na(pt_id)) %>% 
    select(date, study_site, pt_id, enter_time_in,
           triage_desig, triage_time_in, triage_time_out,
           screen_desig, screen_time_in, screen_time_out,
           room_type, room_desig, phq2, gad2, low_score,score_time_in,
           score_time_out, preg, info_time_in, info_time_out,
           phq9, gad7, phq9_time_in, phq9_time_out, clinic_time_in_int, clinic_time_out_int,
           refer_service, refer_time_in, pm_desig, pmass_time_in,
           pmass_time_out,pmsch_time_in, pmsch_time_out, telesch_time_in, telesch_time_out,
           harm_time_in, harm_time_out, eli_yn, eligi_time_in,
           eligi_time_out, enrol_yn, base_time_in,
           base_time_out, visit_time_out)

# Checking those that are PMAD positive
pmad_positive <- screening_int_costing %>% 
    filter(low_score == 0)

supervision <- costing %>% 
    filter(pm_number == "PM+ Supervision")

facility_supervisions <- supervision %>% 
    group_by(study_site) %>% 
    summarise(`Total Supervisions` = n()) %>% 
    arrange(desc(`Total Supervisions`))

# Create flextable with heading
ft_supervision <- flextable(facility_supervisions) %>%
    add_header_lines(values = "PM+ Supervision Summary by Facility") %>%
    bg(i = ~ `Total Supervisions` < 2, j = "Total Supervisions", bg = "yellow") %>%
    color(i = ~ `Total Supervisions` < 2, j = "Total Supervisions", color = "black") %>%
    autofit()


activity_summary <- costing %>%
    filter(study_visit == "Audit and feedback") %>% 
    filter(activity_type %in% c("Audit and Feedback", "Health Talk")) %>%
    group_by(study_site, activity_type) %>%
    summarise(n = n(), .groups = "drop")


activity_summary <- activity_summary %>%
    pivot_wider(
        names_from = activity_type,
        values_from = n,
        values_fill = 0  # fills missing combinations with 0
    )%>% 
    arrange(desc(`Audit and Feedback`))

# Create flextable with conditional formatting
ft <- flextable(activity_summary) %>%
    set_header_labels(
        study_site = "Study Site",
        `Audit and Feedback` = "Audit & Feedback",
        `Health Talk` = "Health Talk"
    ) %>%
    add_header_lines(values = "Audit & Feedback and Health Talks") %>%
    color(i = ~ `Audit and Feedback` < 2, j = "Audit and Feedback", color = "black") %>%
    bg(i = ~ `Audit and Feedback` < 2, j = "Audit and Feedback", bg = "yellow") %>%
    color(i = ~ `Health Talk` < 2, j = "Health Talk", color = "black") %>%
    bg(i = ~ `Health Talk` < 2, j = "Health Talk", bg = "pink") %>%
    autofit()



## Control sites ---------------
screening_ctrl_costing <- costing %>%
    filter(redcap_event_name == "Event 1 (Arm 2: Control)" & 
               study_visit == "Initial screening and enrollment") %>%
    filter(!is.na(pt_id_con)) %>%
    select(date, study_site, c(98, 100:133), -visit_initial_con)

### data collection table ---------
total_summary <- tibble(
    study_site = "Overall",
    N = nrow(screening_int_costing)
)

facility_summary <- screening_int_costing %>%
    group_by(study_site) %>%
    summarise(N = n(), .groups = "drop")

summary_table <- bind_rows(total_summary, facility_summary)

summary_table <- summary_table %>%
    mutate(Group = "Intervention")

total_summary_con <- tibble(
    study_site = "Overall",
    N = nrow(screening_ctrl_costing)
)

facility_summary_con <- screening_ctrl_costing %>%
    group_by(study_site) %>%
    summarise(N = n(), .groups = "drop")

summary_table_con <- bind_rows(total_summary_con, facility_summary_con)

summary_table_con <- summary_table_con %>%
    mutate(Group = "Control")

combined_summary <- bind_rows(summary_table, summary_table_con)

combined_summary <- combined_summary %>%
    mutate(
        overall_flag = if_else(study_site == "Overall", 1, 0)
    ) %>%
    arrange(Group, desc(overall_flag), study_site) %>%
    select(Group, study_site, N)  # Reorder columns

table_combined_summary <- flextable(combined_summary) %>%
    set_header_labels(
        Group = "Arm",
        study_site = "Study Site",
        N = "Number of Participants"
    ) %>%
    autofit() %>% 
    set_caption("Number of Participants by Study Arm and Site for Initial Screening and Enrollment")

### time table for intervention sites ---------
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
        refer_start_time = coalesce(clinic_time_out_int, phq9_time_out, info_time_out),
        refer_duration   = as.numeric(eligi_time_in - refer_start_time) * 1440,
        clinic_duration    = as.numeric(clinic_time_out_int - clinic_time_in_int)*1440,
        pmass_duration     = as.numeric(pmass_time_out- pmass_time_in)*1440,
        pmsch_duration     = as.numeric(pmsch_time_out- pmsch_time_in)*1440,
        telesch_duration   = as.numeric(telesch_time_out- telesch_time_in)*1440,
        harm_duration      = as.numeric(harm_time_out- harm_time_in)*1440,
        eligi_duration     = as.numeric(eligi_time_out- eligi_time_in)*1440,
        baseline_duration  = as.numeric(base_time_out- base_time_in)*1440,
        total_duration     = as.numeric(visit_time_out- enter_time_in)*1440
    )

# examine if there are negative durations
# screening_int_costing %>%
#     filter(
#         triage_duration < 0 | screen_duration < 0 | score_duration < 0 |
#         info_duration < 0 | phq9_duration < 0 | clinic_duration < 0 |
#         pmass_duration < 0 | pmsch_duration < 0 | telesch_duration < 0 |
#         harm_duration < 0 | eligi_duration < 0 | baseline_duration < 0 |
#         total_duration < 0 | refer_duration < 0
#     )

# examine scored but no scoring time
no_scoring_time <- screening_int_costing %>% 
    filter(!is.na(phq2)) %>% 
    filter(is.na(info_time_out))

screening_int_costing <- screening_int_costing %>%
    mutate(
        refer_service = as.character(refer_service), 
        refer_service = recode(refer_service,
                               "Scheduling for PM+ by lay provider" = "PM+",
                               "Scheduling for telepsychiatry by lay provider" = "Telepsychiatry",
                               "Self-harm assessment by study staff" = "Self-harm Assessment"),
        refer_service = replace_na(refer_service, "Refer to study eligibility")
    )

screening_int_table <- screening_int_costing %>%
    select(
        study_site,
        triage_desig, triage_duration,
        screen_desig, screen_duration,
        room_type, room_desig,
        phq2, gad2,
        score_duration, info_duration,
        phq9, gad7, 
        phq9_duration, clinic_duration, 
        eli_yn,
        refer_service, refer_duration,
        pm_desig, pmass_duration, pmsch_duration,
        telesch_duration, harm_duration,
        eligi_duration, baseline_duration, total_duration
    ) %>%
    tbl_summary(
        by = study_site,
        statistic = list(
            all_continuous() ~ "{median} ({p25}, {p75})",
            all_categorical() ~ "{n} ({p}%)"
        ),
        type = list(
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
            total_duration     = "continuous",
            clinic_duration    = "continuous",
            refer_duration     = "continuous",
            eli_yn           = "categorical"
        ),
        label = list(
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
            clinic_duration    = "Clinic consultation time (min)",
            refer_service      = "Referral service(s)",
            pm_desig           = "Pre-PM+ assessment staff",
            pmass_duration     = "PM+ assessment time (min)",
            pmsch_duration     = "PM+ scheduling time (min)",
            telesch_duration   = "Telepsychiatry time (min)",
            harm_duration      = "Self-harm assessment time (min)",
            eligi_duration     = "Eligibility assessment time (min)",
            baseline_duration  = "Baseline assessment time (min)",
            total_duration     = "Total visit time (min)",
            refer_duration     = "Waiting time for study enrollment (min)",
            eli_yn           = "Eligibility"
        ),
        digits = all_continuous() ~ 1,
        missing = "no"
    ) %>%
    add_n() %>% 
    add_overall() %>% 
    bold_labels()%>% 
    # convert from gtsummary object to gt object
    as_gt() %>%
    # modify with gt functions
    gt::tab_header("Summary of Time Used for Initial Screening and Enrollment (Intervention Sites)") %>% 
    gt::tab_options(
        table.font.size = "medium",
        data_row.padding = gt::px(1)) %>%
    tab_options(
        table.font.size = px(14))

table_screening_int <- screening_int_table%>%
    # add a footnote
    gt::tab_footnote(
        footnote = "Note: Waiting time for study eligibility includes time spent in service referral."
    )

### By refer_service ---------
screening_int_table_by_refer <- screening_int_costing %>%
    select(
        triage_desig, triage_duration,
        screen_desig, screen_duration,
        room_type, room_desig,
        phq2, gad2,
        score_duration, info_duration,
        phq9, gad7, 
        phq9_duration, clinic_duration, refer_service,refer_duration,
        pm_desig, pmass_duration, pmsch_duration,
        telesch_duration, harm_duration,
        eligi_duration, baseline_duration, total_duration
    ) %>%
    tbl_summary(
        by = refer_service,
        statistic = list(
            all_continuous() ~ "{median} ({p25}, {p75})",
            all_categorical() ~ "{n} ({p}%)"
        ),
        type = list(
            triage_desig       = "categorical",
            screen_desig       = "categorical",
            room_type          = "categorical",
            room_desig         = "categorical",
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
            total_duration     = "continuous",
            clinic_duration    = "continuous",
            refer_duration     = "continuous"
        ),
        label = list(
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
            clinic_duration    = "Clinic consultation time (min)",
            pm_desig           = "Pre-PM+ assessment staff",
            pmass_duration     = "PM+ assessment time (min)",
            pmsch_duration     = "PM+ scheduling time (min)",
            telesch_duration   = "Telepsychiatry time (min)",
            harm_duration      = "Self-harm assessment time (min)",
            eligi_duration     = "Eligibility assessment time (min)",
            baseline_duration  = "Baseline assessment time (min)",
            total_duration     = "Total visit time (min)",
            refer_duration     = "Waiting time for study enrollment (min)"
        ),
        digits = all_continuous() ~ 1,
        missing = "no"
    ) %>%
    add_n() %>% add_overall() %>% add_p() %>% 
    bold_labels() %>% 
    # convert from gtsummary object to gt object
    as_gt() %>%
    # modify with gt functions
    gt::tab_header("Summary of Time Used for Initial Screening and Enrollment (Intervention Sites) By Referral Services") %>% 
    gt::tab_options(
        table.font.size = "medium",
        data_row.padding = gt::px(1)) %>%
    tab_options(
        table.font.size = px(14))

table_screening_int_by_refer <- screening_int_table_by_refer %>% 
    # add a footnote
    gt::tab_footnote(
        footnote = "Note: Waiting time for study eligibility includes time spent in service referral."
    )


### Time table for control sites ---------
screening_ctrl_costing <- screening_ctrl_costing %>%
    mutate(across(where(~ is.character(.) || is.factor(.)), 
                  ~ droplevels(as.factor(.))))

screening_ctrl_costing <- screening_ctrl_costing %>%
    mutate(
        triage_duration_con   = as.numeric(triage_time_out_con - triage_time_in_con)*1440,
        screen_duration_con   = as.numeric(screen_time_out_con - screen_time_in_con)*1440,
        score_duration_con    = as.numeric(score_time_out_con - score_time_in_con)*1440,
        clinic_duration_con   = as.numeric(clinic_time_out_con - clinic_time_in_con)*1440,
        refer_duration_con    = as.numeric(eligi_time_in_con - refer_time_in_con)*1440,
        eligi_duration_con    = as.numeric(eligi_time_out_con - eligi_time_in_con)*1440,
        baseline_duration_con = as.numeric(base_time_out_con - base_time_in_con)*1440,
        total_duration_con    = as.numeric(visit_time_out_con - enter_time_in_con)*1440
    )

# examine if there are negative durations
# screening_ctrl_costing %>%
#     filter(
#         triage_duration_con < 0 | screen_duration_con < 0 | score_duration_con < 0 |
#         clinic_duration_con < 0 | refer_duration_con < 0 | eligi_duration_con < 0 |
#         baseline_duration_con < 0 | total_duration_con < 0
#     )

screening_ctrl_table <- screening_ctrl_costing %>%
    select(study_site,triage_desig_con, triage_duration_con,
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
            all_continuous() ~ "{median} ({p25}, {p75})",
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
            refer_desig           = "Study referral staff",
            refer_duration_con    = "Waiting time for study enrollment (min)",
            eligi_duration_con    = "Eligibility assessment time (min)",
            baseline_duration_con = "Baseline assessment time (min)",
            total_duration_con    = "Total screening & enrollment time (min)"
        ),
        digits = all_continuous() ~ 1,
        missing = "no"
    ) %>%
    add_n() %>% 
    add_overall() %>% 
    bold_labels() 

table_screening_ctrl <- screening_ctrl_table %>%
    # convert from gtsummary object to gt object
    as_gt() %>%
    # modify with gt functions
    gt::tab_header("Summary of Time Used for Initial Screening and Enrollment (Control Sites)") %>% 
    gt::tab_options(
        table.font.size = "medium",
        data_row.padding = gt::px(1)) %>%
    tab_options(
        table.font.size = px(14))


## Compare intervention and control sites ---------
# Add 'arm' label
screening_int_costing <- screening_int_costing %>%
    mutate(arm = "Intervention") 

screening_ctrl_costing <- screening_ctrl_costing %>%
    mutate(arm = "Control") %>%
    rename(
        triage_desig = triage_desig_con,
        triage_duration = triage_duration_con,
        screen_desig = screen_desig_con,
        screen_duration = screen_duration_con,
        room_type = room_type_con,
        phq2 = phq2_cont,
        gad2 = gad2_cont,
        score_duration = score_duration_con,
        clinic_duration = clinic_duration_con,
        refer_duration = refer_duration_con,
        eligi_duration = eligi_duration_con,
        baseline_duration = baseline_duration_con,
        total_duration = total_duration_con
    )

common_vars <- c(
    "arm", "study_site",
    "triage_desig", "triage_duration",
    "screen_desig", "screen_duration",
    "room_type",
    "phq2", "gad2", "score_duration",
    "clinic_duration",
    "refer_duration",
    "eligi_duration", "baseline_duration",
    "total_duration"
)

int_data <- screening_int_costing %>% select(any_of(common_vars))
ctrl_data <- screening_ctrl_costing %>% select(any_of(common_vars))
combined_data <- bind_rows(int_data, ctrl_data)

comparison_table <- combined_data %>%
    select (
        arm, 
        triage_desig, triage_duration,
        screen_desig, screen_duration,
        room_type,
        phq2, gad2, score_duration,
        clinic_duration,
        refer_duration,
        eligi_duration, baseline_duration,
        total_duration
    ) %>% 
    tbl_summary(
        by = arm,
        statistic = list(
            all_continuous() ~ "{median} ({p25}, {p75})",
            all_categorical() ~ "{n} ({p}%)"
        ),
        label = list(
            triage_desig       = "Triage staff",
            triage_duration    = "Triage time (min)",
            screen_desig       = "Screening staff",
            screen_duration    = "Screening time (min)",
            room_type          = "Room type",
            phq2               = "PHQ-2 score",
            gad2               = "GAD-2 score",
            score_duration     = "Scoring time (min)",
            clinic_duration    = "Clinic time (min)",
            refer_duration     = "Waiting time for study eligibility (min)",
            eligi_duration     = "Eligibility time (min)",
            baseline_duration  = "Baseline time (min)",
            total_duration     = "Total visit time (min)"
        ),
        type = list(
            triage_desig       = "categorical",
            screen_desig       = "categorical",
            room_type          = "categorical",
            phq2               = "continuous",
            gad2               = "continuous",
            triage_duration    = "continuous",
            screen_duration    = "continuous",
            score_duration     = "continuous",
            clinic_duration    = "continuous",
            refer_duration     = "continuous",
            eligi_duration     = "continuous",
            baseline_duration  = "continuous",
            total_duration     = "continuous"
        ),
        digits = all_continuous() ~ 1,
        missing = "no"
    ) %>%
    add_n() %>%
    add_overall() %>%
    add_p() %>%
    bold_labels() %>%
    # convert from gtsummary object to gt object
    as_gt() %>%
    # modify with gt functions
    gt::tab_header("Comparison of Screening & Enrollment by Study Site (All Arms)") %>% 
    gt::tab_options(
        table.font.size = "medium",
        data_row.padding = gt::px(1)) %>%
    tab_options(
        table.font.size = px(14))

table_screening_comparison <- comparison_table %>%
    # add a footnote
    gt::tab_footnote(
        footnote = "Note: Waiting time for study eligibility includes time spent in service referral among intervention sites."
    )

# PM+ ---------------
pm_plus_costing <- costing %>%
    filter(redcap_event_name == "Event 1 (Arm 1: Intervention)" & 
               study_visit == "PM+") %>%
    filter(!is.na(pt_id_pm)) %>%
    select(date, study_site, pt_id_pm, starts_with("pm"),starts_with("super"), -pm_desig, -pmass_time_in,
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
table_pm_summary <- flextable(summary_table_pm) %>%
    autofit() %>% 
    set_caption("Number of Participants for PM+")

#PM+ per facility by session
pm_session <- pm_plus_costing %>%
    filter(!pm_number %in% "PM+ Supervision") %>% 
    select(study_site, pm_number)

session_table <- pm_session %>%
    group_by(study_site, pm_number) %>%
    summarise(n = n(), .groups = "drop") %>%
    pivot_wider(
        names_from = pm_number,
        values_from = n,
        values_fill = 0
    )

session_table <- session_table %>%
    bind_rows(
        session_table %>%
            summarise(
                study_site = "TOTAL",
                across(-study_site, sum)
            )
    )


# --- Make GT table ---
gt_session_table <- session_table %>%
    gt() %>%
    
    # ---- BOLD the TOTAL row ----
tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
        rows = study_site == "TOTAL"
    )
) %>%
    
    # ---- Add heading ----
tab_header(
    title = md("*PM+ Sessions Completed by Facility*")
) %>%
    
    # Format numbers
    fmt_number(
        columns = where(is.numeric),
        decimals = 0
    ) %>%
    
    tab_options(
        table.font.size = px(12),
        data_row.padding = px(3)
    )


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
        pm5_total_duration = as.numeric(pm5_end_time_out - pm_enter_time_in)*1440,
        
        superv_duration = as.numeric(supervision_time_out - supervision_time_in)*1440
    )

pm_summary_table <- pm_plus_costing %>%
    select(
        pm_number, study_site,
        pm1_desig, pm1_duration, pm1_total_duration,
        pm2_desig, pm2_duration, pm2_total_duration,
        pm3_desig, pm3_duration, pm3_total_duration,
        pm4_desig, pm4_duration, pm4_total_duration,
        pm5_desig, pm5_duration, pm5_total_duration,
        superv_duration
    ) %>%
    tbl_summary(
        by = study_site,
        statistic = list(
            all_continuous() ~ "{median} ({p25}, {p75})",
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
            pm5_total_duration = "continuous",
            superv_duration = "continuous"
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
            pm5_total_duration = "Session 5 Total Duration (min)",
            superv_duration = "PM+ Supervision Duration (min)"
        ),
        digits = all_continuous() ~ 1,
        missing = "no"
    ) %>%
    add_n() %>% 
    add_overall() %>%
    bold_labels() %>% 
    # convert from gtsummary object to gt object
    as_gt() %>%
    # modify with gt functions
    gt::tab_header("Summary of PM+ Sessions (Intervention Sites)") %>% 
    gt::tab_options(
        table.font.size = "medium",
        data_row.padding = gt::px(1)) %>%
    tab_options(
        table.font.size = px(14))


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
table_telepsychiatry_summary <- flextable(summary_table_tele) %>%
    autofit() %>% 
    set_caption("Number of Participants for Telepsychiatry Sessions") 

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


table_telepsychiatry_costing <- telepsychiatry_table %>%
    as_gt() %>%
    # modify with gt functions
    gt::tab_header("Summary of Telepsychiatry Sessions (Intervention Sites)") %>% 
    gt::tab_options(
        table.font.size = "medium",
        data_row.padding = gt::px(1)) %>%
    tab_options(
        table.font.size = px(12))

# Audit and Feedback ------------------
audit_feedback_costing <- costing %>%
    filter(study_visit == "Audit and feedback") 

audit_feedback_costing <- audit_feedback_costing %>%
    mutate(arm = if_else(
        redcap_event_name == "Event 1 (Arm 1: Intervention)", "Intervention", "Control"
    )) %>%
    select(date, study_site, arm, part_num, record_id,
           activity_type, starts_with("af_"), starts_with("flipbook_"))

#Audit and Feedback Visit type Query
audit_feedback_QCs <- audit_feedback_costing %>% 
    filter(is.na(activity_type))

## Data collection table for audit and feedback ----------
total_summary_af <- audit_feedback_costing %>%
    group_by(arm) %>%
    summarise(study_site = "Overall", N = n(), .groups = "drop")

facility_summary_af <- audit_feedback_costing %>%
    group_by(arm, study_site) %>%
    summarise(N = n(), .groups = "drop")

summary_table_af <- bind_rows(total_summary_af, facility_summary_af) %>%
    arrange(arm, desc(study_site == "Overall"), study_site)
    
table_audit_feedback_summary<- flextable(summary_table_af) %>%
    autofit() %>%
    set_caption("Number of Participants in Audit and Feedback by Arm and Site")

## Time table for audit and feedback ----------
audit_feedback_costing <- audit_feedback_costing %>%
    mutate(across(where(~ is.character(.) || is.factor(.)), 
                  ~ droplevels(as.factor(.))))

audit_feedback_costing <- audit_feedback_costing %>%
    mutate(
        af_analysis_duration = difftime(af_analysis_time_out, af_analysis_time_in, units = "mins") %>% as.numeric(),
        af_develop_duration = difftime(af_develop_time_out, af_develop_time_in, units = "mins") %>% 
            as.numeric(),
        af_schedule_duration = as.numeric(difftime(af_sche_time_out, af_sche_time_in, units = "mins")),
        af_hurdle_duration = as.numeric(af_hurdle_time_out - af_hurdle_time_in)*1440,
        af_identify_duration = as.numeric(af_identify_time_out - af_identify_time_in)*1440,
        af_refresh_duration = difftime(af_refresh_time_out, af_refresh_time_in, units = "mins") %>% as.numeric(),
        af_map_duration = difftime(af_map_time_out, af_map_time_in, units = "mins") %>% as.numeric(),
        af_map2_duration = as.numeric(af_map2_time_out - af_map2_time_in)*1440,
        af_implement_duration = as.numeric(af_imp_time_out - af_imp_time_in)*1440,
        af_feedback_duration = difftime(af_feedback_time_out, af_feedback_time_in, units = "mins") %>% as.numeric(),
        af_session_duration = rowSums(across(c(
            af_hurdle_duration, af_refresh_duration, 
            af_map_duration, af_map2_duration, 
            af_implement_duration
        )), na.rm = TRUE),
        af_total_duration = rowSums(across(c(
            af_analysis_duration, af_develop_duration, 
            af_schedule_duration, af_hurdle_duration, 
            af_identify_duration, af_refresh_duration, 
            af_map_duration, af_map2_duration, 
            af_implement_duration, af_feedback_duration
        )), na.rm = TRUE),
        health_talk_duration = difftime(flipbook_time_out, flipbook_time_in, units = "mins") %>% as.numeric()
    )

# Negative time duration
negive <- audit_feedback_costing %>% 
    filter(af_total_duration < 0)

# Summarize designation choices per facility for selected activities
designation_summary <- audit_feedback_costing %>%
    group_by(study_site) %>%
    summarise(
        `Report generation` = paste(unique(af_analysis_desig), collapse = ", "),
        `Presentation Development Staff` = paste(unique(af_develop_desig), collapse = ", "),
        `Map Updating Staff` = paste(unique(af_map2_desig), collapse = ", "),
        `Map Revisit Staff` = paste(unique(af_map_desig), collapse = ", ")
        )


audit_feedback_table <- audit_feedback_costing %>%
    select(arm, part_num, 
        af_analysis_desig, af_analysis_duration, 
        af_develop_desig, af_develop_duration,
        af_sche_desig, af_schedule_duration, 
        af_hurdle_desig, af_hurdle_duration,
        #af_identify_desig,
        #af_identify_duration, 
        #af_refresh_desig,
        #af_refresh_duration, 
        af_map_desig,
        af_map_duration, 
        af_map2_desig, af_map2_duration,
        af_imp_desig,
        af_implement_duration, 
        af_feedback_desig,
        af_feedback_duration,
        af_session_duration, 
        af_total_duration,
        health_talk_duration
    ) %>%
    tbl_summary(
        by = arm,
        statistic = list(
            all_continuous() ~ "{median} ({p25}, {p75})",
            all_categorical() ~ "{n} ({p}%)"
        ),
        type = list(
            study_site = "categorical",
            af_analysis_duration = "continuous",
            af_develop_duration = "continuous",
            af_schedule_duration = "continuous",
            af_hurdle_duration = "continuous",
            af_identify_duration = "continuous",
            af_refresh_duration = "continuous",
            af_map_duration = "continuous",
            af_map2_duration = "continuous",
            af_implement_duration = "continuous",
            af_feedback_duration = "continuous",
            af_session_duration = "continuous",
            af_total_duration = "continuous",
            health_talk_duration = "continuous"
        ),
        label = list(
            part_num = "Participant Number",
            af_analysis_desig = "Report Generation Staff",
            af_analysis_duration = "Report Generation Duration (min)",
            af_develop_desig = "Presentation Development Staff",
            af_develop_duration = "Presentation Development Duration (min)",
            af_sche_desig = "Scheduling Staff",
            af_schedule_duration = "Scheduling Duration (min)",
            af_hurdle_desig = "Discussion Leading Staff",
            af_hurdle_duration = "Discussion Leading Duration (min)",
            #af_identify_desig = "Refresher Identification Staff",
            #af_identify_duration = "Refresher Identification Duration (min)",
            #af_refresh_desig = "Refresher Development Staff",
            #af_refresh_duration = "Refresh Development Duration (min)",
            af_map_desig = "Map Revisit Staff",
            af_map_duration = "Map Revisit Duration (min)",
            af_map2_desig = "Map Updating Staff",
            af_map2_duration = "Map Updating Duration (min)",
            af_imp_desig = "Implementation Strategy Reinforcement Staff",
            af_implement_duration = "Implementation Strategy Reinforcement Duration (min)",
            af_feedback_desig = "Feedback Relay Staff",
            af_feedback_duration = "Feedback Relay Duration (min)",
            af_session_duration = "Session Total Duration (min)",
            af_total_duration = "Total Audit & Feedback Duration (min)",
            health_talk_duration = "Health Talk Duration (min)"
        ),
        digits = all_continuous() ~ 1,
        missing = "no"
    ) %>%
    add_n() %>% 
    add_overall() %>% 
    add_p() %>%
    bold_labels() %>% 
    # convert from gtsummary object to gt object
    as_gt() %>%
    # modify with gt functions
    gt::tab_header("Comparison of Audit and Feedback Time between arms") %>% 
    gt::tab_options(
        table.font.size = "medium",
        data_row.padding = gt::px(1)) %>%
    tab_options(
        table.font.size = px(14))

table_audit_feedback_costing <- audit_feedback_table%>%
    # add a footnote
    gt::tab_footnote(
        footnote = "Note: Total Audit & Feedback Duration includes all components from analysis to feedback."
    )


#----
# Change to flextables
ft_designation  <- flextable(designation_summary)

# Create Word doc and add both
doc <- read_docx()%>%
    body_add_par("PM+ Supervision Per Facility", style = "heading 1") %>%
    body_add_flextable(ft_supervision) %>%
    body_add_par("Audit and Feedback Designation", style = "heading 1") %>%
    body_add_flextable(ft_designation) %>%
    body_add_par("") %>%  # Spacer
        body_add_flextable(ft)%>%
    body_add_par("") 

# Save Word file
print(doc, target = paste0("Time and Motion QCs", 
                           format(Sys.time(), 
                                  "%Y-%m-%d_%H%M%S"), ".docx"))

