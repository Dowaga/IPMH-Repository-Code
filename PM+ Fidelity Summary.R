# Header ------------------------------------------------------------------

# Author(s): Owaga
# Date: March 11, 2025
# PM+ FIdelity Analysis

# Setup ------------------------------------------------------------------------
rm(list = ls())
# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")

 # PM+ and Telepsychiatry Referrals
pm_telep_df <- ppw_rct_df %>% 
    filter(redcap_event_name == "Enrollment (Arm 1: Intervention)") %>% 
    select(record_id, clt_study_site, clt_date, starts_with("abs_")) %>% 
    filter(!is.na(clt_date))

anx_depr_Qcs <- ppw_rct_df %>% 
    filter(redcap_event_name == "6 Months (Arm 1: Intervention)") %>% 
    filter(is.na(mv_visit)) %>% 
    select(record_id, clt_study_site, clt_date, starts_with("phq_"), starts_with("gad7_")) %>% 
    filter(!is.na(clt_date)) %>% 
    filter(clt_study_site == "09, Miriu Health Centre")


# PM+ Session 5 abstractions
pm_session5_df <- ppw_rct_df%>% 
    filter(redcap_event_name == "PM+ Session 5 Abstraction (Arm 1: Intervention)") %>% 
    select(record_id, clt_study_site, clt_date, starts_with("abs_"))

post_facility <- pm_session5_df %>% 
    filter(clt_study_site == "16, Usigu Health Centre")

# Define PHQ9 recoding
phq9_labels <- c(
    "not at all" = 0,
    "several days" = 1,
    "more than half the days" = 2,
    "nearly every day" = 3
)

gad7_labels <-c(
    "Not at all" = 0,
    "Several days" = 1,
    "Over half the days"= 2,
    "Nearly every day" = 3)


# Recode PHQ9 variables
pm_telep_df <- pm_telep_df %>%
    mutate(across(c(abs_phq_interest, abs_phq_down, abs_phq_sleep,
                    abs_phq_tired, abs_phq_appetite, abs_phq_bad,
                    abs_phq_concentrate, abs_phq_slow, abs_phq_dead), ~ recode(., !!!phq9_labels)),
           across(c(abs_gad7_nerve, abs_gad7_uncontrol, 
                    abs_gad7_worry, abs_gad7_relax, abs_gad7_restless,
                    abs_gad7_annoyed, abs_gad7_afraid), ~ recode(., !!!gad7_labels)))



pm_telep_df <- pm_telep_df %>% 
    mutate(phq9_scores = rowSums(select(., abs_phq_interest, abs_phq_down,
                                        abs_phq_sleep, abs_phq_tired, 
                                        abs_phq_appetite, abs_phq_bad,
                                        abs_phq_concentrate, abs_phq_slow, 
                                        abs_phq_dead), na.rm = TRUE),
           gad7_scores = rowSums(select(., abs_gad7_nerve, abs_gad7_uncontrol,
                                        abs_gad7_worry, abs_gad7_relax, 
                                        abs_gad7_restless, abs_gad7_annoyed,
                                        abs_gad7_afraid), na.rm = TRUE))

# Endorsed PHQ9 Question 9
self_harm <- pm_telep_df %>% 
    filter(abs_phq_dead > 0)

pm_telep_df <- pm_telep_df %>% 
    filter((phq9_scores >= 10)|(gad7_scores >= 10)|(abs_phq_dead == 1 & abs_phq_ref_tele == "Yes")) %>% 
    mutate(
        max_score = pmax(phq9_scores, gad7_scores, na.rm = TRUE),  # Get the greatest score
        eligible_for = case_when(
            abs_phq_dead > 0 ~ "Telepsychiatry",
            (max_score >= 10 & max_score < 15 &
                 (max_score == phq9_scores | max_score == gad7_scores)) ~ "PM+",
            max_score >= 15 ~ "Telepsychiatry",
            TRUE ~ "Not Eligible"
        ),
        referred_to = case_when(
            abs_gad7_ref_tele == "Yes" | abs_phq_ref_tele == "Yes" ~ "Telepsychiatry",
            abs_phq_ref_pm == "Yes" | abs_gad7_ref_pm == "Yes" ~ "PM+",
            TRUE ~ NA_character_
        )
    )

referral_QCs <- pm_telep_df %>% 
    filter(eligible_for == "PM+" & referred_to == "Telepsychiatry")

# PM+ participants
pm_plus_df <- pm_telep_df %>% 
    filter(max_score >= 10 & max_score < 15) %>%
    select(-max_score)  # Remove max_score if not needed

# PM+ Sessions survey
pm_follow_up <- pm_survey_df %>% 
    #filter(ipmh_participant == "Yes") %>%
    select(pm_ptid, pm_ancid, pm_facility, pm_date, pm_session,
           pm_pt_attend, ipmh_participant)



pm_follow_up_wide <- pm_follow_up %>% 
    #filter(ipmh_participant == "Yes") %>% 
    select(pm_ancid, pm_ptid, pm_facility, pm_session, pm_pt_attend) %>% 
    
    # Create standardized session names
    mutate(
        session_clean = case_when(
            str_detect(pm_session, regex("Pre-intervention", ignore_case = TRUE)) ~ "pre_psychlops",
            str_detect(pm_session, regex("Post-intervention", ignore_case = TRUE)) ~ "post_psychlops",
            str_detect(pm_session, regex("Session 1 content", ignore_case = TRUE)) ~ "session_1",
            str_detect(pm_session, regex("Session 2 content", ignore_case = TRUE)) ~ "session_2",
            str_detect(pm_session, regex("Session 3 content", ignore_case = TRUE)) ~ "session_3",
            str_detect(pm_session, regex("Session 4 content", ignore_case = TRUE)) ~ "session_4",
            str_detect(pm_session, regex("Session 5 content", ignore_case = TRUE)) ~ "session_5",
            TRUE ~ NA_character_
        ),
        attend_flag = if_else(pm_pt_attend == "Yes", 1L, 0L)
    ) %>%
    
    # 1?????? Identify participants who have duplicate session entries ??? restart = 1
    group_by(pm_ptid, session_clean) %>%
    mutate(dup_count = n()) %>%
    ungroup() %>%
    group_by(pm_ptid) %>%
    mutate(restart = if_else(any(dup_count > 1), 1L, 0L)) %>%
    ungroup() %>%
    
    # 2?????? Remove duplicate rows (keep only one per ptid + session)
    group_by(pm_ancid, pm_ptid, pm_facility, session_clean, restart) %>%
    summarise(attend_flag = max(attend_flag), .groups = "drop") %>%
    
    # 3?????? Pivot wider
    pivot_wider(
        names_from = session_clean,
        values_from = attend_flag,
        values_fill = 0L
    ) %>%
    select(pm_ancid, pm_ptid, pm_facility, session_1, session_2,
           session_3, session_4, session_5, pre_psychlops, post_psychlops, restart)

# Finished PM+
finished_pm <- pm_follow_up_wide %>% 
    filter(pm_facility == "Airport Health Centre (Kisumu)") %>% 
    filter(post_psychlops == 1)


pm_follow_up_summary <- pm_follow_up_wide %>% 
    mutate(
        # Initiated = attended session 1
        initiated = session_1,
        
        # Completed = attended session 5
        completed = session_5
    )%>%
    mutate(total_sessions_attended = rowSums(select(., session_1:session_5), na.rm = TRUE))


pm_fidelity_summary <- pm_follow_up_summary %>%
    summarise(
        total_participants = n(),# Initiation as n(%)
        initiated = paste0(sum(session_1), " (", round(sum(session_1)/n() * 100, 1), "%)"),
        
        # Completion as n(%)
        completed = paste0(sum(session_5), " (", round(sum(session_5)/n() * 100, 1), "%)"),
        
        # Average sessions attended
        avg_sessions_attended = round(mean(total_sessions_attended), 1)
    ) %>%
    # Optional: rename for presentation
    rename(
        "Referred Participants" = total_participants,
        "Initiated on PM+ n(%)" = initiated,
        "Completed PM n(%)" = completed,
        "Average Sessions Attended (out of 5)" = avg_sessions_attended
    )



# Create flextable
pm_fidelity_ft <- flextable(pm_fidelity_summary) %>%
    autofit() %>%
    align(align = "center", part = "all")

# Export to Word
output_file <- "C:/Users/hp/OneDrive/Desktop/IPMH/Fidelity Analysis/pm_fidelity_summary.docx"

doc <- read_docx() %>%
    body_add_par("PM+ Fidelity Summary", style = "heading 1") %>%
    body_add_flextable(pm_fidelity_ft)

print(doc, target = output_file)

# Multiple ANC Numbers
multiple_ancs <- pm_follow_up %>%
    filter(pm_facility == "Airport Health Centre (Kisumu)") %>% 
    group_by(pm_ptid) %>%
    summarise(
        ancids = paste(unique(pm_ancid), collapse = ", "),
        .groups = "drop"
    )

    
#----
# 1. Filter the records
problem_records <- pm_survey_df %>%
    distinct(pm_ptid, pm_ancid, .keep_all = TRUE) %>%
    add_count(pm_ptid, name = "n_ancids") %>%
    filter(n_ancids > 1) %>% 
    filter(pm_facility == "Miriu Health Centre") %>% 
    select(pm_ancid, pm_ptid, record_id)

# 2. Create a gt table
gt_tbl <- problem_records %>%
    gt() %>%
    tab_header(
        title = "Participants with Multiple ANC IDs",
        subtitle = "Ndiru Level 4 Hospital"
    )

# 3. Convert gt ??? flextable (needed for Word export)
ft_tbl <- flextable::regulartable(as.data.frame(problem_records))
ft_tbl <- flextable::autofit(ft_tbl)

# 4. Save to Word
doc <- read_docx() %>%
    body_add_par("Participants with Multiple ANC IDs", style = "heading 1") %>%
    body_add_flextable(ft_tbl)

print(doc, target = "Ndiru Level 4 Hospital.docx")


# Status changed
status_change <- pm_survey_df %>%
    distinct(pm_ancid, pm_ptid, pm_session, ipmh_participant, .keep_all = TRUE) %>%
    group_by(pm_ancid) %>%
    summarise(
        n_yes = sum(ipmh_participant == "Yes", na.rm = TRUE),
        n_no = sum(ipmh_participant == "No", na.rm = TRUE)
    ) %>%
    filter(n_yes > 0 & n_no > 0)

# PM+ Sessions - Subsequent Study Enrollees 
status_change_details <- pm_survey_df %>%
    semi_join(status_change, by = "pm_ancid") %>%
    arrange(pm_ptid, pm_session) %>%
    select(pm_ptid, pm_ancid, pm_session, ipmh_participant, everything())


pm_sessions <- pm_survey_df %>% 
    select(pm_ancid, pm_session, pm_facility, pm_ptid, ipmh_participant) %>% mutate(session_flag = 1) %>% 
    distinct() %>% # prevents duplicate 1s from repeated entries 
    pivot_wider( names_from = pm_session, values_from = session_flag, values_fill = 0 )

Qcs <- pm_survey_df %>%
    distinct(pm_ptid, pm_ancid, .keep_all = TRUE) %>%
    filter(pm_facility == "Ndiru Level 4 Hospital") %>% 
    add_count(pm_ptid, name = "n_ancids") %>%
    filter(n_ancids > 1)

duplicates <- pm_survey_df %>%
    distinct(pm_ptid, pm_ancid, .keep_all = TRUE) %>%
    add_count(pm_ptid, name = "n_ancids") %>%
    filter(n_ancids > 1)
    
    
    