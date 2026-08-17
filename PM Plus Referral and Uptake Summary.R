# Header ------------------------------------------------------------------

# Author(s): Dowaga
# Date: July 16, 2026
# This is a script that explores PM+ referrals, uptake 
# and telepsychiatry services among those who have completed all PM+ sessions.

# Setup ------------------------------------------------------------------------
rm(list = ls())
# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")


#data prep ----------------
#only keep the databases we need
rm(list = setdiff(ls(), c("telepsych", "pm_survey_df", 
                          "ppw_rct_df", "rct_ppw_consenting")))



###############################################################
# 1. PREPARE REFERRAL DATA
###############################################################

treatment_referrals <- ppw_rct_df %>%
    select(record_id, clt_study_site, clt_date, redcap_event_name,
           starts_with("abs_")) %>%
    filter(!is.na(clt_date)) %>%
    mutate(visit_type = case_when(
        grepl("Enrollment", redcap_event_name) ~ "Enrollment",
        grepl("6 Weeks", redcap_event_name) ~ "Week 6",
        grepl("14 Weeks", redcap_event_name) ~ "Week 14",
        grepl("6 Months", redcap_event_name) ~ "Month 6",
        TRUE ~ NA_character_)
    )

###############################################################
# 2. CALCULATE PHQ9 & GAD7 SCORES
###############################################################
## Define PHQ9 recoding
phq9_labels <- c(
    "not at all" = 0,
    "several days" = 1,
    "more than half the days" = 2,
    "nearly every day" = 3
)

## Define GAD7 recoding
gad7_labels <-c(
    "Not at all" = 0,
    "Several days" = 1,
    "Over half the days"= 2,
    "Nearly every day" = 3)


treatment_referrals <- treatment_referrals %>%
    mutate(
        across(c(abs_phq_interest, abs_phq_down, abs_phq_sleep,
                 abs_phq_tired, abs_phq_appetite, abs_phq_bad,
                 abs_phq_concentrate, abs_phq_slow, abs_phq_dead),
               ~ recode(., !!!phq9_labels)),
        across(c(abs_gad7_nerve, abs_gad7_uncontrol, abs_gad7_worry,
                 abs_gad7_relax, abs_gad7_restless, abs_gad7_annoyed,
                 abs_gad7_afraid),
               ~ recode(., !!!gad7_labels))
    ) %>%
    mutate(
        phq9_score = rowSums(select(., abs_phq_interest, abs_phq_down,
                                    abs_phq_sleep, abs_phq_tired, abs_phq_appetite,
                                    abs_phq_bad, abs_phq_concentrate, abs_phq_slow,
                                    abs_phq_dead), na.rm = TRUE),
        gad7_score = rowSums(select(., abs_gad7_nerve, abs_gad7_uncontrol,
                                    abs_gad7_worry, abs_gad7_relax, abs_gad7_restless,
                                    abs_gad7_annoyed, abs_gad7_afraid), na.rm = TRUE)
    )



# PM+ Session 5 abstractions----
pm_session5_df <- ppw_rct_df%>% 
    filter(redcap_event_name == "PM+ Session 5 Abstraction (Arm 1: Intervention)") %>% 
    select(record_id, clt_study_site, clt_date, starts_with("abs_"))

###############################################################
# 3. ELIGIBILITY
###############################################################

treatment_referrals <- treatment_referrals %>%
    filter(!grepl("PM\\+ Session 5 Abstraction \\(Arm 1: Intervention\\)", 
                  redcap_event_name))%>% 
    mutate(max_score = 
               pmax(phq9_score,gad7_score,
                    na.rm = TRUE),
           eligible_for = case_when(
               abs_phq_dead > 0 ~ "Telepsychiatry",
               max_score >= 15 ~ "Telepsychiatry",
               max_score >= 10 ~ "PM+",
               TRUE ~ "Not Eligible"),
           referred_to = case_when(
               abs_phq_ref_pm == "Yes" |abs_gad7_ref_pm == "Yes" ~ "PM+",
               abs_phq_ref_tele == "Yes" | 
                   abs_gad7_ref_tele == "Yes" ~ "Telepsychiatry",
               TRUE ~ "None"),
           referral_time = 
               if_else(visit_type == "Enrollment","Enrollment",
                       "Follow-up"),
           severity = case_when(
               max_score < 10 ~ "Mild",
               between(max_score,10,14) ~ "Moderate",
               between(max_score,15,19) ~ "Moderately Severe",
               max_score >=20 ~ "Severe")
    )


###############################################################
# 4. KEEP PM+ PARTICIPANTS
###############################################################

pm_referrals <- treatment_referrals %>%
    group_by(record_id) %>%
    filter(eligible_for == "PM+" | referred_to == "PM+") %>%
    slice_head(n = 1) %>%   # keep only the first PM+ row per record_id
    ungroup()


###############################################################
# 5. ADD ANC NUMBER
###############################################################

pm_referrals <- rct_ppw_consenting %>%
    filter(rct_enrolling=="Yes") %>%
    select(anc_num,partipant_id) %>%
    right_join(pm_referrals,
              by=c("partipant_id"="record_id")
    )

# Check at which visit referrals were made
referral_visit <- pm_referrals %>% tabyl(visit_type)

# Check that referred at enrollment and any follow-up
enr_follow_up_ref <- pm_referrals %>%
    group_by(anc_num) %>%
    filter(any(visit_type == "Enrollment") & any(visit_type != "Enrollment")) %>%
    ungroup()


###############################################################
# 6. PM SESSION SUMMARY
###############################################################

pm_summary <- pm_survey_df %>%
    group_by(pm_ancid) %>%
    summarise(participant_status =
                  case_when(any(!is.na(pm_ptid)) &
                                !any(is.na(pm_ptid))~ "Study Participant",
                            any(!is.na(pm_ptid)) & any(is.na(pm_ptid)) ~ "PM+ then enrolled",
                            TRUE ~ "Non-study Participant"),
              pre = any(pm_session=="Pre-intervention PSYCHLOPS measurement" &
                            pm_pt_attend=="Yes"),
              session1 = any(pm_session=="Session 1 content" & 
                                 pm_pt_attend=="Yes"),
              session2 = any(pm_session == "Session 2 content" & 
                                 pm_pt_attend=="Yes"),
              session3 = any(pm_session == "Session 3 content" &
                                 pm_pt_attend == "Yes"), 
              session4 = any(pm_session == "Session 4 content" &
                                 pm_pt_attend == "Yes"),
              session5 = any(pm_session == "Session 5 content" &
                                 pm_pt_attend == "Yes"),
              post = any(pm_session == "Post-intervention PSYCHLOPS measurement" &
                             pm_pt_attend=="Yes"),
              sessions_attended = sum(unique(
                  ifelse(
                      pm_pt_attend == "Yes" & 
                          pm_session %in% paste("Session",1:5,"content"),
                      pm_session,
                      NA)) %in% paste("Session",1:5,"content")),
              uptake = case_when( sessions_attended > 0 ~ "Started PM+",
                                  TRUE ~ "Never Started"),
              completion_status = case_when(
                  sessions_attended == 0 ~ NA_character_,      # Not applicable
                  sessions_attended == 5 ~ "Completed",
                  TRUE ~ "Incomplete"
              ),
              .groups="drop")%>%
    mutate(
        sessions_cat = case_when(
            sessions_attended == 0 ~ "0 Sessions (None)",
            sessions_attended == 1 ~ "1 Session",
            TRUE ~ paste0(sessions_attended, " Sessions")
        ),
        sessions_cat = factor(
            sessions_cat,
            levels = c(
                "5 Sessions",
                "4 Sessions",
                "3 Sessions",
                "2 Sessions",
                "1 Session",
                "0 Sessions (None)"
            )
        )
    )

###############################################################
# 7. ELIGIBILITY TIMING
###############################################################

eligibility_summary <-
    pm_referrals %>%
    group_by(anc_num) %>%
    summarise(
        eligible_enrollment =
            any(visit_type == "Enrollment"),
        eligible_followup = any(visit_type!= "Enrollment"),
        eligibility_flag = 
            case_when(eligible_enrollment & eligible_followup ~ 
                          "Enrollment + Follow-up",
                      eligible_enrollment ~ "Enrollment Only",
                      eligible_followup ~ "Follow-up Only"),
        .groups="drop")

# Check those referred but missing in eligibilty summary
eligibility_summary %>% tabyl(eligibility_flag)

###############################################################
# 8. MASTER DATASET
###############################################################

pm_master <- pm_summary %>%
    left_join(
        eligibility_summary,
        by = c("pm_ancid"="anc_num")) %>%
    left_join(pm_referrals %>%
                  select(anc_num,clt_study_site, eligible_for,
                         referred_to, referral_time, severity,
                         phq9_score, gad7_score) %>%
                  distinct(),
              by = c("pm_ancid"="anc_num")) %>% 
    filter(!is.na(pm_ancid))

eligibility_check <- pm_master %>% 
    filter(is.na(eligible_for))

study_part <- pm_master %>% 
    filter(participant_status == "Study Participant") %>% 
    filter(is.na(eligible_for))

###############################################################
# 9. PM+ SUMMARY TABLE
###############################################################
pm_summary <- pm_master %>% 
    tbl_summary(
        include = c(participant_status, eligible_for,
                    referred_to, eligibility_flag, referral_time, 
                    severity, uptake, completion_status, sessions_attended,
                    sessions_cat),
        type = list(
            sessions_attended ~ "continuous"),
        statistic = list(
            all_categorical() ~ "{n} ({p}%)",
            all_continuous() ~ "{median} ({p25}, {p75})"),
        missing = "no",
        label = list(
            participant_status ~ "Participants Status", 
            uptake ~ "Initiated PM+",
            sessions_attended ~ "Sessions Attended (Median [IQR])",
            sessions_cat ~ "Distribution of Sessions Attended"
        )) %>%
    bold_labels() %>% 
    modify_caption(
        "**PM+ Eligibility, Referral, Uptake and Completion Summary**") %>%
    gtsummary::as_gt()


############################################################################
# 10. Uptake and completion summary
##########################################################################
pm_facility <- pm_master %>%
    filter(!is.na(clt_study_site)) %>% 
    group_by(clt_study_site) %>%
    summarise(
        Referred = n_distinct(pm_ancid, na.rm = TRUE),
        Uptook   = sum(uptake == "Started PM+", na.rm = TRUE),
        `Uptake %` = ifelse(Referred > 0, Uptook / Referred * 100, NA_real_),
        Completed = sum(completion_status == "Completed", na.rm = TRUE),
        `Completion %` = ifelse(Uptook > 0, Completed / Uptook * 100, NA_real_),
        .groups = "drop"
    ) %>%
    mutate(
        `Uptake %` = round(`Uptake %`, 1),
        `Completion %` = round(`Completion %`, 1)
    ) %>%
    arrange(desc(`Uptake %`))

# Overall summary row
pm_overall <- pm_master %>%
    filter(!is.na(clt_study_site)) %>% 
    summarise(
        clt_study_site = "Overall",
        Referred = n_distinct(pm_ancid, na.rm = TRUE),
        Uptook   = sum(uptake == "Started PM+", na.rm = TRUE),
        `Uptake %` = ifelse(Referred > 0, Uptook / Referred * 100, NA_real_),
        Completed = sum(completion_status == "Completed", na.rm = TRUE),
        `Completion %` = ifelse(Uptook > 0, Completed / Uptook * 100, NA_real_),
        .groups = "drop"
    ) %>%
    mutate(
        `Uptake %` = round(`Uptake %`, 1),
        `Completion %` = round(`Completion %`, 1)
    )

# Bind facility + overall
pm_facility_all <- bind_rows(pm_overall, pm_facility)

# GT table
pm_uptake_tbl <- gt(pm_facility_all) %>%
    tab_header(
        title = md("**PM+ Uptake and Completion Summary**"),
        subtitle = md("Uptake among referred; completion among those who started")
    ) %>%
    fmt_number(
        columns = c(`Uptake %`, `Completion %`),
        decimals = 1
    ) %>%
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(rows = clt_study_site == "Overall")
    ) %>%
    tab_options(table.font.size = px(12), data_row.padding = px(4))



