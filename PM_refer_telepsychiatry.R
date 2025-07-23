# Header ------------------------------------------------------------------

# Author(s): Yuwei
# Date: July 10, 2025
# This is a script that explores the trajectory of participants using PM+ 
# and telepsychiatry services among those who have completed all PM+ sessions.

# Setup ------------------------------------------------------------------------
# Reference source codes & other dependencies:
source("REDCap_datapull.R")

#data prep ----------------
#only keep the databases we need
rm(list = setdiff(ls(), c("telepsych", "pm", "rct_ppw", "rct_ppw_consenting")))

pm <- pm %>% 
    filter(ipmh_participant == "Yes")

ids_with_session5 <- rct_ppw %>%
    filter(redcap_event_name == "PM+ Session 5 Abstraction (Arm 1: Intervention)") %>%
    pull(record_id)

rct_ppw_session5 <- rct_ppw %>%
    filter(is.na(redcap_repeat_instrument)) %>%
    filter(record_id %in% ids_with_session5)

rct_ppw_en_5session <- rct_ppw_session5 %>%
    filter(redcap_event_name %in% c("Enrollment (Arm 1: Intervention)", 
                                    "PM+ Session 5 Abstraction (Arm 1: Intervention)"))

# merge anc_number in consenting to rct_ppw
rct_ppw_en_5session <- rct_ppw_en_5session %>%
    left_join(rct_ppw_consenting %>% select(partipant_id, anc_num), by = c("clt_ptid" = "partipant_id"))


#look at the PHQ9/GAD7 enrollment & 5th session 
score <- rct_ppw_en_5session %>%
    select(redcap_event_name, record_id, clt_study_site, clt_date, starts_with("abs_")) 

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

# Recode variables
score <- score %>%
    mutate(across(c(abs_phq_interest, abs_phq_down, abs_phq_sleep,
                    abs_phq_tired, abs_phq_appetite, abs_phq_bad,
                    abs_phq_concentrate, abs_phq_slow, abs_phq_dead), ~ recode(., !!!phq9_labels)),
           across(c(abs_gad7_nerve, abs_gad7_uncontrol, 
                    abs_gad7_worry, abs_gad7_relax, abs_gad7_restless,
                    abs_gad7_annoyed, abs_gad7_afraid), ~ recode(., !!!gad7_labels)))

score <- score %>% 
    mutate(phq9_scores = rowSums(select(., abs_phq_interest, abs_phq_down,
                                        abs_phq_sleep, abs_phq_tired, 
                                        abs_phq_appetite, abs_phq_bad,
                                        abs_phq_concentrate, abs_phq_slow, 
                                        abs_phq_dead), na.rm = TRUE),
           gad7_scores = rowSums(select(., abs_gad7_nerve, abs_gad7_uncontrol,
                                        abs_gad7_worry, abs_gad7_relax, 
                                        abs_gad7_restless, abs_gad7_annoyed,
                                        abs_gad7_afraid), na.rm = TRUE))
self_harm <- score %>% 
    filter(abs_phq_dead > 0) #0 found

score <- score %>%
    mutate(event = case_when(
        redcap_event_name == "Enrollment (Arm 1: Intervention)" ~ "enrollment",
        redcap_event_name == "PM+ Session 5 Abstraction (Arm 1: Intervention)" ~ "session5"
    ))
score_wide <- score %>%
    select(record_id, event, phq9_scores, gad7_scores) %>%
    pivot_wider(names_from = event, values_from = c(phq9_scores, gad7_scores), names_sep = "_")

# reduction of scores ===========
score_combined_df <- score_wide %>%
    filter(!is.na(phq9_scores_enrollment), !is.na(phq9_scores_session5),
           !is.na(gad7_scores_enrollment), !is.na(gad7_scores_session5)) %>%
    mutate(
        phq9_diff = phq9_scores_session5 - phq9_scores_enrollment,
        gad7_diff = gad7_scores_session5 - gad7_scores_enrollment,
        phq9_pct_reduction = 100 * (phq9_scores_enrollment - phq9_scores_session5) / phq9_scores_enrollment,
        gad7_pct_reduction = 100 * (gad7_scores_enrollment - gad7_scores_session5) / gad7_scores_enrollment
    ) %>%
    filter(
        (phq9_scores_enrollment > 10 & (phq9_pct_reduction < 50 | abs(phq9_diff) < 5)) |
            (gad7_scores_enrollment > 10 & (gad7_pct_reduction < 50 | abs(gad7_diff) < 5))
    ) %>%
    select(record_id,
           phq9_scores_enrollment, phq9_scores_session5, phq9_diff, phq9_pct_reduction,
           gad7_scores_enrollment, gad7_scores_session5, gad7_diff, gad7_pct_reduction)

score_combined_df %>%
    flextable() %>%
    set_caption("Participants with <50% Reduction or <5-Point Decrease in PHQ-9 or GAD-7 Scores") %>%
    
    # Format numeric values
    colformat_num(j = c("phq9_diff", "gad7_diff"), digits = 1) %>%
    colformat_num(j = c("phq9_pct_reduction", "gad7_pct_reduction"), digits = 1, suffix = "%") %>%
    
    # Highlight rows not meeting 50% reduction or 5-point drop
    bg(i = ~ phq9_pct_reduction < 50, j = "phq9_pct_reduction", bg = "#FADBD8") %>%
    bg(i = ~ abs(phq9_diff) < 5, j = "phq9_diff", bg = "#FADBD8") %>%
    bg(i = ~ gad7_pct_reduction < 50, j = "gad7_pct_reduction", bg = "#FADBD8") %>%
    bg(i = ~ abs(gad7_diff) < 5, j = "gad7_diff", bg = "#FADBD8") %>%
    
    # Optional: bold the highlighted cells
    bold(i = ~ phq9_pct_reduction < 50, j = "phq9_pct_reduction", bold = TRUE) %>%
    bold(i = ~ abs(phq9_diff) < 5, j = "phq9_diff", bold = TRUE) %>%
    bold(i = ~ gad7_pct_reduction < 50, j = "gad7_pct_reduction", bold = TRUE) %>%
    bold(i = ~ abs(gad7_diff) < 5, j = "gad7_diff", bold = TRUE) %>%
    
    # Autofit table
    autofit()

# Those people need to be referred to telepsychiatry. How many of them have been referred?
# referral? ========
# Restrict to enrollment rows only (1 per participant)
rct_ppw_enroll <- rct_ppw_en_5session %>%
    filter(redcap_event_name == "Enrollment (Arm 1: Intervention)") %>%
    select(clt_ptid, anc_num) %>%
    distinct(clt_ptid, .keep_all = TRUE)

# Join to get unique anc_num
score_combined_anc <- score_combined_df %>%
    left_join(rct_ppw_enroll, by = c("record_id" = "clt_ptid")) %>%
    select(record_id, anc_num, everything())

# Join with telepsychiatry data
score_combined_anc_flagged <- score_combined_anc %>%
    left_join(
        telepsych %>% 
            select(tele_ancid, pt_attend) %>%
            distinct(tele_ancid, .keep_all = TRUE),
        by = c("anc_num" = "tele_ancid")
    ) %>%
    mutate(
        telepsychiatry_record = ifelse(!is.na(pt_attend), "Yes", "No")
    )

#Trajectory ==============
# No body is escalated to telepsychiatry
unique(telepsych$tele_ancid) %>% 
    length()
# 8 is receiving telepsychiatry services directly
unique(ids_with_session5) %>% 
    length()
# 33 completed PM+
# Among these 33 women - 
session5_participant_info <- rct_ppw %>%
    filter(clt_ptid %in% ids_with_session5) %>%
    select(clt_ptid, su_pmp, su_tele,
           su_pmrestart___0, su_pmrestart___1, su_pmrestart___2,
           su_pmrestart_sp, su_pmrestart_spother) %>%
    distinct()
pm_or_tele_flag <- session5_participant_info %>%
    group_by(clt_ptid) %>%
    summarise(
        ever_stopped = any(su_pmrestart___1 == "Checked", na.rm = TRUE),
        ever_restarted = any(su_pmrestart___2 == "Checked", na.rm = TRUE),
        ever_stopped_or_restarted = ever_stopped | ever_restarted)
pm_or_tele_flag %>%
    count(ever_stopped_or_restarted, name = "n") %>%
    mutate(
        label = ifelse(ever_stopped_or_restarted, "Yes", "No"),
        pct = round(100 * n / sum(n), 1)
    ) %>%
    select(`Ever Stopped or Restarted` = label, `Participants (n)` = n, `Percent (%)` = pct)
# 8 participants have ever stopped or restarted PM+.
# Among all participants - 
pm_tele_info <- rct_ppw %>%
    select(clt_ptid, redcap_event_name, su_pmp, su_tele,
           su_pmrestart___0, su_pmrestart___1, su_pmrestart___2,
           su_pmrestart_sp, su_pmrestart_spother, starts_with("abs_")) %>%
    filter(!is.na(clt_ptid))
pm_tele_summary <- pm_tele_info %>%
    group_by(clt_ptid) %>%
    summarise(
        any_pm = any(su_pmp == "Yes", na.rm = TRUE),
        any_tele = any(su_tele == "Yes", na.rm = TRUE),
        ever_stopped_or_restarted = any(su_pmrestart___1 == "Checked" | su_pmrestart___2 == "Checked", na.rm = TRUE))
pm_tele_summary %>%
    count(ever_stopped_or_restarted, name = "n") %>%
    mutate(
        label = ifelse(ever_stopped_or_restarted, "Yes", "No"),
        pct = round(100 * n / sum(n), 1)
    ) %>%
    select(`Ever Stopped or Restarted` = label, `Participants (n)` = n, `Percent (%)` = pct)
# 18 participants have ever stopped or restarted PM+.

