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

rct_ppw <- rct_ppw %>%
    filter(is.na(redcap_repeat_instrument)) %>%
    filter(record_id %in% ids_with_session5)

rct_ppw_en_5session <- rct_ppw %>%
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
    filter(!is.na(phq9_scores_enrollment) & !is.na(phq9_scores_session5) &
               !is.na(gad7_scores_enrollment) & !is.na(gad7_scores_session5)) %>%
    mutate(
        phq9_diff = phq9_scores_session5 - phq9_scores_enrollment,
        gad7_diff = gad7_scores_session5 - gad7_scores_enrollment,
        phq9_pct_reduction = 100 * (phq9_scores_enrollment - phq9_scores_session5) / phq9_scores_enrollment,
        gad7_pct_reduction = 100 * (gad7_scores_enrollment - gad7_scores_session5) / gad7_scores_enrollment
    ) %>%
    filter(
        (phq9_scores_enrollment > 0 & phq9_pct_reduction < 50) | abs(phq9_diff) < 5 |
            (gad7_scores_enrollment > 0 & gad7_pct_reduction < 50) | abs(gad7_diff) < 5
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
table(rct_ppw$su_pmrestart___0, rct_ppw$redcap_event_name)
table(rct_ppw$su_pmrestart___1)
table(rct_ppw$su_pmrestart___2)
table(rct_ppw$su_pmrestart_sp)
table(rct_ppw$su_pmrestart_spother)
table(rct_ppw$su_tele, useNA = "ifany") #0 so none of them have used telepsychiatry

score_classified <- score_combined_anc_flagged %>%
    left_join(rct_ppw %>%
                  select(clt_ptid, su_pmp, su_pmrestart___1, su_pmrestart___2, su_tele),
              by = c("record_id" = "clt_ptid")) %>%
    mutate(
        category = case_when(
            su_pmrestart___1 == "Checked" | su_pmrestart___2 == "Checked" ~ "Delayed or Restarted PM+",
            su_tele == "Yes" & (phq9_pct_reduction < 50 | gad7_pct_reduction < 50 |
                                abs(phq9_diff) < 5 | abs(gad7_diff) < 5) ~ "Escalated to Telepsychiatry",
            su_tele == "Yes" & su_pmp != "Yes" ~ "Direct Telepsychiatry",
            su_tele != "Yes" & (phq9_pct_reduction >= 50 | gad7_pct_reduction >= 50 |
                                phq9_diff <= -5 | gad7_diff <= -5) ~ "Completed PM+",
            TRUE ~ "Unclassified"
        )
    )
score_classified %>%
    count(category, name = "n") %>%
    mutate(pct = round(100 * n / sum(n), 1)) %>%
    flextable() %>%
    set_caption("Refined Classification of 33 Participants Using Survey Data") %>%
    autofit()
