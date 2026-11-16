# Header -----------------------------------------------------------------------

# Author(s): Dowaga
# Date: November 04, 2026
# This is a script to prepare a cleaned Patient Health Questionnaire 9
# that's ready for analysis, manuscript writing, and sharing 

# Setup ------------------------------------------------------------------------
# Reference source codes & other dependencies:
source("Dependencies.R")
source("DataTeam_ipmh.R")
source("data_import.R")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1. only keep the databases we need----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(list = setdiff(ls(), c("ppw_rct_df", "ipmh_filepath",
                          "data_freeze", "clean_dir")))


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2. Create visit type from redcap event names and filter Enrollment----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ppw_rct_df <- ppw_rct_df %>% 
    mutate(
        visit_type = case_when(
            grepl("Enrollment", redcap_event_name) ~ "Enrollment",
            grepl("6 Weeks", redcap_event_name) ~ "6 Weeks",
            grepl("14 Weeks", redcap_event_name) ~ "14 Weeks", 
            grepl("6 Months", redcap_event_name) ~ "6 Months",
            TRUE ~ NA_character_
        ))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. Select varibales for the DPHQ9----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
phq9_df  <- ppw_rct_df %>% 
    select(clt_ptid, clt_study_site, clt_date, visit_type,
           starts_with("phq_"), patient_health_questionnaire_9_complete)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. Recode PHQ-9 items to numeric (0-3)----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Select the nine PHQ-9 questionnaire items
phq_items <- names(phq9_df)[5:13]

phq9_lookup <- c(
    "not at all" = 0,
    "several days" = 1,
    "more than half the days" = 2,
    "nearly every day" = 3
)

phq9_df <- phq9_df %>%
    mutate(
        across(
            all_of(phq_items),
            ~ {
                x <- trimws(tolower(as.character(.x)))
                numeric_value <- suppressWarnings(as.numeric(x))
                
                case_when(
                    !is.na(numeric_value) & numeric_value %in% 0:3 ~
                        numeric_value,
                    !is.na(x) ~
                        as.numeric(unname(phq9_lookup[x])),
                    TRUE ~ NA_real_
                )
            }
        )
    )


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4. Calculate overall PHQ-9 score per participant per visit----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

phq9_scored <- phq9_df %>%
    mutate(
        # Number of answered PHQ-9 items
        phq9_n_answered = rowSums(
            !is.na(pick(all_of(phq_items)))
        ),
        
        # Number of missing PHQ-9 items
        phq9_n_missing = length(phq_items) - phq9_n_answered,
        
        # Overall score: calculated only when all 9 items are answered
        phq9_total = if_else(
            phq9_n_answered == 9,
            rowSums(pick(all_of(phq_items)), na.rm = TRUE),
            NA_real_
        )
    ) %>%
    select(
        clt_ptid,
        clt_study_site,
        visit_type,
        phq9_total,
        phq9_n_answered,
        phq9_n_missing
    )


# Review scores by visit
phq9_scored %>%
    group_by(visit_type) %>%
    summarise(
        participants = n(),
        complete_scores = sum(!is.na(phq9_total)),
        incomplete_scores = sum(is.na(phq9_total)),
        mean_score = round(mean(phq9_total, na.rm = TRUE), 2),
        median_score = median(phq9_total, na.rm = TRUE),
        .groups = "drop"
    )


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 5. Check duplicate participant records per visit----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

phq9_duplicates <- phq9_scored %>%
    count(clt_ptid, visit_type) %>%
    filter(n > 1)

phq9_duplicates

# Resolve any duplicate participant-visit records before reshaping.


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 6. Reshape to wide format: one row per participant----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

phq9_wide <- phq9_scored %>%
    select(
        clt_ptid,
        clt_study_site,
        visit_type,
        phq9_total
    ) %>%
    pivot_wider(
        names_from = visit_type,
        values_from = phq9_total,
        names_prefix = "phq9_"
    ) %>%
    select(
        clt_ptid,
        clt_study_site,
        any_of(c(
            "phq9_Enrollment",
            "phq9_6 Weeks",
            "phq9_14 Weeks",
            "phq9_6 Months"
        ))
    )


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 7. Calculate differences between visits----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

phq9_clean_df <- phq9_wide %>%
    mutate(
        # Enrollment compared with 6 Weeks
        diff_enrollment_6w =
            phq9_Enrollment- `phq9_6 Weeks` ,
        
        # Enrollment compared with 14 Weeks
        diff_enrollment_14w =
             phq9_Enrollment - `phq9_14 Weeks`,
        
        # 6 Weeks compared with 6 Months
        diff_enrollment_6m =
            `phq9_Enrollment` - `phq9_6 Months`,
        
        #  6 Weeks compared with 14 Weeks
        diff_6w_14w =
            `phq9_6 Weeks` - `phq9_14 Weeks`,
        
        # 6 Weeks compared with 6 Months
        diff_6w_6m =
            `phq9_6 Weeks` - `phq9_6 Months`,
        
        # 14 Weeks compared with 6 Months
        diff_14w_6m =
            `phq9_14 Weeks` - `phq9_6 Months`,
        
        # Overall change from Enrollment to 6 Months
        diff_overall =
            phq9_Enrollment - `phq9_6 Months`
    )







