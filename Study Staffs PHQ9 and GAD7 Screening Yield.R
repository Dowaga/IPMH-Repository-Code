# Header ------------------------------------------------------------------

# Author(s): David
# Date: April 10, 2025
# Screening Yields

# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")
#--------------------------------------------------------------------------

# Intervention Facility Staff Screening Yields (PHQ9 and GAD7)
facility_yield_df <- ppw_rct_df %>% 
    filter(redcap_event_name == "Enrollment (Arm 1: Intervention)") %>% 
    select(record_id, clt_study_site,starts_with("abs_"))

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
facility_yield_df <- facility_yield_df %>%
    mutate(across(c(abs_phq_interest, abs_phq_down, abs_phq_sleep,
                    abs_phq_tired, abs_phq_appetite, abs_phq_bad,
                    abs_phq_concentrate, abs_phq_slow, abs_phq_dead), ~ recode(., !!!phq9_labels)),
           across(c(abs_gad7_nerve, abs_gad7_uncontrol, 
                    abs_gad7_worry, abs_gad7_relax, abs_gad7_restless,
                    abs_gad7_annoyed, abs_gad7_afraid), ~ recode(., !!!gad7_labels)))



facility_yield_df <- facility_yield_df %>% 
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
self_harm <- facility_yield_df %>% 
    filter(abs_phq_dead > 0)


# PHQ9 And GAD7 Positives
facility_yield <- facility_yield_df %>%
    mutate(
        phq9_positive = if_else(phq9_scores >= 10, 1, 0),  # common cutoff is ???10
        gad7_positive = if_else(gad7_scores >= 10, 1, 0)
    ) %>% 
    mutate(clt_study_site = gsub("^[0-9]+,\\s*", "", clt_study_site))

fc_yield_summary <- facility_yield %>%
    group_by(clt_study_site) %>%
    summarise(
        screened_phq9 = sum(!is.na(phq9_scores)),
        #Overall = sum(phq9_positive | gad7_positive, na.rm = TRUE),
        phq9_positive = sum(phq9_positive, na.rm = TRUE),
        screened_gad7 = sum(!is.na(gad7_scores)),
        gad7_positive = sum(gad7_positive, na.rm = TRUE)
    ) %>%
    mutate(
        `PHQ9 Yield` = round((phq9_positive / screened_phq9) * 100, 1),
        `GAD7 Yield` = round((gad7_positive / screened_gad7) * 100, 1)
    ) %>% 
    select(-c(screened_gad7)) %>% 
    rename(`Total Screened` = screened_phq9,
           `PHQ9 Positive` = phq9_positive,
           `GAD7 Positive` = gad7_positive,
           `Facility` = clt_study_site) %>% 
    # convert from gtsummary object to gt object
    gt() %>%
    # modify with gt functions
    gt::tab_header(
        title = "Facility Administered PHQ9 and GAD7 Screening Yields",
        subtitle = "Showing Facility Staff PHQ9 and GAD7 Yields") %>%
    gt::tab_options(
        table.font.size = "medium",
        data_row.padding = gt::px(1)) %>%
    tab_options(
        table.font.size = px(14))

fc_yield_summary

#-------------------------------------------------------------------------------
### Depressive symptoms profile across facilities
phq9_distribution <- facility_yield %>%
    tbl_summary(by = "clt_study_site",
                include = c(phq9_positive, phq9_scores, abs_phq_interest, abs_phq_down, abs_phq_sleep,
                            abs_phq_tired, abs_phq_appetite, abs_phq_bad,
                            abs_phq_concentrate, abs_phq_slow, abs_phq_dead),
                label = list(abs_phq_interest ~ "Little interest or pleasure in doing things",
                             abs_phq_down ~ "Feeling down, depressed, or hopeless",
                             abs_phq_sleep ~ "Trouble falling or staying asleep, or sleeping too much ",
                             abs_phq_tired ~ "Feeling tired or having little energy ",
                             abs_phq_appetite ~ "Poor appetite or overeating",
                             abs_phq_bad ~ "Feeling bad about yourself - or that you are a failure or have let yourself or y",
                             abs_phq_concentrate ~ "Trouble concentrating on things, such as reading the newspaper or watching telev",
                             abs_phq_slow ~ "Moving or speaking so slowly that other people could have noticed. Or the opposi",
                             abs_phq_dead ~ "Thoughts that you would be better off dead, or of hurting yourself?",
                             phq9_scores ~ "Median PHQ9 Scores")) %>% 
                    bold_labels()%>% 
                    as_gt() %>% 
                    tab_header(title = "Depressive Symptom Profile Across Facilities",
               subtitle = "Item-Level PHQ-9 Frequency and Proportion of Depressive Symptoms Across Health Facilities") %>%
                   gt::tab_options(
                       table.font.size = "medium",
                       data_row.padding = gt::px(1))
phq9_distribution
#--------------------------------------------------------------------------
# Study Staff Screening Yields (PHQ9 and GAD7)
study_yield_df <- ppw_rct_df %>% 
        select(record_id, clt_study_site, starts_with("phq_"), starts_with("gad7_")) %>% 
    filter(!is.na(clt_study_site))

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
study_yield_df <- study_yield_df %>%
    mutate(across(c(phq_interest, phq_down, phq_sleep,
                    phq_tired, phq_appetite, phq_bad,
                    phq_concentrate, phq_slow, phq_dead), ~ recode(., !!!phq9_labels)),
           across(c(gad7_nerve, gad7_uncontrol, 
                    gad7_worry, gad7_relax, gad7_restless,
                    gad7_annoyed, gad7_afraid), ~ recode(., !!!gad7_labels)))



study_yield_df <- study_yield_df %>% 
    mutate(phq9_scores = rowSums(select(., phq_interest, phq_down,
                                        phq_sleep, phq_tired, 
                                        phq_appetite, phq_bad,
                                        phq_concentrate, phq_slow, 
                                        phq_dead), na.rm = TRUE),
           gad7_scores = rowSums(select(., gad7_nerve, gad7_uncontrol,
                                        gad7_worry, gad7_relax, 
                                        gad7_restless, gad7_annoyed,
                                        gad7_afraid), na.rm = TRUE))

# Endorsed PHQ9 Question 9
self_harm <- study_yield_df %>% 
    filter(phq_dead > 0)

study_yield_df <- study_yield_df %>% 
    mutate(
        max_score = pmax(phq9_scores, gad7_scores, na.rm = TRUE),  # Get the greatest score
        eligible_for = case_when(
            (max_score >= 10 & max_score < 15 & 
                 ((max_score == phq9_scores)|(max_score == gad7_scores))) ~ "PM+",
            max_score >= 15 ~ "Telepsychiatry"))

# PHQ9 And GAD7 Positives
yield_df <- study_yield_df %>%
    select(-c(phq_difficulty, gad7_difficulty)) %>% 
    mutate(
        phq9_positive = if_else(phq9_scores >= 10, 1, 0),  # common cutoff is ???10
        gad7_positive = if_else(gad7_scores >= 10, 1, 0)
    ) %>% 
    mutate(clt_study_site = gsub("^[0-9]+,\\s*", "", clt_study_site)) %>% 
    mutate(facility = as.numeric(str_sub(record_id, 3, 4))) %>% 
    mutate(arm = case_when(
        facility %in% c(2, 5, 6, 8, 11, 14, 15, 18, 20, 21) ~ "Control",
        TRUE ~ "Intervention"))


yield_summary <- yield_df %>%
    group_by(arm, clt_study_site) %>%
    summarise(
        screened_phq9 = sum(!is.na(phq9_scores)),
        #Overall = sum(phq9_positive | gad7_positive, na.rm = TRUE),
        phq9_positive = sum(phq9_positive, na.rm = TRUE),
        screened_gad7 = sum(!is.na(gad7_scores)),
        gad7_positive = sum(gad7_positive, na.rm = TRUE)
    ) %>%
    mutate(
        `PHQ9 Yield` = round((phq9_positive / screened_phq9) * 100, 1),
        `GAD7 Yield` = round((gad7_positive / screened_gad7) * 100, 1)
    ) %>% 
    select(-c(screened_gad7)) %>% 
    rename(`Total Screened` = screened_phq9,
           `PHQ9 Positive` = phq9_positive,
           `GAD7 Positive` = gad7_positive,
           `Facility` = clt_study_site) %>% 
    # convert from gtsummary object to gt object
    gt() %>%
    # modify with gt functions
    gt::tab_header(
        title = "Study Administered PHQ9 and GAD7 Screening Yields",
        subtitle = "Showing Study Staff PHQ9 and GAD7 Yields") %>%
    gt::tab_options(
        table.font.size = "medium",
        data_row.padding = gt::px(1)) %>%
    tab_options(
        table.font.size = px(14))

yield_summary


# Missed Opportunities

facility_yield <- facility_yield %>% 
    rename(phq9_fac = phq9_positive,
           gad7_fac = gad7_positive)

yield_df <- yield_df %>% 
    rename(phq9_staff = phq9_positive,
           gad7_staff = gad7_positive)

missed_oppor <- facility_yield %>% 
    select(record_id, clt_study_site, phq9_fac, gad7_fac) %>% 
    right_join(yield_df %>% 
                   select(record_id, clt_study_site, phq9_staff, gad7_staff, arm),
               by = c("record_id", "clt_study_site")) %>% 
    #filter the intervention sites
    filter(arm == "Intervention")%>%
    select(-arm) %>% 
    mutate(
        missed_opportunity = case_when(
            (phq9_staff == 1 & phq9_fac == 0) |
                (gad7_staff == 1 & gad7_fac == 0)  ~ 1L,
            TRUE                                 ~ 0L
        )
    ) %>% 
    filter(missed_opportunity == 1)


# # Save each Missed opportunityas a separate xlsx file with date in the filename
# missed_oppor %>%
#     group_split(clt_study_site) %>%
#     walk(~ write_xlsx(.x, path = paste0("C:/Users/DAMARIS/Desktop/IPMH/QCs/", 
#                                         "Missed Opportunities ", 
#                                         unique(.x$clt_study_site), "_", 
#                                         format(Sys.Date(), "%Y-%m-%d"), ".xlsx")))

# sensitivity & specificity table
# Merge study and facility data
comparison_df <- facility_yield %>% 
    select(record_id, clt_study_site, phq9_fac, gad7_fac) %>%
    inner_join(yield_df %>% 
                   select(record_id, phq9_staff, gad7_staff),
               by = "record_id")

# Combine results for PHQ-9 and GAD-7
phq9_eval <- comparison_df %>%
    mutate(
        TP = phq9_staff == 1 & phq9_fac == 1,
        FP = phq9_staff == 0 & phq9_fac == 1,
        FN = phq9_staff == 1 & phq9_fac == 0,
        TN = phq9_staff == 0 & phq9_fac == 0
    ) %>%
    summarise(
        TP = sum(TP), FP = sum(FP), FN = sum(FN), TN = sum(TN),
        Sensitivity = TP / (TP + FN),
        Specificity = TN / (TN + FP)
    ) %>%
    mutate(measure = "PHQ-9")

gad7_eval <- comparison_df %>%
    mutate(
        TP = gad7_staff == 1 & gad7_fac == 1,
        FP = gad7_staff == 0 & gad7_fac == 1,
        FN = gad7_staff == 1 & gad7_fac == 0,
        TN = gad7_staff == 0 & gad7_fac == 0
    ) %>%
    summarise(
        TP = sum(TP), FP = sum(FP), FN = sum(FN), TN = sum(TN),
        Sensitivity = TP / (TP + FN),
        Specificity = TN / (TN + FP)
    ) %>%
    mutate(measure = "GAD-7")

# Combine both into one table
eval_summary <- bind_rows(phq9_eval, gad7_eval) %>%
    select(measure, TP, FP, FN, TN, Sensitivity, Specificity)

# Create GT table
gt(eval_summary) %>%
    tab_header(
        title = "Facility Staff Screening Validity",
        subtitle = "Using Study Staff Screening as Gold Standard"
    ) %>%
    fmt_number(columns = c(Sensitivity, Specificity), decimals = 2) %>%
    cols_label(
        measure = "Measure",
        TP = "True Positive",
        FP = "False Positive",
        FN = "False Negative",
        TN = "True Negative",
        Sensitivity = "Sensitivity",
        Specificity = "Specificity"
    ) %>%
    tab_options(
        table.font.size = "medium",
        data_row.padding = px(3)
    )
