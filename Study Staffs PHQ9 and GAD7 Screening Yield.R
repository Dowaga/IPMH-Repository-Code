# Header ------------------------------------------------------------------

# Author(s): David
# Date: April 10, 2025
# Screening Yields

# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")
#--------------------------------------------------------------------------

# Intervention Screening Yield for Facility administred Survey
yield_df <- ppw_rct_df %>% 
    filter(redcap_event_name == "Enrollment (Arm 1: Intervention)")



# Study-administered Screening Yield
study_yield_df <- ppw_rct_df %>% 
        select(record_id, clt_study_site, starts_with("phq_"), starts_with("gad7_"))

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

pm_telep_df <- study_yield_df %>% 
    filter((phq9_scores >= 10)|(gad7_scores >= 10)) %>% 
    mutate(
        max_score = pmax(phq9_scores, gad7_scores, na.rm = TRUE),  # Get the greatest score
        eligible_for = case_when(
            (max_score >= 10 & max_score < 15 & 
                 ((max_score == phq9_scores)|(max_score == gad7_scores))) ~ "PM+",
            max_score >= 15 ~ "Telepsychiatry"))

# Assuming your data frame is called `df`
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
        title = "Table 3: Study Administered PHQ9 and GAD7 Screening Yields",
        subtitle = "Showing Study Staff PHQ9 and GAD& Yieds") %>%
    gt::tab_options(
        table.font.size = "medium",
        data_row.padding = gt::px(1)) %>%
    tab_options(
        table.font.size = px(14))

yield_summary

