# Header ------------------------------------------------------------------

# Author(s): Yuwei
# Date: July 4, 2025
# This is a script that explores acceptability, appropriateness, and feasibility of IPMH at baseline,
# and how they are related to IPMH PHQ2/GAD2 screening rates at intervention facilities.

# Setup ------------------------------------------------------------------------
# Reference source codes & other dependencies:
source("REDCap_datapull.R")
library(lme4)
library(parameters)

#data prep ----------------
#only keep the databases we need
rm(list = setdiff(ls(), c("daily_closeout", "rct_hcw")))

##IS database---------------
is_survey_df <- rct_hcw

is_survey_df <- is_survey_df %>%
    mutate(
        visit_type = case_when(
            grepl("pre", redcap_event_name, ignore.case = TRUE) ~ "Baseline",
            grepl("6-month", redcap_event_name) ~ "6 month",
            grepl("12-month", redcap_event_name) ~ "12 month",
            TRUE ~ "Other"
        )
    ) %>% 
    mutate(visit_type = factor(visit_type, levels = c("Baseline", "6 month", "12 month")))

is_survey_df <- is_survey_df %>%
    mutate(
        facility_num = str_extract(facility_id, "^[0-9]+"),  # Extracts only the leading number
        arm = case_when(
            facility_num %in% c("01", "03", "04", "07", "09", "13", "16", "17", "19", "22") ~ "Intervention",
            TRUE ~ "Control"
        ))

#load data regarding acceptability, appropriateness, and feasibility
acceptability <- is_survey_df %>% 
    select(pt_id, pt_type, facility_id, arm, aim_1, aim_2,
           aim_3, aim_4, visit_type) %>% 
    filter(arm == "Intervention") %>% 
    filter(visit_type == "Baseline") %>% 
    filter(!is.na(pt_type)) %>%  # Remove rows where pt_type is NA
    mutate(pt_type = droplevels(factor(pt_type))) %>% 
    filter(!is.na(facility_id)) %>% 
    mutate(facility_id = droplevels(factor(facility_id))) 

# Code into numbers
acceptability <- acceptability %>% 
    mutate(across(c(starts_with("aim_")), ~ case_when(
        . == "Completely disagree" ~ 1,
        . == "Disagree" ~ 2,
        . == "Neither agree nor disagree" ~ 3,
        . == "Agree" ~ 4,
        . == "Completely agree" ~ 5,
        . == "Prefer not to answer" ~ NA_real_,  # Convert to NA
        TRUE ~ NA_real_  # Ensure numeric conversion
    ))) 
    
acceptability <- acceptability %>%
    mutate(
        aim_total = rowMeans(select(., starts_with("aim_")), na.rm = TRUE)
    )

acceptability_layonly <- acceptability %>%
    filter(pt_type == "Lay providers: 22") 

appropriateness <- is_survey_df %>% 
    select(visit_type, pt_id, pt_type, facility_id, arm, iam_1, iam_2, iam_3, iam_4) %>% 
    filter(arm == "Intervention") %>%     filter(visit_type == "Baseline") %>% 
    filter(!is.na(pt_type)) %>%  # Remove rows where pt_type is NA
    mutate(pt_type = droplevels(factor(pt_type))) %>% 
    filter(!is.na(facility_id)) %>% 
    mutate(facility_id = droplevels(factor(facility_id))) 

appropriateness <- appropriateness %>% 
    mutate(across(c(starts_with("iam_")), ~ case_when(
        . == "Completely disagree" ~ 1,
        . == "Disagree" ~ 2,
        . == "Neither agree nor disagree" ~ 3,
        . == "Agree" ~ 4,
        . == "Completely agree" ~ 5,
        . == "Prefer not to answer" ~ NA_real_,  # Convert to NA
        TRUE ~ NA_real_  # Ensure numeric conversion
    ))) 

appropriateness <- appropriateness %>%
    mutate(
        iam_total = rowMeans(select(., starts_with("iam_")), na.rm = TRUE)
    )

appropriateness_layonly <- appropriateness %>%
    filter(pt_type == "Lay providers: 22")

feasibility <- is_survey_df %>% 
    select(visit_type, pt_id, pt_type, facility_id, arm,  fim_1,  fim_2,  fim_3,  fim_4) %>% 
    filter(arm == "Intervention") %>%     filter(visit_type == "Baseline") %>% 
    filter(!is.na(pt_type)) %>%  # Remove rows where pt_type is NA
    mutate(pt_type = droplevels(factor(pt_type))) %>% 
    filter(!is.na(facility_id)) %>% 
    mutate(facility_id = droplevels(factor(facility_id))) 

# Code into numbers
feasibility <- feasibility %>% 
    mutate(across(c(starts_with("fim_")), ~ case_when(
        . == "Completely disagree" ~ 1,
        . == "Disagree" ~ 2,
        . == "Neither agree nor disagree" ~ 3,
        . == "Agree" ~ 4,
        . == "Completely agree" ~ 5,
        . == "Prefer not to answer" ~ NA_real_,  # Convert to NA
        TRUE ~ NA_real_  # Ensure numeric conversion
    ))) 

feasibility <- feasibility %>%
    mutate(
        fim_total = rowMeans(select(., starts_with("fim_")), na.rm = TRUE)
    )

feasibility_layonly <- feasibility %>%
    filter(pt_type == "Lay providers: 22")

# Combine all three dataframes
baseline_data <- acceptability %>%
    left_join(appropriateness, by = c("visit_type", "pt_id", "pt_type", "facility_id", "arm")) %>%
    left_join(feasibility, by = c("visit_type", "pt_id", "pt_type", "facility_id", "arm"))

baseline_data_layonly <- acceptability_layonly %>%
    left_join(appropriateness_layonly, by = c("visit_type", "pt_id", "pt_type", "facility_id", "arm")) %>%
    left_join(feasibility_layonly, by = c("visit_type", "pt_id", "pt_type", "facility_id", "arm"))

# make it facility level
facility_baseline <- baseline_data %>%
    group_by(facility_id) %>%
    summarise(
        aim_total_mean = mean(aim_total, na.rm = TRUE),
        iam_total_mean = mean(iam_total, na.rm = TRUE),
        fim_total_mean = mean(fim_total, na.rm = TRUE),
        n_participants = n(),
        .groups = 'drop'
    )
facility_baseline <- facility_baseline %>%
    mutate(facility_id = str_replace(facility_id, "^\\d+,\\s*", ""))

facility_baseline_layonly <- baseline_data_layonly %>%
    group_by(facility_id) %>%
    summarise(
        aim_total_mean = mean(aim_total, na.rm = TRUE),
        iam_total_mean = mean(iam_total, na.rm = TRUE),
        fim_total_mean = mean(fim_total, na.rm = TRUE),
        n_participants = n(),
        .groups = 'drop'
    )

facility_baseline_layonly <- facility_baseline_layonly %>%
    mutate(facility_id = str_replace(facility_id, "^\\d+,\\s*", ""))

## Daily screening rate database ----------------
daily_closeout$rct_dcr_date <- as.Date(daily_closeout$rct_dcr_date)
daily_closeout <- daily_closeout %>% filter(rct_dcr_date < "2025-07-01")

daily_closeout <- daily_closeout %>%
    mutate(rct_facility_name = str_replace(rct_facility_name, "Mirogi Heath Centre", "Mirogi Health Centre"))

daily_closeout <- daily_closeout %>%
   mutate(screening_rate = rct_screening/rct_anc_number) %>% 
    select(rct_facility_name, rct_dcr_autodate, rct_anc_number, rct_screening,
           screening_rate) %>% 
    rename(study_site = rct_facility_name, day = rct_dcr_autodate) %>%
    mutate(screening_rate = as.numeric(screening_rate)) 

## merging --------
 screening_rate <- daily_closeout %>%
  left_join(facility_baseline, by = c("study_site" = "facility_id"))

screening_rate_layonly <- daily_closeout %>%
  left_join(facility_baseline_layonly, by = c("study_site" = "facility_id")) 

# analysis [full IS data] -----------------
# Filter daily data for facilities with baseline measures
 daily_analysis <- screening_rate %>%
     filter(!is.na(aim_total_mean))

# drop all the screening rate that is above 100
 daily_analysis <- daily_analysis %>%
     filter(screening_rate <= 1)

# Basic overview
 daily_analysis %>%
     summarise(
         n_facilities = n_distinct(study_site),
         n_days = n_distinct(day),
         total_observations = n(),
         date_range = paste(min(day), "to", max(day)),
         days_per_facility = round(n() / n_distinct(study_site), 1)
     )
 daily_summary <- daily_analysis %>%
     group_by(study_site) %>%
     summarise(
         n_days = n(),
         daily_mean_rate = mean(screening_rate, na.rm = TRUE),
         daily_sd_rate = sd(screening_rate, na.rm = TRUE),
         daily_min_rate = min(screening_rate, na.rm = TRUE),
         daily_max_rate = max(screening_rate, na.rm = TRUE),
         aim_score = first(aim_total_mean),
         fim_score = first(fim_total_mean),
         iam_score = first(iam_total_mean),
         .groups = 'drop'
     ) %>%
     arrange(desc(daily_mean_rate))
 cor.test(daily_summary$daily_mean_rate, daily_summary$aim_score)
 cor.test(daily_summary$daily_mean_rate, daily_summary$fim_score)
 cor.test(daily_summary$daily_mean_rate, daily_summary$iam_score)
 
 daily_trends <- daily_analysis %>%
     arrange(study_site, day) %>%
     group_by(study_site) %>%
     mutate(day_number = row_number()-1) %>%
     ungroup()
 
 daily_trends <- daily_trends %>% 
     mutate(aim_binary = ifelse(aim_total_mean >= mean(aim_total_mean), "high", "low"),
            iam_binary = ifelse(iam_total_mean >= mean(iam_total_mean), "high", "low"),
            fim_binary = ifelse(fim_total_mean >= mean(fim_total_mean), "high", "low")) 
 
 ## Model with time only --------

 daily_model_basic <- glmer(cbind(rct_screening, rct_anc_number - rct_screening) ~ day_number + (1|study_site), 
                          data = daily_trends, family = "binomial")
 parameters(daily_model_basic, exponentiate = T)
 
 ##binary models - give very huge confidence intervals - not good ------
 
 daily_model_aim <- glmer(cbind(rct_screening, rct_anc_number - rct_screening) ~ day_number + aim_binary + (1|study_site), 
                         data = daily_trends, family = "binomial")
 parameters(daily_model_aim, exponentiate = T)
 
 daily_model_iam <- glmer(cbind(rct_screening, rct_anc_number - rct_screening) ~ day_number + iam_binary + (1|study_site), 
                          data = daily_trends, family = "binomial")
 parameters(daily_model_iam, exponentiate = T)
 
 daily_model_fim <- glmer(cbind(rct_screening, rct_anc_number - rct_screening) ~ day_number + fim_binary + (1|study_site), 
                          data = daily_trends, family = "binomial")
 parameters(daily_model_fim, exponentiate = T)
 
 
## Continuous models - more stable but need to standardize the predictors ------
daily_trends$aim_total_mean_z <- scale(daily_trends$aim_total_mean)[,1]
daily_trends$iam_total_mean_z <- scale(daily_trends$iam_total_mean)[,1]
daily_trends$fim_total_mean_z <- scale(daily_trends$fim_total_mean)[,1] 

daily_model_aim_con <- glmer(cbind(rct_screening, rct_anc_number - rct_screening) ~ day_number + aim_total_mean_z + (1|study_site), 
                         data = daily_trends, family = "binomial")
parameters(daily_model_aim_con, exponentiate = T)

daily_model_iam_con <- glmer(cbind(rct_screening, rct_anc_number - rct_screening) ~ day_number + iam_total_mean_z + (1|study_site), 
                          data = daily_trends, family = "binomial")
parameters(daily_model_iam_con, exponentiate = T)

daily_model_fim_con <- glmer(cbind(rct_screening, rct_anc_number - rct_screening) ~ day_number + fim_total_mean_z + (1|study_site), 
                          data = daily_trends, family = "binomial")
parameters(daily_model_fim_con, exponentiate = T)

# Combined model - This is key!
daily_model_combined <- glmer(cbind(rct_screening, rct_anc_number - rct_screening) ~ day_number + aim_total_mean_z + iam_total_mean_z + fim_total_mean_z + (1|study_site), 
                              data = daily_trends, family = "binomial")

parameters(daily_model_combined, exponentiate = T)

car::vif(daily_model_combined)

# Model comparisons
anova(daily_model_basic, daily_model_aim_con)
anova(daily_model_basic, daily_model_iam_con) 
anova(daily_model_basic, daily_model_fim_con)
anova(daily_model_basic, daily_model_combined)

# analysis [layonly IS data] -----------------
# Filter daily data for facilities with baseline measures
daily_analysis_layonly <- screening_rate_layonly %>%
    filter(!is.na(aim_total_mean))

# drop all the screening rate that is above 100
daily_analysis_layonly <- daily_analysis_layonly %>%
    filter(screening_rate <= 1)

# Basic overview
daily_analysis_layonly %>%
    summarise(
        n_facilities = n_distinct(study_site),
        n_days = n_distinct(day),
        total_observations = n(),
        date_range = paste(min(day), "to", max(day)),
        days_per_facility = round(n() / n_distinct(study_site), 1)
    )
daily_summary_layonly <- daily_analysis_layonly %>%
    group_by(study_site) %>%
    summarise(
        n_days = n(),
        daily_mean_rate = mean(screening_rate, na.rm = TRUE),
        daily_sd_rate = sd(screening_rate, na.rm = TRUE),
        daily_min_rate = min(screening_rate, na.rm = TRUE),
        daily_max_rate = max(screening_rate, na.rm = TRUE),
        aim_score = first(aim_total_mean),
        fim_score = first(fim_total_mean),
        iam_score = first(iam_total_mean),
        .groups = 'drop'
    ) %>%
    arrange(desc(daily_mean_rate))
cor.test(daily_summary_layonly$daily_mean_rate, daily_summary_layonly$aim_score)
cor.test(daily_summary_layonly$daily_mean_rate, daily_summary_layonly$fim_score)
cor.test(daily_summary_layonly$daily_mean_rate, daily_summary_layonly$iam_score)

daily_trends_layonly <- daily_analysis_layonly %>%
    arrange(study_site, day) %>%
    group_by(study_site) %>%
    mutate(day_number = row_number()-1) %>%
    ungroup()

## Model with time only --------

daily_model_basic_layonly <- glmer(cbind(rct_screening, rct_anc_number - rct_screening) ~ day_number + (1|study_site), 
                           data = daily_trends_layonly, family = "binomial")
parameters(daily_model_basic, exponentiate = T)

## Continuous models - more stable but need to standardize the predictors ------
daily_trends_layonly$aim_total_mean_z <- scale(daily_trends_layonly$aim_total_mean)[,1]
daily_trends_layonly$iam_total_mean_z <- scale(daily_trends_layonly$iam_total_mean)[,1]
daily_trends_layonly$fim_total_mean_z <- scale(daily_trends_layonly$fim_total_mean)[,1] 

daily_model_aim_con_layonly <- glmer(cbind(rct_screening, rct_anc_number - rct_screening) ~ day_number + aim_total_mean_z + (1|study_site), 
                             data = daily_trends_layonly, family = "binomial")
parameters(daily_model_aim_con_layonly, exponentiate = T)

daily_model_iam_con_layonly <- glmer(cbind(rct_screening, rct_anc_number - rct_screening) ~ day_number + iam_total_mean_z + (1|study_site), 
                             data = daily_trends_layonly, family = "binomial")
parameters(daily_model_iam_con_layonly, exponentiate = T)

daily_model_fim_con_layonly <- glmer(cbind(rct_screening, rct_anc_number - rct_screening) ~ day_number + fim_total_mean_z + (1|study_site), 
                             data = daily_trends_layonly, family = "binomial")
parameters(daily_model_fim_con_layonly, exponentiate = T)

# Combined model - This is key!
daily_model_combined_layonly <- glmer(cbind(rct_screening, rct_anc_number - rct_screening) ~ day_number + aim_total_mean_z + iam_total_mean_z + fim_total_mean_z + (1|study_site), 
                              data = daily_trends_layonly, family = "binomial")

parameters(daily_model_combined_layonly, exponentiate = T)

car::vif(daily_model_combined_layonly)

# Model comparisons
anova(daily_model_basic_layonly, daily_model_aim_con_layonly)
anova(daily_model_basic_layonly, daily_model_iam_con_layonly) 
anova(daily_model_basic_layonly, daily_model_fim_con_layonly)
anova(daily_model_basic_layonly, daily_model_combined_layonly)

# Graphs requested by Keshet ---------------
## line graph of daily screening ----------
ggplot(daily_trends, aes(x = day_number, y = screening_rate, color = study_site)) +
    geom_line(size = 1) +
    geom_point(alpha = 0.6) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    labs(
        title = "Daily Screening Coverage by Facility",
        x = "Implementation Day",
        y = "Screening Coverage (%)",
        color = "Study Site"
    ) +
    theme_minimal()

ggplot(daily_trends, aes(x = day_number, y = screening_rate, color = study_site)) +
    geom_point(alpha = 0.4, size = 0.8) +
    geom_smooth(method = "loess", se = FALSE, size = 1.2) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    labs(
        title = "Screening Coverage Trends by Facility", 
        subtitle = "Smoothed trends over implementation period",
        x = "Implementation Day",
        y = "Screening Coverage (%)",
        color = "Study Site"
    ) +
    theme_minimal() +
    theme(legend.position = "right")

ggplot(daily_trends, aes(x = day_number, y = screening_rate)) +
    geom_line(color = "steelblue", size = 0.8) +
    geom_point(alpha = 0.6) +
    facet_wrap(~ study_site, ncol = 3) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
        title = "Daily Screening Coverage by Facility",
        x = "Implementation Day", 
        y = "Screening Coverage (%)"
    ) +
    theme_minimal()

## table for IS by facility
facility_table <- baseline_data %>%
    group_by(facility_id) %>%
    summarise(
        N_providers = n(),
        AIM = paste0(round(mean(aim_total, na.rm = TRUE), 2), 
                     " (", round(sd(aim_total, na.rm = TRUE), 2), ")"),
        IAM = paste0(round(mean(iam_total, na.rm = TRUE), 2), 
                     " (", round(sd(iam_total, na.rm = TRUE), 2), ")"),
        FIM = paste0(round(mean(fim_total, na.rm = TRUE), 2), 
                     " (", round(sd(fim_total, na.rm = TRUE), 2), ")"),
        .groups = "drop"
    )
facility_table %>%
    kable(caption = "Acceptability (AIM), Appropriateness (IAM), and Feasibility (FIM) by Facility",
          col.names = c("Study Site", "N", "AIM Mean (SD)", "IAM Mean (SD)", "FIM Mean (SD)")) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))

# table for IS by facility (lay only)
facility_table_layonly <- baseline_data_layonly %>%
    group_by(facility_id) %>%
    summarise(
        N_providers = n(),
        AIM = paste0(round(mean(aim_total, na.rm = TRUE), 2), 
                     " (", round(sd(aim_total, na.rm = TRUE), 2), ")"),
        IAM = paste0(round(mean(iam_total, na.rm = TRUE), 2), 
                     " (", round(sd(iam_total, na.rm = TRUE), 2), ")"),
        FIM = paste0(round(mean(fim_total, na.rm = TRUE), 2), 
                     " (", round(sd(fim_total, na.rm = TRUE), 2), ")"),
        .groups = "drop"
    )
facility_table_layonly %>%
    kable(caption = "Acceptability (AIM), Appropriateness (IAM), and Feasibility (FIM) by Facility (Lay Only)",
          col.names = c("Study Site", "N", "AIM Mean (SD)", "IAM Mean (SD)", "FIM Mean (SD)")) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))

# use tbl_summary to create a table by provide type
tbl_summary(baseline_data, 
             by = pt_type, 
            include = c(aim_total, iam_total, fim_total),
            type = c(aim_total, iam_total, fim_total) ~ "continuous",
             statistic = list(all_continuous() ~ "{mean} ({sd})", 
                              all_categorical() ~ "{n} ({p}%)")) %>%
    add_overall() %>%
    add_p()

            