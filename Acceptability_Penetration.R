# Header ------------------------------------------------------------------

# Author(s): Yuwei
# Date: July 4, 2025
# This is a script that explores acceptability, appropriateness, and feasibility of IPMH at baseline,
# and how they are related to IPMH PHQ2/GAD2 screening rates at intervention facilities.

# Setup ------------------------------------------------------------------------
# Reference source codes & other dependencies:
source("REDCap_datapull.R")
source("Audit and feedback script.R")

#data prep ----------------
#only keep the databases we need
rm(list = setdiff(ls(), c("screening_rate_weekly", "screening_rate", "total_screening_rate", "rct_hcw")))
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

# Combine all three dataframes
baseline_data <- acceptability %>%
    left_join(appropriateness, by = c("visit_type", "pt_id", "pt_type", "facility_id", "arm")) %>%
    left_join(feasibility, by = c("visit_type", "pt_id", "pt_type", "facility_id", "arm"))

# make it facility level
facility_baseline_simple <- baseline_data %>%
    group_by(facility_id) %>%
    summarise(
        aim_total_mean = mean(aim_total, na.rm = TRUE),
        iam_total_mean = mean(iam_total, na.rm = TRUE),
        fim_total_mean = mean(fim_total, na.rm = TRUE),
        n_participants = n(),
        .groups = 'drop'
    )
facility_baseline_simple <- facility_baseline_simple %>%
    mutate(facility_id = str_replace(facility_id, "^\\d+,\\s*", ""))

# merging 
 screening_rate_with_baseline <- screening_rate %>%
  left_join(facility_baseline_simple, by = c("study_site" = "facility_id"))

 screening_rate_weekly_with_baseline <- screening_rate_weekly %>%
     left_join(facility_baseline_simple, by = c("study_site" = "facility_id"))
 
 total_screening_rate_with_baseline <- total_screening_rate %>%
     left_join(facility_baseline_simple, by = c("study_site" = "facility_id"))

# analysis using the total screening rate database ---------------
 total_screening_analysis <- total_screening_rate_with_baseline %>%
     rename(
         screening_rate = `PHQ2/GAD2 screening rate`,
         total_screening = `Total screening`,
         total_anc = `Total ANC clients`
     ) %>%
     filter(!is.na(aim_total_mean))
 
 # Correlation matrix
 cor_data <- total_screening_analysis %>%
     select(screening_rate, aim_total_mean, iam_total_mean, fim_total_mean)
 
 # Calculate correlations
 cor_matrix <- cor(cor_data, use = "complete.obs")
 print(round(cor_matrix, 3))
 
 # Test statistical significance
 cor.test(total_screening_analysis$screening_rate, total_screening_analysis$aim_total_mean)
 cor.test(total_screening_analysis$screening_rate, total_screening_analysis$iam_total_mean)
 cor.test(total_screening_analysis$screening_rate, total_screening_analysis$fim_total_mean)

 # Scatterplot matrix
 library(GGally)
 ggpairs(total_screening_analysis[, c("screening_rate", "aim_total_mean", 
                                      "iam_total_mean", "fim_total_mean")],
         columnLabels = c("Screening Rate", "Acceptability", "Appropriateness", "Feasibility"))
 
 # Individual scatterplots
 p1 <- ggplot(total_screening_analysis, aes(x = aim_total_mean, y = screening_rate)) +
     geom_point(size = 3, alpha = 0.7) +
     geom_smooth(method = "lm", se = TRUE) +
     labs(title = "Acceptability vs Screening Rate", 
          x = "AIM Score (Acceptability)", y = "Screening Rate (%)") +
     theme_minimal()
 
 p2 <- ggplot(total_screening_analysis, aes(x = iam_total_mean, y = screening_rate)) +
     geom_point(size = 3, alpha = 0.7) +
     geom_smooth(method = "lm", se = TRUE) +
     labs(title = "Appropriateness vs Screening Rate", 
          x = "IAM Score (Appropriateness)", y = "Screening Rate (%)") +
     theme_minimal()
 
 p3 <- ggplot(total_screening_analysis, aes(x = fim_total_mean, y = screening_rate)) +
     geom_point(size = 3, alpha = 0.7) +
     geom_smooth(method = "lm", se = TRUE) +
     labs(title = "Feasibility vs Screening Rate", 
          x = "FIM Score (Feasibility)", y = "Screening Rate (%)") +
     theme_minimal()
 
 # Display plots
 library(gridExtra)
 grid.arrange(p1, p2, p3, ncol = 2)
 
 ggplot(total_screening_analysis, aes(x = aim_total_mean, y = screening_rate)) +
     geom_point(size = 4, alpha = 0.7, color = "steelblue") +
     geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
     geom_text_repel(aes(label = paste0(substr(study_site, 1, 15), "\n(", round(screening_rate, 1), "%)")), 
                     size = 3, box.padding = 0.5) +
     labs(title = "The Implementation Paradox", 
          subtitle = "Higher baseline acceptability ??? Lower screening performance (r = -0.60, p = 0.066)",
          x = "Baseline Acceptability Score (AIM)", 
          y = "Actual Screening Rate (%)") +
     theme_minimal() +
     theme(plot.title = element_text(size = 14, face = "bold"))
 
 # Create a summary table showing the paradox
 total_screening_analysis %>%
     arrange(desc(screening_rate)) %>%
     select(study_site, screening_rate, aim_total_mean) %>%
     mutate(
         performance_rank = row_number(),
         aim_rank = rank(desc(aim_total_mean)),
         rank_difference = aim_rank - performance_rank
     ) %>%
     head(5)
 
 ## Analysis using the weekly screening rate database ---------------
 screening_weekly_clean <- screening_rate_weekly_with_baseline %>%
     filter(!is.na(aim_total_mean))
 ggplot(screening_weekly_clean, aes(x = week, y = screening_rate, color = study_site)) +
     geom_line(alpha = 0.7, size = 1) +
     geom_smooth(aes(group = study_site), method = "lm", se = FALSE, size = 0.5) +
     labs(title = "Weekly Screening Rates by Facility", 
          x = "Week", y = "Screening Rate (%)") +
     theme_minimal() +
     theme(legend.position = "none") +
     facet_wrap(~study_site, scales = "free_y")

 weekly_overall <- screening_weekly_clean %>%
     group_by(week) %>%
     summarise(
         mean_rate = mean(screening_rate, na.rm = TRUE),
         median_rate = median(screening_rate, na.rm = TRUE),
         n_facilities = n_distinct(study_site),
         .groups = 'drop'
     )
 
 ggplot(weekly_overall, aes(x = week, y = mean_rate)) +
     geom_line(size = 1.2, color = "steelblue") +
     geom_point(size = 2) +
     labs(title = "Average Weekly Screening Rate Across All Facilities", 
          x = "Week", y = "Mean Screening Rate (%)") +
     theme_minimal()
  
 facility_baseline_weekly <- screening_weekly_clean %>%
     group_by(study_site) %>%
     summarise(
         mean_weekly_rate = mean(screening_rate, na.rm = TRUE),
         median_weekly_rate = median(screening_rate, na.rm = TRUE),
         sd_weekly_rate = sd(screening_rate, na.rm = TRUE),
         aim_total_mean = first(aim_total_mean),
         iam_total_mean = first(iam_total_mean),
         fim_total_mean = first(fim_total_mean),
         .groups = 'drop'
     )
 cor.test(facility_baseline_weekly$mean_weekly_rate, facility_baseline_weekly$aim_total_mean)
 cor.test(facility_baseline_weekly$mean_weekly_rate, facility_baseline_weekly$iam_total_mean)
 cor.test(facility_baseline_weekly$mean_weekly_rate, facility_baseline_weekly$fim_total_mean)
 
 #multilevel analysis
 library(lme4)
 library(broom.mixed)
 
 # Multilevel model with random intercepts for facilities
 model_aim <- lmer(screening_rate ~ aim_total_mean + (1|study_site), 
                   data = screening_weekly_clean)
 
 model_fim <- lmer(screening_rate ~ fim_total_mean + (1|study_site), 
                   data = screening_weekly_clean)
 
 model_iam <- lmer(screening_rate ~ iam_total_mean + (1|study_site), 
                   data = screening_weekly_clean)
 
 model_combined <- lmer(screening_rate ~ aim_total_mean + iam_total_mean + fim_total_mean + (1|study_site), 
                        data = screening_weekly_clean)
 
 # Results
 tidy(model_aim, effects = "fixed")
 tidy(model_fim, effects = "fixed")
 tidy(model_iam, effects = "fixed")
 tidy(model_combined, effects = "fixed")
 
 # trend analysis
 screening_weekly_clean <- screening_weekly_clean %>%
     mutate(
         time_period = case_when(
             week_number <= 5 ~ "Early (Weeks 1-5)",
             week_number <= 10 ~ "Middle (Weeks 6-10)", 
             TRUE ~ "Late (Weeks 11+)"
         )
     )
 # Performance by time period and baseline scores
 time_analysis <- screening_weekly_clean %>%
     group_by(study_site, time_period, aim_total_mean) %>%
     summarise(mean_rate = mean(screening_rate, na.rm = TRUE), .groups = 'drop') %>%
     pivot_wider(names_from = time_period, values_from = mean_rate) %>%
     arrange(desc(aim_total_mean))
 
 print(time_analysis)
 
 # Simple visualization of trends by baseline acceptability
 ggplot(screening_weekly_clean, aes(x = week_number, y = screening_rate)) +
     geom_point(alpha = 0.3) +
     geom_smooth(method = "lm", se = TRUE) +
     facet_wrap(~cut(aim_total_mean, breaks = 3, labels = c("Low AIM", "Medium AIM", "High AIM"))) +
     labs(title = "Screening Rate Trends by Baseline Acceptability Level",
          x = "Week Number", y = "Screening Rate (%)") +
     theme_minimal()
 
 # Calculate improvement over time
 trajectory_analysis <- screening_weekly_clean %>%
     group_by(study_site) %>%
     summarise(
         aim_total_mean = first(aim_total_mean),
         early_rate = mean(screening_rate[week_number <= 5], na.rm = TRUE),
         late_rate = mean(screening_rate[week_number >= 11], na.rm = TRUE),
         improvement = late_rate - early_rate,
         improvement_pct = ((late_rate - early_rate) / early_rate) * 100,
         .groups = 'drop'
     ) %>%
     arrange(desc(aim_total_mean))
 
 print(trajectory_analysis)

 # Visualize the trajectory patterns
 ggplot(trajectory_analysis, aes(x = aim_total_mean, y = improvement)) +
     geom_point(size = 4, alpha = 0.7) +
     geom_smooth(method = "lm", se = TRUE) +
     geom_text_repel(aes(label = paste0(substr(study_site, 1, 15), 
                                        "\n(+", round(improvement, 1), "%)")), 
                     size = 3) +
     labs(title = "The Learning Curve Paradox", 
          subtitle = "Higher baseline confidence ??? Bigger improvement over time",
          x = "Baseline Acceptability (AIM)", 
          y = "Improvement from Early to Late Period (%)") +
     theme_minimal()
 
 # Create trajectory plot
 screening_weekly_clean %>%
     mutate(confidence_level = case_when(
         aim_total_mean >= 4.8 ~ "High Confidence (???4.8)",
         aim_total_mean >= 4.6 ~ "Medium Confidence (4.6-4.79)", 
         TRUE ~ "Realistic Confidence (<4.6)"
     )) %>%
     group_by(confidence_level, week_number) %>%
     summarise(mean_rate = mean(screening_rate, na.rm = TRUE), .groups = 'drop') %>%
     ggplot(aes(x = week_number, y = mean_rate, color = confidence_level)) +
     geom_line(size = 1.5) +
     geom_point(size = 2) +
     labs(title = "Performance Trajectories by Baseline Confidence Level",
          x = "Week Number", y = "Mean Screening Rate (%)",
          color = "Baseline Confidence") +
     theme_minimal()

 # analysis using daily screening rate database ---------------
 # Filter daily data for facilities with baseline measures
 daily_analysis <- screening_rate_with_baseline %>%
     filter(!is.na(aim_total_mean))
 
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
 
#### analysis for the conference abstract --------------
 # Use raw day numbers for easier interpretation
 daily_trends <- daily_analysis %>%
     arrange(study_site, day) %>%
     group_by(study_site) %>%
     mutate(day_number = row_number()) %>%
     ungroup()
 
 # Simple main effects models (most interpretable)
 daily_model_basic <- lmer(screening_rate ~ day_number + (1|study_site), 
                           data = daily_trends)
 
 daily_model_aim <- lmer(screening_rate ~ day_number + aim_total_mean + (1|study_site), 
                         data = daily_trends)
 
 daily_model_iam <- lmer(screening_rate ~ day_number + iam_total_mean + (1|study_site), 
                         data = daily_trends)
 
 daily_model_fim <- lmer(screening_rate ~ day_number + fim_total_mean + (1|study_site), 
                         data = daily_trends)
 # Use raw day numbers for easier interpretation
daily_trends <- daily_analysis %>%
  arrange(study_site, day) %>%
  group_by(study_site) %>%
  mutate(day_number = row_number()) %>%
  ungroup()

# Individual models
daily_model_basic <- lmer(screening_rate ~ day_number + (1|study_site), 
                          data = daily_trends)

daily_model_aim <- lmer(screening_rate ~ day_number + aim_total_mean + (1|study_site), 
                        data = daily_trends)

daily_model_iam <- lmer(screening_rate ~ day_number + iam_total_mean + (1|study_site), 
                        data = daily_trends)

daily_model_fim <- lmer(screening_rate ~ day_number + fim_total_mean + (1|study_site), 
                        data = daily_trends)

# Combined model - This is key!
daily_model_combined <- lmer(screening_rate ~ day_number + aim_total_mean + iam_total_mean + fim_total_mean + (1|study_site), 
                             data = daily_trends)

# Model comparisons
anova(daily_model_basic, daily_model_aim)
anova(daily_model_basic, daily_model_iam) 
anova(daily_model_basic, daily_model_fim)
anova(daily_model_basic, daily_model_combined)

# See which constructs survive in the combined model
summary(daily_model_combined)
tidy(daily_model_combined, effects = "fixed")