# Header ------------------------------------------------------------------

# Author(s): David
# Date: March 13, 2025
# Enrollment Progress Script

# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")

#### Setting plot themes to format graphs
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))
#--------------------------------------------------------------------------


## Figure 1: Enrollment progress since the beginning of the study to today
enrollment_progress <- screening_consent_df %>% 
    filter(rct_enrolling == "Yes") %>% 
    select(study_site, consent_date_auto)

## Convert consent_date_auto column to date format
enrollment_progress <- enrollment_progress %>%
    mutate(consent_date_auto = as.Date(consent_date_auto, origin = "1899-12-30"))

# Aggregate weekly enrollments per site
weekly_count <- enrollment_progress %>%
    mutate(week = floor_date(consent_date_auto, "week", week_start = 1)) %>%  # Group by week
    group_by(study_site, week) %>%
    summarise(enrollment_count = n(), .groups = "drop")  # Count enrollments

# Define the sequence of weekly dates (assuming enrollment started on 2025-02-17)
date_seq <- seq(as.Date("2025-02-16"), as.Date("2025-03-29"), by = "week")

# Convert to a dataframe
dateSeq_df <- data.frame(week = date_seq)

weekly_enrollment <- full_join(weekly_count, dateSeq_df, by = "week", week_start = 1) %>%
    arrange(week) %>%  # Ensure weeks are in order
    mutate(enrollment_count = ifelse(is.na(enrollment_count), 
                                     0, enrollment_count)) %>%   # Fill missing counts with 0
    filter(!is.na(study_site))

# Compute cumulative enrollment per site
weekly_enrollment <- weekly_enrollment %>%
    group_by(study_site) %>%
    mutate(cumulative_enrollment = cumsum(enrollment_count)) %>%
    ungroup()


figure_1 <- ggplot(weekly_enrollment, aes(x = week, y = cumulative_enrollment, color = study_site, group = study_site)) +
    geom_line(size = 1) +  # Line plot for trends
    geom_point(size = 2) +  # Add points for clarity
    labs(title = "Weekly Enrollment Trends by Study Site",
         x = "Week",
         y = "Number of Enrollments",
         color = "Study Site") +
    theme_minimal() +
    theme(legend.position = "right")  # Adjust legend placement

figure_1

## Weekly enrollment table by Facility
Enrollment_wide <- weekly_enrollment %>%
    select(study_site, week, enrollment_count) %>% 
    pivot_wider(
        names_from = week,          # Convert weeks into column names
        values_from = enrollment_count,  # Fill values with enrollment counts
        values_fill = list(enrollment_count = 0)  # Fill missing values with 0
    ) %>% 
    adorn_totals(where = c("row","col")) %>% 
    arrange(desc(Total))
pacman::p_load(dplyr, tidyr, janitor, flextable)

# Convert to flextable and format totals
Enrollment_wide_ft <- Enrollment_wide %>%
    flextable() %>%
        bold(part = "body", i = 1) %>%  # Bold column total
    italic(part = "body", i = 1) %>%  # Italicize column total
    autofit()  # Adjust table to fit content

Enrollment_wide_ft

