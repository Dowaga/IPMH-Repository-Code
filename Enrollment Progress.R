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
    filter(rct_enrolling == "Yes"| latest_consent == 
               'Yes') %>% 
    select(study_site, consent_date_auto, consent_date_auto_v2)


## 
enrollment_progress <- enrollment_progress %>%
    mutate(
        consent_date_auto = coalesce(consent_date_auto, consent_date_auto_v2)
    ) %>%
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
dateSeq_df <- data.frame(week = seq(as.Date("2025-02-16"), as.Date("2026-05-11"), by = "week"))


weekly_enrollment <- full_join(weekly_count, dateSeq_df, by = "week") %>%
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
    geom_line(linewidth = 1) +  # Line plot for trends
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


# Convert to flextable and format totals
Enrollment_wide_ft <- Enrollment_wide %>%
    flextable() %>%
        bold(part = "body", i = 1) %>%  # Bold column total
    italic(part = "body", i = 1) %>%  # Italicize column total
    autofit()  # Adjust table to fit content

Enrollment_wide_ft

# Date when 25% of the overall enrollment was met
total_enrollment <- weekly_enrollment %>%
    group_by(week) %>%
    summarise(enrollment_count = sum(enrollment_count)) %>%
    arrange(week) %>%
    mutate(cumulative_enrollment = cumsum(enrollment_count)) %>%
    mutate(study_site = "Total")

target_reached_25 <- total_enrollment %>%
    mutate(week = format(as.Date(week), "%b %d, %Y"))%>% 
    filter(cumulative_enrollment >= 743) %>%
    slice(1) %>%
    pull(week) 

target_reached_50 <- total_enrollment %>%
    mutate(week = format(as.Date(week), "%b %d, %Y"))%>% 
    filter(cumulative_enrollment >= 1486) %>%
    slice(1) %>%
    pull(week)

target_reached_75 <- total_enrollment %>%
    mutate(week = format(as.Date(week), "%b %d, %Y"))%>% 
    filter(cumulative_enrollment >= 2229) %>%
    slice(1) %>%
    pull(week)

# Enrollments ----
## Summary of Study Enrollment by Facility----
Enrollments <- screening_consent_df %>%
    group_by(study_site) %>%
    reframe(Screened = n(),
            `Eligible` = sum(rct_eligible == 1),
            `Enrolled` = sum(rct_enrolling == "Yes", na.rm = "TRUE"),
            `% Enrolled (of Eligible)` = round((Enrolled / Eligible) * 100)) %>%
    # Remove the facility code
    mutate(study_site = gsub("^[0-9]+,\\s*", "", study_site)) %>% 
    rename(`Facility` = study_site)  %>%
    bind_rows(summarise(.,Facility = "Overall",
                        Screened = sum(Screened),
                        Eligible = sum(Eligible),Enrolled = sum(Enrolled),
                        `% Enrolled (of Eligible)` = ifelse(sum(Eligible) > 0, round((sum(Enrolled) / sum(Eligible)) * 100, 1), NA)))  %>%
    arrange(desc(`Enrolled`))%>%
    gt() %>%
    tab_header(title = "Summary of Study Enrollment by Facility",subtitle = "Showing Participants enrollments") %>%
    tab_style(style = cell_text(weight = "bold"),locations = cells_body(rows = Facility == "Overall")) %>%
    tab_options(table.font.size = px(12))

## Enrollment and HIV Summary by Facility----
Enrollment_summary <- ppw_rct_df %>%
    group_by(clt_ptid) %>%
    summarise(
        study_site = first(clt_study_site),
        
        # Total enrolled (everyone in dataset)
        enrolled = 1,
        
        # HIV at baseline (adjust variable if needed)
        hiv_positive = any(med_pastdiag___2 == "Checked", na.rm = TRUE),
        
        # Zero conversion at any visit
        zero_converted = any(hivct_newdiag == "Yes", na.rm = TRUE)
    ) %>%
    ungroup()%>%
    # Remove the facility code
    mutate(study_site = gsub("^[0-9]+,\\s*", "", study_site))

facility_summary <- Enrollment_summary %>%
    group_by(study_site) %>%
    summarise(
        `Total Enrolled` = n(),
        `Enrolled with HIV` = sum(hiv_positive, na.rm = TRUE),
        `Zero-converted` = sum(zero_converted, na.rm = TRUE)
    ) %>%
    ungroup() %>% 
    arrange(desc(`Total Enrolled`)) %>% 
    adorn_totals() %>%
    gt() %>%
    tab_header(
        title = "Enrollment and HIV Summary by Facility",
        subtitle = "Includes Sero Conversion Across All Visits"
    ) %>%
    cols_label(
        study_site = "Facility"
    ) %>%
    fmt_number(
        columns = -study_site,
        decimals = 0
    ) %>%
    tab_options(
        table.font.size = gt::px(12),
        data_row.padding = gt::px(2)
    )



# HIV status data quality check----
check_df <- ppw_rct_df %>%
    group_by(clt_ptid) %>%
    summarise(
        study_site = first(clt_study_site),
        
        # HIV positive at baseline
        hiv_positive_baseline = any(med_pastdiag___2 == "Checked" & clt_visit == "Enrollment", na.rm = TRUE),
        
        # Sero-converted at any visit
        sero_converted_any = any(hivct_newdiag == "Yes", na.rm = TRUE)
    ) %>%
    ungroup()

inconsistent_cases <- check_df %>%
    filter(hiv_positive_baseline & sero_converted_any)




