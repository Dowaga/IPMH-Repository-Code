# Header ------------------------------------------------------------------

# Author(s): Owaga & Yuwei
# Date: Apr 10, 2025
# Consort Diagram for weekly report

# Setup ------------------------------------------------------------------------

rm(list = ls())
# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")

#data prep ----------------
# Summarise ANC attendees and extract the sum as a numeric value
Attendees <- daily_closeout_df %>%
    summarise(`anc attendees` = sum(rct_anc_number, na.rm = TRUE)) %>%
    pull(`anc attendees`)  # Extract the sum value as a number

# Create a new data frame with anc_count rows, value = "Yes"
anc_attendees_df <- data.frame(
    anc_id = paste0("anc_", seq_len(Attendees)),
    anc_attendees = rep("Yes", Attendees)
)

#Study participants in the PM+ Survey
pm_abstractions <- pm_survey_df %>%
    mutate(pm_ptid = as.numeric(pm_ptid)) %>%       # if needed
    filter(ipmh_participant == "Yes") %>%
    distinct(pm_ptid, .keep_all = TRUE)

pm_df <- pm_abstractions %>% 
    select(pm_ptid, ipmh_participant)

# telepsy
telepsych <- telepsych %>%
    mutate(tele_ancid = if_else(row_number() == 1, 
                                tele_ancid[4], tele_ancid))


telepsych_df <- telepsych %>%
    filter(pt_attend == "Yes") %>%               # keep only attended
    arrange(tele_ancid, tele_date) %>%            # sort by person and date
    group_by(tele_ancid) %>%
    slice(1) %>%                                 # keep first record per person
    ungroup()

#now match this with consenting database to get their pt_id
tele_with_id <- telepsych_df %>%
    left_join(
        rct_ppw_consenting %>% 
            select(anc_num, partipant_id),
        by = c("tele_ancid" = "anc_num")
    )

tele_with_id <- tele_with_id %>%
    select(tele_ancid, partipant_id) %>% 
    mutate(tele = "Yes") 

# merging data
consort_data <- screening_consent_df %>% 
    select(record_id, partipant_id, rct_harm_thought, rct_aud_hallucinations, 
           rct_vis_hallucinations, rct_paranoia,rct_delusions,
           rct_memory_problem,rct_eligible_gestation, rct_eligible_harm, 
           rct_risk, rct_eligible, rct_enrolling, rct_decline_reason, 
           rct_other_reasons) %>% 
    mutate(
        eligible = case_when(
            rct_eligible == 1 ~ "1",
            TRUE ~ NA_character_)) %>% 
    mutate(exclusion = case_when(
        rct_eligible == 0 & rct_eligible_gestation == "No" ~ "Gestation <20 Weeks",
        rct_harm_thought == "Yes" & rct_memory_problem == "No" ~"Self harm",
        rct_harm_thought == "Yes" & rct_memory_problem == "Yes" ~"Self harm and memory problem",
        rct_eligible == 0 & rct_aud_hallucinations == "Yes" ~ "Hearing voices that others cannot hear",
        rct_memory_problem == "Yes" ~"Memory problem",
        rct_delusions == "Yes" ~ "Holding unusual beliefs",
        rct_paranoia == "Yes" ~ "Feels watched/followed",
                TRUE ~ NA_character_)) %>% 
    mutate(
        exclusion = case_when(
            eligible == 1 & exclusion == "Self harm" & rct_risk %in% c("Low", "Moderate") ~ NA_character_,
            TRUE ~ exclusion
              )) %>% 
    mutate(facility = as.numeric(str_sub(partipant_id, 3, 4))) %>% 
    mutate(arm = case_when(
        facility %in% c(2, 5, 6, 8, 11, 14, 15, 18, 20, 21) ~ "Control",
        TRUE ~ "Intervention"
    )) %>% 
    mutate(rct_decline_reason = case_when(
            rct_decline_reason == "Other (specify) ___" ~ "Relocate post delivery",
            TRUE ~ rct_decline_reason  # Keep other values unchanged
        )) 

# Merge consort data with pm_df
consort_data <- consort_data %>% 
    left_join(pm_df, by = c("partipant_id"="pm_ptid"))

consort_data <- bind_rows(anc_attendees_df, consort_data)

elig <- consort_data %>% 
    filter(is.na(eligible)) %>% 
    filter(is.na(exclusion))

decline_reason <- consort_data %>% 
    filter(!is.na(rct_decline_reason))

secondvisit <- ppw_rct_df %>% 
    filter(clt_visit == "6 weeks post-partum") %>% 
    select(clt_ptid) %>% 
    mutate(secondvisit = "Yes") 

thirdvisit <- ppw_rct_df %>% 
    filter(clt_visit == "14 weeks post-partum") %>% 
    select(clt_ptid) %>% 
    mutate(thirdvisit = "Yes") 


fourthvisit <- ppw_rct_df %>% 
    filter(clt_visit == "6 months post-partum") %>% 
    select(clt_ptid) %>% 
    mutate(fourthvisit = "Yes") 

# merge secondvisit people into consort_data
consort_data <- consort_data %>% 
    left_join(secondvisit, by = c("partipant_id" = "clt_ptid")) 

consort_data <- consort_data %>% 
    left_join(tele_with_id, by = c("partipant_id" = "partipant_id")) 


#merge thirdvisit people into consort_data
consort_data <- consort_data %>% 
    left_join(thirdvisit, by = c("partipant_id" = "clt_ptid"))

# merge fourthvisit people into consort_data
consort_data <- consort_data %>% 
    left_join(fourthvisit, by = c("partipant_id" = "clt_ptid"))

# Create a dummy arm for DSMB Closed report
consort_data <- consort_data %>% 
    mutate(
        dummy_arm = case_when(
            is.na(arm) ~ NA_character_,        # keep NA as NA
            grepl("Control", arm) ~ "Arm X",   # if arm contains "Control"
            TRUE ~ "Arm Y"                     # everything else
        ))
#generate consort diagram without percentages (also without telepsychiatry)
consort_diagram <- consort_plot(data = consort_data,
                    orders = c(anc_attendees = "ANC Attendees",
                               arm = "Assessed for Eligibility",
                        #record_id = "Assessed for Eligibility",
                               exclusion = "Excluded",
                               eligible = "Eligible",
                               rct_decline_reason = "Declined Enrollment",
                               rct_enrolling = "Enrolled",
                        ipmh_participant = "PM+",
                        tele = "Telepsychiatry",
                        secondvisit = "6 weeks postpartum visit",
                        thirdvisit = "14 Weeks Postpartum Visit",
                        fourthvisit = "6 Months Postpartum Visit"),
                    side_box = c("exclusion", "rct_decline_reason"),
                    allocation = "arm")
consort_diagram

#### HEAD
library(grid)
options(txt_gp = gpar(cex = 1.2)) 

#### HEAD
txt_anc <- c("ANC Attendees (n=4,465)")
txt_ass <- c("Assessed for Eligibility\n (n=1,098, 24.59%)") 
txt_arm <- c("Control\n (n=568, 51.73%)", "Intervention\n (n=530, 48.27%)")
txt_ex_control <- "Excluded (n=207, 36.44%):\n\u2022 Gestation < 28 weeks (n=190, 91.79%)\n\u2022 Hearing voices that others cannot hear (n=3, 1.45%)\n\u2022 Holding unusual beliefs (n=4, 1.93%)\n\u2022 Memory problem (n=3, 1.45%)\n\u2022 Self harm (n=5, 2.42%)\n\u2022 Self harm and memory problem (n=2, 0.97%)"
txt_ex_intervention <- "Excluded (n=224, 42.26%):\n\u2022 Gestation < 28 weeks (n=219, 97.77%)\n\u2022 Memory problem (n=1, 0.45%)\n\u2022 Self harm (n=4, 1.79%)"
txt_ex <- c(txt_ex_control, txt_ex_intervention)
txt_eli <- c("Eligible\n (n=360, 63.38%)", "Eligible\n (n=306, 57.74%)")
txt_decline_control <- "Declined Enrollment (n=20, 5.56%)\n\u2022 Consult spouse (n=2, 10.00%)\n\u2022 Not enough time (n=4, 20.00%)\n\u2022 Not interested (n=5, 25.00%)\n\u2022 Relocate post delivery (n=6, 30.00%)\n\u2022 Time to think about it (n=3, 15.00%)"
txt_decline_intervention <- "Declined Enrollment (n=13, 4.25%)\n\u2022 Not enough time (n=5, 38.46%)\n\u2022 Relocate post delivery (n=7, 58.85%)\n\u2022 Time to think about it (n=1, 7.69%)"
txt_decline <- c(txt_decline_control, txt_decline_intervention)
txt_enrol <- c("Enrolled (n=340, 94.44%)", "Enrolled (n=293, 95.75%)")

      
##### 3726121ecb0f3c621a7bf2a3a4198199f4de42db
               
txt_pm <- c("Study Nurse PM+ Yields\n (n=0, 0%)", 
            "Study Nurse PM+ Yields\n (n=65, 23.55%)")

#####
#automatically adding percentages (for weekly reports) ----------------
# Total ANC attendees
n_attendees <- consort_data %>% 
    filter(anc_attendees == "Yes") %>% 
    nrow()

# Assessed for eligibility (non-NA arm)
n_assessed <- consort_data %>% 
    filter(!is.na(arm)) %>% 
    nrow()

# By arm
counts_by_arm <- consort_data %>%
    filter(!is.na(arm)) %>%
    group_by(arm) %>%
    summarise(
        assessed = n(),
        excluded = sum(!is.na(exclusion)),
        eligible = sum(eligible == 1, na.rm = TRUE),
        enrolled = sum(rct_enrolling == "Yes", na.rm = TRUE),
        pm_participants = sum(ipmh_participant == "Yes", na.rm = TRUE),
        tele_participants = sum(tele == "Yes", na.rm = TRUE),
        postpartum_visit = sum(secondvisit == "Yes", na.rm = TRUE),
        postpartum_visit1 = sum(thirdvisit == "Yes", na.rm = TRUE),
        postpartum_visit2 = sum(fourthvisit == "Yes", na.rm = TRUE)
    )

# Function to generate stage text
generate_stage_text <- function(stage_label, numerator, denominator, decimals = 2) {
    percent <- ifelse(denominator > 0, (numerator / denominator) * 100, 0)
    sprintf("%s\n (n=%d, %.*f%%)", stage_label, numerator, decimals, percent)
}

# Apply to each arm
counts_by_arm <- counts_by_arm %>%
    rowwise() %>%
    mutate(
        txt_eligible = generate_stage_text("Eligible", eligible, assessed),
        txt_excluded = generate_stage_text("Excluded", excluded, assessed),
        txt_enrolled = generate_stage_text("Enrolled", enrolled, eligible),
        txt_pm = generate_stage_text("PM+", pm_participants, enrolled),
        txt_tele = generate_stage_text("Telepsychiatry", tele_participants, enrolled),
        txt_postpartum = generate_stage_text("6 weeks postpartum visit", postpartum_visit, enrolled),
        txt_postpartum1 = generate_stage_text("14 weeks postpartum visit", postpartum_visit1, enrolled),
        txt_postpartum2 = generate_stage_text("6 months postpartum visit", postpartum_visit2, enrolled)
    )

# Total text for ANC attendees
txt_anc <- sprintf("ANC Attendees (n=%d)", n_attendees)

# Total assessment
txt_ass <- sprintf("Assessed for Eligibility\n (n=%d, %.1f%%)", n_assessed, (n_assessed / n_attendees) * 100)

txt_arm <- counts_by_arm %>%
    mutate(
        txt = sprintf("%s\n (n=%d, %.1f%%)", arm, assessed, (assessed / n_assessed) * 100)
    ) %>%
    pull(txt)

#combine PM+ and telepsychiatry
counts_by_arm <- counts_by_arm %>%
    mutate(
        txt_pm_tele = paste(txt_pm, txt_tele, sep = "\n")
    )

generate_box_header <- function(label, numerator, denominator, decimals = 2) {
percent <- ifelse(denominator > 0, (numerator / denominator) * 100, 0)
sprintf("%s (n=%d, %.*f%%):", label, numerator, decimals, percent)
}

# Build exclusion string per arm
exclusion_side_boxes <- consort_data %>%
    filter(!is.na(exclusion)) %>%
    group_by(arm, exclusion_reason = exclusion) %>%
    summarise(count = n(), .groups = "drop") %>%
    left_join(
        counts_by_arm %>% select(arm, excluded, assessed),
        by = "arm"
    ) %>%
    group_split(arm) %>%
    lapply(function(df_arm) {
        arm_label <- unique(df_arm$arm)
        excluded <- unique(df_arm$excluded)
        assessed <- unique(df_arm$assessed)
        
        # Header line (no real line break)
        header <- generate_box_header("Excluded", excluded, assessed)
        
        # Bullet lines (keep \\n as character)
        reason_lines <- mapply(
            function(reason, count) {
                sprintf("\u2022 %s (n=%d, %.1f%%)", reason, count, (count / excluded) * 100)
            },
            df_arm$exclusion_reason,
            df_arm$count
        )
        
        # Combine header + bullets with literal \n characters
        box_text <- paste(c(header, reason_lines), collapse = "\n")
        
        box_text
    })

# Combine into character vector
txt_ex <- unlist(exclusion_side_boxes)

# Build decline side boxes per arm
decline_side_boxes <- consort_data %>%
    filter(!is.na(rct_decline_reason)) %>%
    group_by(arm, decline_reason = rct_decline_reason) %>%
    summarise(count = n(), .groups = "drop") %>%
    left_join(
        counts_by_arm %>%
            mutate(declined = eligible - enrolled) %>%
            select(arm, declined, eligible),
        by = "arm"
    ) %>%
    group_split(arm) %>%
    lapply(function(df_arm) {
        arm_label <- unique(df_arm$arm)
        declined <- unique(df_arm$declined)
        eligible <- unique(df_arm$eligible)
        
        # Header line (with real line break later)
        header <- sprintf("Declined Enrollment (n=%d, %.1f%%):", declined, (declined / eligible) * 100)
        
        # Bullet lines with Unicode bullets and percentages
        reason_lines <- mapply(
            function(reason, count) {
                sprintf("\u2022 %s (n=%d, %.1f%%)", reason, count, (count / declined) * 100)
            },
            df_arm$decline_reason,
            df_arm$count
        )
        
        # Combine header + bullets with **real line breaks**
        box_text <- paste(c(header, reason_lines), collapse = "\n")
        
        box_text
    })

# Combine into character vector
txt_decline <- unlist(decline_side_boxes)

# 14d834c07edb977b556fed4c00a6c156b7d65458

consort_per <- add_box(txt = txt_anc) |>
    add_box(txt = txt_ass) |>
    add_split(txt = txt_arm) |>
    add_side_box(txt = txt_ex) |>
    add_box(txt = counts_by_arm$txt_eligible) |>
    add_side_box(txt = txt_decline) |>
    add_box(txt = counts_by_arm$txt_enrolled) |>
    add_side_box(txt= counts_by_arm$txt_pm_tele) |>
    add_box(txt = counts_by_arm$txt_postpartum)|>
    add_box(txt = counts_by_arm$txt_postpartum1)|>
    add_box(txt = counts_by_arm$txt_postpartum2)

consort_per


## consort diagram without arm breaking--------
# Total ANC attendees
n_attendees <- consort_data %>% filter(anc_attendees == "Yes") %>% nrow()

# Total assessed (non-NA arm)
n_assessed <- consort_data %>% filter(!is.na(arm)) %>% nrow()

# Total excluded
n_excluded <- consort_data %>% 
    filter(!is.na(exclusion)) %>% 
    nrow()


# Total eligible
n_eligible <- consort_data %>% 
    filter(eligible == 1) %>% 
    nrow()

# Total enrolled
n_enrolled <- consort_data %>% 
    filter(rct_enrolling == "Yes")%>% 
    nrow()

# Total PM+ participants
n_pm <- consort_data %>% 
    filter(ipmh_participant == "Yes") %>% 
    nrow()

# Total telepsychiatry participants
n_tele <- consort_data %>% 
    filter(tele == "Yes") %>% 
    nrow()

# Total postpartum visits
w6_postpartum <- consort_data %>%
         filter(rct_enrolling == "Yes", secondvisit == "Yes") %>%
         nrow()
w14_postpartum <- consort_data %>%
    filter(rct_enrolling == "Yes", thirdvisit == "Yes") %>%
    nrow()
m6_postpartum <- consort_data %>%
    filter(rct_enrolling == "Yes", fourthvisit == "Yes") %>%
    nrow()

# ANC attendees (no percentage)
txt_anc <- sprintf("ANC Attendees (n=%d)", n_attendees)

# Assessed
txt_ass <- sprintf("Assessed for Eligibility\n (n=%d, %.1f%%)", n_assessed, (n_assessed / n_attendees) * 100)

# Eligible
txt_eligible <- sprintf("Eligible\n (n=%d, %.1f%%)", n_eligible, (n_eligible / n_assessed) * 100)

# Excluded
txt_excluded <- sprintf("Excluded\n (n=%d, %.1f%%)", n_excluded, (n_excluded / n_assessed) * 100)

# Enrolled
txt_enrolled <- sprintf("Enrolled\n (n=%d, %.1f%%)", n_enrolled, (n_enrolled / n_eligible) * 100)

# PM+
txt_pm <- sprintf("PM+\n (n=%d, %.1f%%)", n_pm, (n_pm / n_enrolled) * 100)

#telepsychiatry
txt_tele <- sprintf("Telepsychiatry\n (n=%d, %.1f%%)", 10, (10 / n_enrolled) * 100)

# Postpartum
six_weeks_postpartum <- sprintf("6 weeks postpartum visit\n (n=%d, %.1f%%)", w6_postpartum, (w6_postpartum / n_enrolled) * 100)
fourteen_weeks_postpartum <- sprintf("14 weeks postpartum visit\n (n=%d, %.1f%%)", w14_postpartum, (w14_postpartum / n_enrolled) * 100)
six_months_postpartum <- sprintf("6 months postpartum visit\n (n=%d, %.1f%%)", m6_postpartum, (m6_postpartum / n_enrolled) * 100)


#combining pm+ and telepsychiatry
txt_pm_tele <- paste(txt_pm, txt_tele, sep = "\n")

#exclusion side box
exclusion_summary <- consort_data %>%
    filter(!is.na(exclusion)) %>%
    group_by(exclusion_reason = exclusion) %>%
    summarise(count = n(), .groups = "drop")

header_ex <- sprintf("Excluded (n=%d, %.1f%%):", n_excluded, (n_excluded / n_assessed) * 100)

reason_lines_ex <- mapply(
    function(reason, count) {
        sprintf("\u2022 %s (n=%d, %.1f%%)", reason, count, (count / n_excluded) * 100)
    },
    exclusion_summary$exclusion_reason,
    exclusion_summary$count
)

txt_ex <- paste(c(header_ex, reason_lines_ex), collapse = "\n")


#decline side box

n_declined <- n_eligible - n_enrolled

decline_summary <- consort_data %>%
    filter(!is.na(rct_decline_reason)) %>%
    group_by(decline_reason = rct_decline_reason) %>%
    summarise(count = n(), .groups = "drop")

header_decline <- sprintf("Declined Enrollment (n=%d, %.1f%%):", n_declined, (n_declined / n_eligible) * 100)

reason_lines_decline <- mapply(
    function(reason, count) {
        sprintf("\u2022 %s (n=%d, %.1f%%)", reason, count, (count / n_declined) * 100)
    },
    decline_summary$decline_reason,
    decline_summary$count
)

txt_decline <- paste(c(header_decline, reason_lines_decline), collapse = "\n")

#consort generation
consort_single <- add_box(txt = txt_anc) |>
    add_box(txt = txt_ass) |>
    add_side_box(txt = txt_ex) |>
    add_box(txt = txt_eligible) |>
    add_side_box(txt = txt_decline) |>
    add_box(txt = txt_enrolled) |>
    add_side_box(txt = txt_pm_tele) |>
    add_box(txt = six_weeks_postpartum)|>
    add_box(txt = fourteen_weeks_postpartum)|>
    add_box(txt = six_months_postpartum)

consort_single

#-------------------------------------------------------------------------------
#### Ineligibility Reasons
ineligibility_summary <- consort_data %>% 
    filter(!is.na(exclusion)) %>% 
    tbl_summary(
        sort = list(all_categorical() ~ "frequency"),  # Sort categorical levels by frequency in descending order
        include = c(exclusion),
        label = list(exclusion ~ "Reasons for Ineligibility")) %>%
    as_gt() %>% 
    # modify with gt functions
    gt::tab_header("Summary of Reasons for Study Ineligibility") %>% 
    gt::tab_options(
        table.font.size = "medium",
        data_row.padding = gt::px(1)) %>%
    tab_options(
        table.font.size = px(14))

ineligibility_summary

#### Ineligibility Reasons by Arm

arm_ineligibility_summary <- consort_data %>% 
    filter(!is.na(exclusion)) %>% 
    tbl_summary(by = dummy_arm,
        sort = list(all_categorical() ~ "frequency"),  # Sort categorical levels by frequency in descending order
        include = c(exclusion),
        label = list(exclusion ~ "Reasons for Ineligibility")) %>% 
    bold_labels() %>%
    #add_p() %>% 
    add_overall() %>% 
    # convert from gtsummary object to gt object
    as_gt() %>%
    # modify with gt functions
    gt::tab_header("Summary of Study Ineligibility by Arm") %>% 
    gt::tab_options(
        table.font.size = "medium",
        data_row.padding = gt::px(1)) %>%
    tab_options(
        table.font.size = px(14))

arm_ineligibility_summary

#-------------------------------------------------------------------------------

