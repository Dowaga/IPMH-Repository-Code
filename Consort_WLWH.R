# Header ------------------------------------------------------------------

# Author(s): Yuwei
# Date: May 4, 2025
# Consort Diagram for WLWH

# Setup ------------------------------------------------------------------------

# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
# source("Dependencies.R")
# source("data_import.R")

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
telepsych$tele_ancid[1] <- telepsych$tele_ancid[4] 

telepsych_df <- telepsych %>%
    filter(pt_attend == "Yes") %>%               # keep only attended
    arrange(tele_ancid, tele_date) %>%            # sort by person and date
    group_by(tele_ancid) %>%
    slice(1) %>%                                 # keep first record per person
    ungroup()

#now match this with consenting database to get their pt_id
tele_with_id <- telepsych_df %>%
    left_join(
        rct_ppw_consenting %>% select(anc_num, partipant_id),
        by = c("tele_ancid" = "anc_num")
    )
tele_with_id <- tele_with_id %>%
    select(tele_ancid, partipant_id) %>% 
    mutate(tele = "Yes") 

#merging data
consort_data <- screening_consent_df %>% 
    select(record_id, partipant_id, rct_harm_thought, rct_aud_hallucinations, 
           rct_vis_hallucinations, rct_paranoia,rct_delusions,
           rct_memory_problem,rct_eligible_gestation, rct_eligible_harm, 
           rct_eligible, rct_enrolling, rct_decline_reason, 
           rct_other_reasons) %>% 
    mutate(
        eligible = case_when(
            rct_eligible == 1 ~ "1",
            TRUE ~ NA_character_)) %>% 
    mutate(exclusion = case_when(
        rct_eligible == 0 & rct_eligible_gestation == "No" ~ "Gestation <28 Weeks",
        rct_harm_thought == "Yes" & rct_memory_problem == "No" ~"Self harm",
        rct_harm_thought == "Yes" & rct_memory_problem == "Yes" ~"Self harm and memory problem",
        rct_eligible == 0 & rct_aud_hallucinations == "Yes" ~ "Hearing voices that others cannot hear",
        rct_memory_problem == "Yes" ~"Memory problem",
        rct_delusions == "Yes" ~ "Holding unusual beliefs",
        TRUE ~ NA_character_)) %>% 
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

secondvisit <- ppw_rct_df %>% filter(
    clt_visit == "6 weeks post-partum") %>% 
    select(clt_ptid) %>% mutate(secondvisit = "Yes")

thirdvisit <- ppw_rct_df %>% 
    filter(clt_visit == "14 weeks post-partum") %>% 
    select(clt_ptid) %>% 
    mutate(thirdvisit = "Yes") 


fourthvisit <- ppw_rct_df %>% 
    filter(clt_visit == "6 months post-partum") %>% 
    select(clt_ptid) %>% 
    mutate(fourthvisit = "Yes") 

#merge secondvisit people into consort_data
consort_data <- consort_data %>% 
    left_join(secondvisit, by = c("partipant_id" = "clt_ptid")) 

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

# HIV
hiv <- ppw_rct_df %>% filter(
    med_pastdiag___2 == "Checked") %>% 
    select(clt_ptid) %>% mutate(WLWH = "Yes") 

# Merge HIV data into consort_data
consort_data <- consort_data %>% 
    left_join(hiv, by = c("partipant_id" = "clt_ptid"))

#merge telepsychiatry data
consort_data <- consort_data %>% 
    left_join(tele_with_id, by = c("partipant_id" = "partipant_id")) 

### binding
consort_data <- bind_rows(anc_attendees_df, consort_data)

elig <- consort_data %>% 
    filter(is.na(eligible)) %>% 
    filter(is.na(exclusion))

decline_reason <- consort_data %>% 
    filter(!is.na(rct_decline_reason))

#automatically adding percentages (for weekly reports) ----------------
# Total ANC attendees
n_attendees <- consort_data %>% filter(anc_attendees == "Yes") %>% nrow()

# Assessed for eligibility (non-NA arm)
n_assessed <- consort_data %>% filter(!is.na(arm)) %>% nrow()

# By arm
counts_by_arm <- consort_data %>%
    filter(!is.na(arm)) %>%
    group_by(arm) %>%
    summarise(
        assessed = n(),
        excluded = sum(!is.na(exclusion)),
        eligible = sum(eligible == 1, na.rm = TRUE),
        enrolled = sum(rct_enrolling == "Yes", na.rm = TRUE),
        hiv = sum(WLWH == "Yes", na.rm = TRUE),
        pm_participants = sum(ipmh_participant == "Yes" & WLWH == "Yes", na.rm = TRUE),
        tele_participants = sum(tele == "Yes" & WLWH == "Yes", na.rm = TRUE),
        six_weeks_visit = sum(secondvisit == "Yes" & WLWH == "Yes", na.rm = TRUE),
        fourteen_weeks_visit = sum(thirdvisit == "Yes" & WLWH == "Yes", na.rm = TRUE),
        six_months_visit = sum(fourthvisit == "Yes" & WLWH == "Yes", na.rm = TRUE)
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
        txt_hiv = generate_stage_text("HIV+ (WLWH)", hiv, enrolled),
        txt_pm = generate_stage_text("PM+", pm_participants, hiv),
        txt_tele = generate_stage_text("Telepsychiatry", tele_participants, hiv),
        txt_6weeks_postpartum = generate_stage_text("6 weeks postpartum visit", six_weeks_visit, hiv),
        txt_14weeks_postpartum = generate_stage_text("14 weeks postpartum visit", fourteen_weeks_visit, hiv),
        txt_6months_postpartum = generate_stage_text("6 months postpartum visit", six_months_visit, hiv)
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

consort_per <- add_box(txt = txt_anc) |>
    add_box(txt = txt_ass) |>
    add_split(txt = txt_arm) |>
    #add_side_box(txt = txt_ex) |>
    add_box(txt = counts_by_arm$txt_eligible) |>
    #add_side_box(txt = txt_decline) |>
    add_box(txt = counts_by_arm$txt_enrolled) |>
    add_box(txt = counts_by_arm$txt_hiv) |>
    add_side_box(txt= counts_by_arm$txt_pm_tele) |>
    add_box(txt = counts_by_arm$txt_6weeks_postpartum) %>% 
    add_box(txt = counts_by_arm$txt_14weeks_postpartum) %>% 
    add_box(txt = counts_by_arm$txt_6months_postpartum)


consort_per

## consort diagram without arm breaking--------
# Total ANC attendees
n_attendees <- consort_data %>% filter(anc_attendees == "Yes") %>% nrow()

# Total assessed (non-NA arm)
n_assessed <- consort_data %>% filter(!is.na(arm)) %>% nrow()

# Total excluded
n_excluded <- consort_data %>% filter(!is.na(exclusion)) %>% nrow()

# Total eligible
n_eligible <- consort_data %>% filter(eligible == 1) %>% nrow()

# Total enrolled
n_enrolled <- consort_data %>% filter(rct_enrolling == "Yes") %>% nrow()

n_hiv <- consort_data %>% filter(WLWH == "Yes") %>% nrow()

# Total PM+ participants
n_pm <- consort_data %>% filter(ipmh_participant == "Yes" & WLWH == "Yes") %>% nrow()

# Total telepsychiatry participants
n_tele <- consort_data %>% filter(tele == "Yes" & WLWH == "Yes") %>% nrow()

# Total postpartum visits
six_weeks <- consort_data %>%
    filter(secondvisit == "Yes" & WLWH == "Yes") %>%
    nrow()

fourteen_weeks <- consort_data %>%
    filter(thirdvisit == "Yes" & WLWH == "Yes") %>%
    nrow()

six_months <- consort_data %>%
    filter(fourthvisit == "Yes" & WLWH == "Yes") %>%
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

txt_hiv <- sprintf("HIV+ (WLWH)\n (n=%d, %.1f%%)", n_hiv, (n_hiv / n_enrolled) * 100)
# PM+
txt_pm <- sprintf("PM+\n (n=%d, %.1f%%)", n_pm, (n_pm / n_hiv) * 100)

# tele
txt_tele <- sprintf("Telepsychiatry\n (n=%d, %.1f%%)", n_tele, (n_tele / n_hiv) * 100)

# Postpartum
six_weeks_postpartum <- sprintf("6 weeks postpartum visit\n (n=%d, %.1f%%)", six_weeks, (six_weeks / n_hiv) * 100)
fourteen_weeks_postpartum <- sprintf("14 weeks postpartum visit\n (n=%d, %.1f%%)", fourteen_weeks, (fourteen_weeks / n_hiv) * 100)
six_months_postpartum <- sprintf("6 months postpartum visit\n (n=%d, %.1f%%)", six_months, (six_months / n_hiv) * 100)

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
    add_box(txt = txt_hiv) |>
    add_side_box(txt = txt_pm_tele) |>
    add_box(txt = six_weeks_postpartum)|>
    add_box(txt = fourteen_weeks_postpartum)|>
    add_box(txt = six_months_postpartum)

consort_single
