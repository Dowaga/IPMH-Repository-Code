# Header ------------------------------------------------------------------

# Author(s): Yuwei
# Date: Oct 4, 2026
# This is a script to prepare a cleaned HCW implementation variable dataset 
# that's ready for analysis, manuscript writing, and sharing 

# CHANGED: rerun on new export, Nov 25, 2026 (data team corrected DOBs, record 189,
# Miriu 12-month records and the Nyandiwa pair; see notes in each section)

# Setup ------------------------------------------------------------------------
rm(list = ls())

# Reference source codes & other dependencies:
source("Dependencies.R")
source("data_import.R")
source("DataTeam_ipmh.R")

# only keep relevant databases
rm(list = setdiff(ls(), c("screening_consent_hcw_df", 
                          "ipmh_filepath",
                          "hcw_is_df",
                          "data_freeze"))) 

# Helper: REDCap event label to wave number (0 = Pre-launch, 6, 12)
event_to_wave <- function(x) {
    case_when(str_detect(x, "^Pre-launch") ~ 0,
              str_detect(x, "^6-month")    ~ 6,
              str_detect(x, "^12-month")   ~ 12)
}

# Screening database ------------
## Here I only focused on the screening form and ignored all the QC forms.
screen_tidy <- screening_consent_hcw_df %>%
    filter(is.na(redcap_repeat_instrument)) %>%
    select(record_id, redcap_event_name,
           rct_study_staff:hcw_screening_complete) %>%
    # drop fields that were 100% empty in the variable profile
    select(-rct_job_other,
           -screen_enroll_n, -screen_enroll_n_other,
           -screen_enroll_dk, -screen_enroll_dk_other) %>%
    mutate(
        wave           = event_to_wave(redcap_event_name),
        site_id        = as.integer(str_extract(study_site, "^\\d+")),
        site_name      = str_trim(str_remove(study_site, "^\\d+,\\s*")),
        staff_initials = str_to_upper(str_remove_all(rct_study_staff, "[^A-Za-z0-9]")),
        cadre_code     = as.integer(str_extract(pt_type, "^\\d+")),
        across(starts_with("rct_dpt___"), ~ as.integer(.x == "Checked")),
        rct_dob           = ymd(rct_dob),
        screener_datetime = ymd_hms(screener_datetime),
        screen_date       = as_date(screener_datetime),
        
        # recomputed age at screening; rct_age_cal is not used (see notes)
        age_at_screen     = time_length(interval(rct_dob, screen_date), "years"),
        enroll = case_when(screen_enroll_yn == "Yes" ~ 1L,
                           screen_enroll_yn == "No"  ~ 0L),
        elig_recomputed = as.integer(rct_eligible_age == 1 &
                                         rct_eligible_cadre == 1 &
                                         rct_eligible_ipmh == 1)
    )

# Check response options before recoding
screening_consent_hcw_df %>%
    filter(redcap_repeat_instrument == "Consent Forms For Hcws") %>%
    count(hcw_participate, consent_literacy)

consent_tidy <- screening_consent_hcw_df %>%
    filter(redcap_repeat_instrument == "Consent Forms For Hcws") %>%
    transmute(
        record_id,
        wave         = event_to_wave(redcap_event_name),
        rct_pt_id,
        consented    = case_when(str_detect(hcw_participate, "^Yes") ~ 1L,
                                 !is.na(hcw_participate)             ~ 0L),
        consent_literacy,
        consent_date = ymd(hcw_date1),
        staff_date   = ymd(hcw_date_2),
        consent_auto = ymd_hm(hcw_date_auto),
        consent_form_status = consent_forms_for_hcws_complete
    )

screen_key <- screen_tidy %>%
    left_join(consent_tidy, by = c("record_id", "wave")) %>%
    mutate(id_match         = screen_id == rct_pt_id,
           days_scr_to_cons = as.numeric(consent_date - screen_date))

# Sanity check: should be 594 rows (200 + 198 + 196), no duplicates
nrow(screen_key)
screen_key %>% count(record_id, wave) %>% filter(n > 1)

# eligibility
screen_key %>% count(wave, agree = rct_eligible == elig_recomputed)

screen_key %>%
    count(wave, rct_eligible) %>%
    group_by(wave) %>%
    mutate(pct = round(100 * n / sum(n), 1)) %>%
    ungroup()

screen_key %>%
    filter(rct_eligible == 0) %>%
    count(wave,
          fails_age   = rct_eligible_age   == 0,
          fails_cadre = rct_eligible_cadre == 0,
          fails_ipmh  = rct_eligible_ipmh  == 0)

screen_key %>%
    count(rct_eligible_age, age_18plus = age_at_screen >= 18)

# enrollment & decline
screen_key %>%
    group_by(wave) %>%
    summarise(screened       = n(),
              eligible       = sum(rct_eligible == 1, na.rm = TRUE),
              enroll_yes     = sum(enroll == 1, na.rm = TRUE),
              enroll_no      = sum(enroll == 0, na.rm = TRUE),
              enroll_missing = sum(is.na(enroll)),
              consent_yes    = sum(consented == 1, na.rm = TRUE),
              .groups = "drop")

# Result (Nov 25, 2026): 594 screening rows (200 / 198 / 196), no duplicates.
# All eligible and all enrolled and consented;
# no refusals recorded.

# Demographics database (main data collection) ------------
## Keep IDs + the demographic form only.
## One row per HCW per wave where the demographic form was filled.
## Result (Oct 4, 2026): 340 forms; 200 at Pre-launch, 7 at 6-month
## (new entrants), 133 at 12-month. All forms Complete.

to01 <- function(x) as.integer(x %in% c("Checked", "1", 1))   # checkbox to 1/0

demog_df <- hcw_is_df %>%
    mutate(wave  = event_to_wave(redcap_event_name),
           arm   = str_extract(redcap_event_name, "(?<=: ).+(?=\\))"),
           pt_id = as.numeric(pt_id)) %>%
    select(part_id, redcap_event_name, wave, arm,
           pt_type, facility_id, consent_id, pt_id,
           dob_uk, dob, age, sex, education,
           dpt___1:dpt___7, dpt_other,
           starts_with("job_arm"), job_other,
           monthsjob, monthswork, terms, terms_other,
           care, monthscare, hrscare, wlwh,
           demographic_complete) %>%
    filter(if_any(c(dob_uk, dob, age, sex, education),
                  ~ !is.na(.x) & .x != ""))

nrow(demog_df)                                     # 340
demog_df %>% count(wave)                           # 200 / 7 / 133
demog_df %>% count(pt_id, wave) %>% filter(n > 1)  # 0 rows

## IDs, waves and arm ------------
## pt_id = cadre (2 digits) + facility (2 digits) + consent_id (4 digits)
## Arm: provider type must match arm; condition must match randomization;
## screening site must match facility.
intervention_fac <- c(1, 3, 4, 7, 9, 13, 16, 17, 19, 22)   # randomization list

id_arm_chk <- demog_df %>%
    mutate(
        cadre_code    = as.integer(str_extract(pt_type, "\\d+")),
        facility_code = as.integer(str_extract(facility_id, "^\\d+")),
        condition     = str_extract(arm, "^(Intervention|Control)"),
        arm_cadre     = case_when(str_detect(arm, "lay providers") ~ 22L,
                                  str_detect(arm, "in charge")     ~ 23L,
                                  str_detect(arm, "nurses/cos")    ~ 24L),
        condition_rand = case_when(
            facility_code %in% intervention_fac        ~ "Intervention",
            !is.na(facility_code) & facility_code != 0 ~ "Control"),
        id_ok  = pt_id %/% 1e6 == cadre_code &
            (pt_id %/% 1e4) %% 100 == facility_code &
            pt_id %% 1e4 == as.numeric(consent_id),
        arm_ok = arm_cadre == cadre_code & condition == condition_rand
    ) %>%
    left_join(screen_key %>% select(screen_id, wave, site_id),
              by = c("pt_id" = "screen_id", "wave")) %>%
    mutate(in_screening = !is.na(site_id),
           site_ok      = site_id == facility_code)

# Expect one row: all TRUE, n = 340
id_arm_chk %>% count(id_ok, arm_ok, in_screening, site_ok)

# Same arm across waves. Expect n_arm = 1 for all 217
id_arm_chk %>%
    group_by(pt_id) %>%
    summarise(n_arm = n_distinct(arm), .groups = "drop") %>%
    count(n_arm)

# Wave pattern of demographic forms per HCW
id_arm_chk %>%
    arrange(pt_id, wave) %>%
    group_by(pt_id) %>%
    summarise(pattern = paste(wave, collapse = "-"), .groups = "drop") %>%
    count(pattern)

## Result (Oct 4, 2026 - unchanged Nov 25, 2026): all IDs, arms and sites correct.
## Demographic form at each HCW's entry visit (200 / 7 / 10), plus a
## repeat at 12-month for 123 HCWs.

## Age ------------
## Sources:
##   main   : dob on the demographic form ("yyyy-mm-dd"; all 340 forms know DOB)
##   screen : rct_dob, re-entered at every screening visit
## Rule for dob_final:
##   (a) main and screening DOB at the ENTRY visit agree -> use that
##   (b) otherwise, the DOB recorded most often across all forms
##   (c) tie -> main DOB at entry
## Entry is preferred because two forms filled at the same visit agree.

### All DOBs recorded per HCW (one row per form) ------
dob_votes <- bind_rows(
    demog_df   %>% transmute(pt_id, wave, source = "main",   dob = ymd(dob, quiet = TRUE)),
    screen_key %>% transmute(pt_id = screen_id, wave, source = "screen", dob = rct_dob)
) %>%
    filter(!is.na(dob))

dob_votes %>% count(source)   # main = 340; screen = 593 

### Same visit: main vs screening ------
dob_same_visit <- dob_votes %>%
    pivot_wider(id_cols = c(pt_id, wave), names_from = source, values_from = dob) %>%
    filter(!is.na(main), !is.na(screen)) %>%
    mutate(status = case_when(
        main == screen               ~ "same",
        year(main) == year(screen)   ~ "day/month differs",
        TRUE                         ~ "year differs"))

dob_same_visit %>% count(wave, status)
## Result (Nov 25, 2026): 334/340 same; 3 day/month; 3 differ by
## >= 1 year (all at Pre-launch). 

### Entry visit per HCW ------
entry <- screen_key %>%
    arrange(screen_id, wave) %>%
    group_by(screen_id) %>%
    slice(1) %>%
    ungroup() %>%
    transmute(pt_id = screen_id, entry_wave = wave, entry_date = screen_date)

entry_dob <- dob_same_visit %>%
    semi_join(entry, by = c("pt_id", "wave" = "entry_wave")) %>%
    transmute(pt_id, entry_main = main, entry_scr = screen)

### Majority across all forms ------
dob_majority <- dob_votes %>%
    count(pt_id, dob, name = "votes") %>%
    group_by(pt_id) %>%
    summarise(n_candidates = n(),
              range_yrs    = as.numeric(max(dob) - min(dob)) / 365.25,
              top_dob      = dob[votes == max(votes)][1],
              tie          = sum(votes == max(votes)) > 1,
              .groups = "drop")

### Final DOB and age ------
dob_ref <- entry %>%
    left_join(entry_dob,    by = "pt_id") %>%
    left_join(dob_majority, by = "pt_id") %>%
    mutate(
        dob_final = case_when(entry_main == entry_scr ~ entry_main,
                              !tie                    ~ top_dob,
                              TRUE                    ~ entry_main),
        dob_rule  = case_when(entry_main == entry_scr ~ "entry forms agree",
                              !tie                    ~ "majority",
                              TRUE                    ~ "tie: main at entry"),
        dob_agreement = case_when(n_candidates == 1 ~ "all agree",
                                  range_yrs < 1     ~ "minor (< 1 year)",
                                  TRUE              ~ "major (>= 1 year)"),
        age_entry = time_length(interval(dob_final, entry_date), "years")
    )

nrow(dob_ref)                      # 217, one per HCW
sum(is.na(dob_ref$dob_final))      # 0
dob_ref %>% count(dob_rule)
dob_ref %>% count(dob_agreement)   # Nov 25: 209 all agree, 3 minor, 5 major
summary(dob_ref$age_entry)         # Nov 25: 22.9 to 59.3 (median 36.1)

### Screening forms with age only (no DOB): consistent with dob_final? ------
screen_key %>%
    filter(is.na(rct_dob)) %>%
    transmute(pt_id = screen_id, wave, rct_age, screen_date) %>%
    left_join(dob_ref %>% select(pt_id, dob_final), by = "pt_id") %>%
    mutate(age_from_final = floor(time_length(interval(dob_final, screen_date), "years")),
           diff = rct_age - age_from_final)
## Result (Nov 25, 2026): 23080125 diff -3

## 23080125: reported age 56 at 6m vs 59 from dob_final (1965-10-23, changed by
## data team from 1967-10-23). Own estimate supports 1967 -> query data team.

# Cadre and job ------------
## Cadre: fixed per HCW and encoded in pt_id (first 2 digits).
##   Section 1 verified main cadre = ID = arm for all 340 forms.
##   Here: screening cadre at ALL visits, and cadre vs job title.
## Job rule: screening job at entry (single choice, complete for all 217).
##   Main survey job is multi-select by arm; used only as a check.

### Cadre ------
# Screening cadre matches the ID at every visit. Expect all TRUE (594)
screen_key %>% count(cadre_ok = cadre_code == screen_id %/% 1e6)

# Cadre vs job title
screen_key %>% count(cadre_code, rct_job)
## Result (Oct 4, 2026): 22 = HTS counsellor / mentor mother only;
## 24 = nurse / clinical officer only; 23 (in-charges) = CO, nurse, MO.
## Fully consistent; no rule needed for cadre.

### Job: screening vs main at the same visit ------
## Match = screening job is among the jobs ticked in the main survey.
job_main <- demog_df %>%
    transmute(pt_id, wave, across(starts_with("job_arm"), to01)) %>%
    mutate(hts    = job_arm14___1,
           mm     = job_arm14___2,
           mo     = pmax(job_arm25___1, job_arm36___1),
           co     = pmax(job_arm25___3, job_arm36___2),
           nurse  = pmax(job_arm25___4, job_arm36___3),
           n_jobs = rowSums(across(starts_with("job_arm")))) %>%
    select(pt_id, wave, hts, mm, mo, co, nurse, n_jobs)

job_compare <- screen_key %>%
    select(pt_id = screen_id, wave, rct_job) %>%
    inner_join(job_main, by = c("pt_id", "wave")) %>%
    mutate(job_status = case_when(
        n_jobs == 0                                ~ "blank in main",
        rct_job == "HTS counsellor"   & hts   == 1 ~ "match",
        rct_job == "Mentor mother"    & mm    == 1 ~ "match",
        rct_job == "Medical officer"  & mo    == 1 ~ "match",
        rct_job == "Clinical Officer" & co    == 1 ~ "match",
        rct_job == "Nurse"            & nurse == 1 ~ "match",
        TRUE                                       ~ "mismatch"))

job_compare %>% count(wave, job_status)

## NEW (Nov 25): which HCWs have a job mismatch at entry?
job_compare %>%
    filter(job_status == "mismatch") %>%
    select(pt_id, wave, rct_job, hts, mm, mo, co, nurse)

swap_check_ids <- c(22110153, 22110154, 22060091, 22060093, 22060094)

full_join(
    screen_key %>% filter(screen_id %in% swap_check_ids) %>%
        transmute(pt_id = screen_id, wave, job_screen = rct_job),
    job_main %>% filter(pt_id %in% swap_check_ids) %>%
        transmute(pt_id, wave,
                  job_main = case_when(hts == 1 ~ "HTS counsellor", mm == 1 ~ "Mentor mother",
                                       mo == 1 ~ "Medical officer", co == 1 ~ "Clinical Officer",
                                       nurse == 1 ~ "Nurse", TRUE ~ NA_character_)),
    by = c("pt_id", "wave")) %>%
    arrange(pt_id, wave)

## Result (Nov 25, 2026): 335/340 match.
## 3 mismatches at Pre-launch, all swap records (new since Oct 4):
##   22110153 / 22110154: HCW survey Pre-launch job still swapped (DOB and sex
##     corrected, job not). Screening job consistent at all visits, so final_job
##     is correct. Query: were other Pre-launch survey forms also moved?
##   22060093: mentor mother at all visits except Pre-launch screening (HTS).
##     Its Pre-launch screening and 22060091's 6-month screening both show
##     HTS counsellor + DOB 1992-10-20: possibly one HCW's records filed under
##     the wrong IDs. final_job (HTS) likely wrong; flagged; query data team.
## 5 blank in main, all 6-month entrants (job question likely hidden by
## branching at the 6-month event). 24060098 12-month mismatch corrected.
## Cadre unchanged: fully consistent with job title; no rule needed.

### Cadre and job at entry ------
job_entry <- screen_key %>%
    semi_join(entry, by = c("screen_id" = "pt_id", "wave" = "entry_wave")) %>%
    transmute(pt_id = screen_id,
              cadre = str_remove(pt_type, "^\\d+:\\s*"),
              job_entry = rct_job)

nrow(job_entry)   # 217

# n (%)
job_entry %>%
    count(cadre) %>%
    mutate(n_pct = paste0(n, " (", round(100 * n / sum(n), 1), "%)")) %>%
    select(cadre, n_pct)

job_entry %>%
    count(job_entry) %>%
    mutate(n_pct = paste0(n, " (", round(100 * n / sum(n), 1), "%)")) %>%
    select(job = job_entry, n_pct)

## Department ------------
## Options (same order in both forms):
##   1 ANC, 2 PNC, 3 Immunization, 4 PMTCT, 5 Family planning,
##   6 Psychological/psychiatric care, 7 Other (specify)
## Department changes over time (rotation), so no majority vote.
## Rule: main demographic form at the ENTRY visit (participant self-report).
##   Screening at the same visit used as a check.
## "Other" text recoded into extra categories (multi-label).

dpt_main <- demog_df %>%
    transmute(pt_id, wave,
              across(dpt___1:dpt___7, to01, .names = "d{str_remove(.col, 'dpt___')}"),
              other_text = na_if(str_trim(dpt_other), ""))

dpt_scr <- screen_key %>%
    transmute(pt_id = screen_id, wave,
              d1 = rct_dpt___1, d2 = rct_dpt___2, d3 = rct_dpt___3,
              d4 = rct_dpt___4, d5 = rct_dpt___5, d6 = rct_dpt___6,
              d7 = rct_dpt___7,
              other_text = na_if(str_trim(rct_dpt_other), ""))

## Validity: at least one ticked; "Other" text only with "Other" ticked 
bind_rows(dpt_main %>% mutate(source = "main"),
          dpt_scr  %>% mutate(source = "screen")) %>%
    group_by(source) %>%
    summarise(n             = n(),
              none_ticked   = sum(d1 + d2 + d3 + d4 + d5 + d6 + d7 == 0),
              other_no_text = sum(d7 == 1 & is.na(other_text)),
              text_no_other = sum(d7 == 0 & !is.na(other_text)),
              .groups = "drop")
## Result (Oct 4, 2026): all 0 in both sources.

## Main vs screening at the same visit
dpt_compare <- dpt_main %>%
    inner_join(dpt_scr, by = c("pt_id", "wave"), suffix = c("_m", "_s")) %>%
    mutate(n_diff = (d1_m != d1_s) + (d2_m != d2_s) + (d3_m != d3_s) +
               (d4_m != d4_s) + (d5_m != d5_s) + (d6_m != d6_s) +
               (d7_m != d7_s))

dpt_compare %>% count(wave, exact = n_diff == 0)
dpt_compare %>% count(n_diff)

## Recode "Other" text 
recode_other_dpt <- function(df) {
    df %>%
        mutate(
            txt = str_squish(str_to_upper(other_text)),
            o_maternity = str_detect(txt, "MATERNITY|MARTENITY"),
            o_opd       = str_detect(txt, "OPD|OUT ?PATIENT|\\bOPP\\b|CASUALTY"),
            o_ipd       = str_detect(txt, "IPD|IN ?PATIENT|\\bWARD\\b|PAEDIATRIC"),
            o_ccc       = str_detect(txt, "CCC|COMPREHENSIVE CARE|CARE AND TREATMENT|ADHERENCE|DEFAU?L?TER|LINKAGE"),
            o_hts       = str_detect(txt, "HTS|HTC|H TS|TESTING"),
            o_triage    = str_detect(txt, "TRIAG"),
            o_cancer    = str_detect(txt, "CANCER"),
            o_theatre   = str_detect(txt, "THEATRE"),
            o_cwc       = str_detect(txt, "UNDER 5|CWC"),
            across(starts_with("o_"), ~ as.integer(replace_na(.x, FALSE)))
        ) %>%
        mutate(o_unclassified = as.integer(!is.na(txt) &
                                               rowSums(across(starts_with("o_"))) == 0)) %>%
        select(-txt)
}

## Department at entry (main form) 
dpt_entry <- dpt_main %>%
    semi_join(entry, by = c("pt_id", "wave" = "entry_wave")) %>%
    recode_other_dpt()

nrow(dpt_entry)   # 217

# Mapping check: what each "Other" text became
dpt_entry %>%
    filter(!is.na(other_text)) %>%
    pivot_longer(starts_with("o_"), names_to = "category") %>%
    filter(value == 1) %>%
    group_by(other_text) %>%
    summarise(categories = paste(str_remove(category, "^o_"), collapse = ", "),
              .groups = "drop") %>%
    as_tibble() %>%
    print(n = Inf)

# n (%) at entry (multi-select: % of HCWs, does not sum to 100)
dpt_entry %>%
    summarise(across(c(d1:d7, starts_with("o_")), sum)) %>%
    pivot_longer(everything(), names_to = "department", values_to = "n") %>%
    mutate(
        department = recode(department,
                            d1 = "ANC", d2 = "PNC", d3 = "Immunization", d4 = "PMTCT",
                            d5 = "Family planning", d6 = "Psych care", d7 = "Other (any)",
                            o_maternity = "Other: Maternity", o_opd = "Other: OPD",
                            o_ipd = "Other: IPD / wards", o_ccc = "Other: CCC",
                            o_hts = "Other: HTS", o_triage = "Other: Triage",
                            o_cancer = "Other: Cancer screening", o_theatre = "Other: Theatre",
                            o_cwc = "Other: Under 5 / CWC", o_unclassified = "Other: Unclassified"),
        n_pct = paste0(n, " (", round(100 * n / nrow(dpt_entry), 1), "%)")
    ) %>%
    select(department, n_pct)

## NEW (Nov 25): same-visit department mismatches of 2 or more options
dpt_compare %>%
    filter(n_diff >= 2) %>%
    transmute(pt_id, wave, n_diff,
              set_main   = paste0(d1_m, d2_m, d3_m, d4_m, d5_m, d6_m, d7_m),
              set_screen = paste0(d1_s, d2_s, d3_s, d4_s, d5_s, d6_s, d7_s))

## Result (Nov 25, 2026): 325/340 exact (196/200 at entry, 122/133 at
## 12m); 13 differ by 1 option, 2 by 2 or more, both swap records at Pre-launch:
##   22110153 (main PMTCT vs screening ANC/PNC/FP; 4 options)
##   22060093 (main ANC/PNC/PMTCT vs screening PMTCT; 2 options)
## final_dpt (main at entry) may be another HCW's for these; flagged.
## (Oct 4: 313/340; 22 and 5.) Miriu 12-month mismatches corrected.
## Validity checks all 0 in both sources.

## Other demographics (main survey only) ------------
## Fields: sex, education, terms (+ other), care for perinatal women,
##   months in job title, months at facility, months in perinatal care,
##   hours/week perinatal care, care for perinatal WLWH.
## No screening equivalent, so checks are internal:
##   (1) free text numeric fields parse, (2) branching consistency,
##   (3) plausibility at entry, (4) entry vs 12-month form (123 HCWs).
## Rule: report entry values. Do not overwrite with 12-month values
##   (12-month forms are less reliable: swaps, and monthsjob often copied
##   from monthswork). Impossible entry values set to NA.

num_vars <- c("monthsjob", "monthswork", "monthscare", "hrscare")

## (1) Non-numeric entries in the free text numeric fields. Expect 0 rows 
demog_df %>%
    select(pt_id, wave, all_of(num_vars)) %>%
    mutate(across(all_of(num_vars), as.character)) %>%
    pivot_longer(all_of(num_vars), names_to = "field", values_to = "value") %>%
    filter(!is.na(value), value != "",
           is.na(suppressWarnings(as.numeric(value)))) %>%
    count(field, value)

## Entry form per HCW 
other_entry <- demog_df %>%
    semi_join(entry, by = c("pt_id", "wave" = "entry_wave")) %>%
    select(pt_id, sex, education, terms, terms_other, care, wlwh, all_of(num_vars)) %>%
    mutate(across(all_of(num_vars), ~ suppressWarnings(as.numeric(.x)))) %>%
    left_join(dob_ref %>% select(pt_id, age_entry), by = "pt_id")

nrow(other_entry)   # 217

## (2) Branching consistency. Expect 0 in each column
## monthscare / hrscare only if care = Yes; terms_other only if terms = Other
other_entry %>%
    summarise(care_no_but_months = sum(care == "No"  & !is.na(monthscare), na.rm = TRUE),
              care_yes_no_months = sum(care == "Yes" &  is.na(monthscare), na.rm = TRUE),
              care_yes_no_hours  = sum(care == "Yes" &  is.na(hrscare),    na.rm = TRUE),
              terms_other_text   = sum(str_detect(terms, "^Other") & is.na(terms_other), na.rm = TRUE))


## (3) Plausibility at entry 
## Cannot have worked longer than (age - 15) years; perinatal care months
## should not exceed months at the facility; > 80 h/week is implausible.
other_entry <- other_entry %>%
    mutate(max_months = (age_entry - 15) * 12,
           flag_months = (monthsjob  > max_months) %in% TRUE |
               (monthswork > max_months) %in% TRUE |
               (monthscare > monthswork) %in% TRUE |
               (hrscare    > 80)         %in% TRUE)

other_entry %>%
    filter(flag_months) %>%
    select(pt_id, age_entry, all_of(num_vars))
## Result (Oct 4, 2026): 3 flagged at entry.
##   24190264: monthsjob = 16836 (impossible) -> NA in other_final; query.
##   23170227, 24090149: monthscare (120) > monthswork (90, 60). Likely
##   counted perinatal care across facilities. Kept as recorded; flagged.

## (4) Entry vs 12-month form (HCWs with two forms) 
other_pairs <- demog_df %>%
    arrange(pt_id, wave) %>%
    group_by(pt_id) %>%
    filter(n() == 2) %>%
    summarise(sex_entry = first(sex),       sex_12m = last(sex),
              educ_entry = first(education), educ_12m = last(education),
              .groups = "drop")

educ_levels <- c("No schooling completed", "Primary school", "Secondary school",
                 "Diploma", "Bachelor's degree or higher degree")

other_pairs %>%
    summarise(n           = n(),
              sex_change  = sum(sex_entry != sex_12m, na.rm = TRUE),
              educ_down   = sum(match(educ_12m, educ_levels) < match(educ_entry, educ_levels),
                                na.rm = TRUE))

## Result (Oct 4, 2026): 123 pairs. Sex changes only for 22110153 and
## 22110154 (opposite directions: suspect swapped 12-month forms?).

educ_down_ids <- other_pairs %>%
    filter(match(educ_12m, educ_levels) < match(educ_entry, educ_levels)) %>%
    pull(pt_id)
educ_down_ids

## Result (Oct 4, 2026): 5 HCWs (22040064, 22060093, 22060100, 22170229,
## 23220308). Only 22060093 is a swap suspect. Entry values kept. But needed query.

## Final entry values ------
other_final <- other_entry %>%
    mutate(monthsjob  = if_else(monthsjob  > max_months, NA_real_, monthsjob),
           monthswork = if_else(monthswork > max_months, NA_real_, monthswork),
           hrscare    = if_else(hrscare > 168, NA_real_, hrscare)) %>%   # > hours in a week
    select(-max_months)


## n (%) and median [IQR] at entry ------
n_pct <- function(df, var) {
    df %>%
        count(value = {{ var }}) %>%
        mutate(n_pct = paste0(n, " (", round(100 * n / sum(n), 1), "%)")) %>%
        select(value, n_pct)
}

n_pct(other_final, sex)
n_pct(other_final, education)
n_pct(other_final, terms)
n_pct(other_final, care)
n_pct(other_final, wlwh)

other_final %>%
    summarise(across(all_of(num_vars),
                     ~ sprintf("%.0f [%.0f, %.0f]  (n = %d)",
                               median(.x, na.rm = TRUE),
                               quantile(.x, 0.25, na.rm = TRUE),
                               quantile(.x, 0.75, na.rm = TRUE),
                               sum(!is.na(.x))))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "median_iqr")
## Result (Nov 25, 2026): all clean for this section


##  Master dataset ------------
## One row per HCW (217).
##   final_*                 : cleaned values for reporting (entry visit)
##   *_main_w0, *_screen_w6  : ORIGINAL values, by source and visit
##   flag_*                  : conflicts (TRUE = issue)
## Nothing is dropped; originals sit next to final values.

## IDs and arm (entry form) 
master_ids <- id_arm_chk %>%
    semi_join(entry, by = c("pt_id", "wave" = "entry_wave")) %>%
    transmute(pt_id, part_id, consent_id = as.numeric(consent_id),
              facility_code, facility_name = str_remove(facility_id, "^\\d+,\\s*"),
              condition, arm, cadre_code) %>%
    left_join(entry, by = "pt_id")


## Final values 
dpt_final <- dpt_entry %>%
    select(pt_id, d1:d7, starts_with("o_"), dpt_other_text = other_text) %>%
    rename(final_dpt_anc = d1, final_dpt_pnc = d2, final_dpt_immunization = d3,
           final_dpt_pmtct = d4, final_dpt_fp = d5, final_dpt_psych = d6,
           final_dpt_other = d7) %>%
    rename_with(~ str_replace(.x, "^o_", "final_dpt_other_"), starts_with("o_"))

master_final <- master_ids %>%
    left_join(dob_ref %>%
                  transmute(pt_id, final_dob = dob_final, final_age = round(age_entry, 1),
                            dob_rule, dob_agreement),
              by = "pt_id") %>%
    left_join(job_entry %>% rename(final_cadre = cadre, final_job = job_entry),
              by = "pt_id") %>%
    left_join(dpt_final, by = "pt_id") %>%
    left_join(other_final %>%
                  select(-age_entry, -flag_months) %>%
                  rename_with(~ paste0("final_", .x), -pt_id),
              by = "pt_id")


## Original values (wide) 
orig_dob <- dob_votes %>%
    transmute(pt_id, col = paste0("dob_", source, "_w", wave), dob) %>%
    pivot_wider(names_from = col, values_from = dob)

orig_dpt <- bind_rows(dpt_main %>% mutate(source = "main"),
                      dpt_scr  %>% mutate(source = "screen")) %>%
    transmute(pt_id, col = paste0("dpt_set_", source, "_w", wave),
              set = paste0(d1, d2, d3, d4, d5, d6, d7)) %>%   # options 1..7, 1 = ticked
    pivot_wider(names_from = col, values_from = set)

orig_job <- screen_key %>%
    transmute(pt_id = screen_id, col = paste0("job_screen_w", wave), rct_job) %>%
    pivot_wider(names_from = col, values_from = rct_job)

orig_other <- demog_df %>%
    select(pt_id, wave, sex, education, monthsjob, monthswork, monthscare, hrscare) %>%
    pivot_wider(id_cols = pt_id, names_from = wave,
                values_from = c(sex, education, monthsjob, monthswork, monthscare, hrscare),
                names_glue = "{.value}_main_w{wave}")


## Flags 
dpt_flag <- dpt_compare %>%
    arrange(pt_id, wave) %>%
    group_by(pt_id) %>%
    summarise(dpt_ndiff_entry = first(n_diff),   # entry visit
              dpt_ndiff_max   = max(n_diff),     # any visit
              .groups = "drop")

job_flag <- job_compare %>%
    group_by(pt_id) %>%
    summarise(job_mismatch_any = any(job_status == "mismatch"), .groups = "drop")

sex_change_ids <- other_pairs %>% filter(sex_entry != sex_12m) %>% pull(pt_id)
# (Nov 25, 2026): kept all 5. Nyandiwa DOB and sex corrected, but Pre-launch
# HCW survey job and department still swapped; Madiany unresolved.
swap_ids       <- c(22110153, 22110154, 22060091, 22060093, 22060094)
# (Nov 25, 2026): Miriu 12-month records corrected; no longer flagged
fac09_ids      <- numeric(0)


## Assemble 
demog_master <- master_final %>%
    left_join(dpt_flag,   by = "pt_id") %>%
    left_join(job_flag,   by = "pt_id") %>%
    left_join(other_entry %>% select(pt_id, flag_months), by = "pt_id") %>%
    left_join(orig_dob,   by = "pt_id") %>%
    left_join(orig_dpt,   by = "pt_id") %>%
    left_join(orig_job,   by = "pt_id") %>%
    left_join(orig_other, by = "pt_id") %>%
    mutate(
        flag_dob_major    = dob_agreement == "major (>= 1 year)",
        flag_dob_minor    = dob_agreement == "minor (< 1 year)",
        flag_dpt_entry    = (dpt_ndiff_entry >= 1) %in% TRUE,
        flag_dpt_large    = (dpt_ndiff_max   >= 2) %in% TRUE,
        flag_job_mismatch = job_mismatch_any %in% TRUE,
        flag_months_entry = flag_months %in% TRUE,
        flag_sex_change   = pt_id %in% sex_change_ids,
        flag_educ_down    = pt_id %in% educ_down_ids,
        flag_swap_suspect = pt_id %in% swap_ids,
        flag_fac09_12m    = pt_id %in% fac09_ids
    ) %>%
    select(-flag_months, -job_mismatch_any)

flag_cols <- grep("^flag_", names(demog_master), value = TRUE)

demog_master <- demog_master %>%
    mutate(
        n_flags = rowSums(across(all_of(flag_cols))),
        issues  = apply(across(all_of(flag_cols)), 1, function(r)
            paste(str_remove(flag_cols[as.logical(r)], "^flag_"), collapse = "; ")),
        # major = possibly another person's data, or a wrong age
        conflict_level = case_when(
            flag_dob_major | flag_sex_change | flag_swap_suspect | flag_fac09_12m |
                flag_dpt_large | flag_months_entry ~ "major",
            n_flags > 0                          ~ "minor",
            TRUE                                 ~ "none"),
        # does the conflict touch the ENTRY values we report?
        affects_entry = (flag_dob_major & dob_rule != "entry forms agree") |
            flag_dpt_entry | flag_months_entry | flag_swap_suspect
    ) %>%
    select(pt_id, part_id, consent_id, facility_code, facility_name, condition, arm,
           cadre_code, entry_wave, entry_date,
           starts_with("final_"), dpt_other_text,
           conflict_level, affects_entry, n_flags, issues, starts_with("flag_"),
           dob_rule, dob_agreement, dpt_ndiff_entry, dpt_ndiff_max,
           everything())

stopifnot(nrow(demog_master) == 217, n_distinct(demog_master$pt_id) == 217)

demog_master %>% count(conflict_level)
demog_master %>% count(conflict_level, affects_entry)

#######################################################################################
### Data team list  ------------
## Major conflicts only. IDs and original values; no names or contacts.
datateam_list <- demog_master %>%
    filter(conflict_level == "major") %>%
    select(pt_id, part_id, consent_id, facility_name, condition, issues,
           starts_with("dob_main_"), starts_with("dob_screen_"), final_dob,
           starts_with("sex_main_"), starts_with("education_main_"),
           starts_with("dpt_set_"), starts_with("job_screen_"),
           starts_with("monthsjob_main_"), starts_with("monthswork_main_"),
           starts_with("monthscare_main_")) %>%
    arrange(facility_name, pt_id)

nrow(datateam_list)
datateam_list %>% count(issues, sort = TRUE)


## Save (run when ready; contains DOBs, keep on the secure drive) 
# write.csv(demog_master,  file.path(ipmh_filepath, "demog_master.csv"),        row.names = FALSE)
# write.csv(datateam_list, file.path(ipmh_filepath, "demog_datateam_list.csv"), row.names = FALSE)

# arm check (newly added Nov 24) --------
## Arm check on ALL HCW survey rows (not only demographic forms) 
## Recorded arm (from the REDCap event) vs expected arm
## (facility randomization + cadre from the ID). Expect 0 rows.
intervention_fac <- c(1, 3, 4, 7, 9, 13, 16, 17, 19, 22)

arm_all_rows <- hcw_is_df %>%
    mutate(wave  = event_to_wave(redcap_event_name),
           pt_id = as.numeric(pt_id),
           arm   = str_extract(redcap_event_name, "(?<=: ).+(?=\\))"),
           condition_recorded = str_extract(arm, "^(Intervention|Control)"),
           # expected from the ID: facility digits -> randomization
           fac_from_id        = (pt_id %/% 1e4) %% 100,
           condition_expected = if_else(fac_from_id %in% intervention_fac,
                                        "Intervention", "Control"),
           cadre_from_id      = pt_id %/% 1e6,
           cadre_recorded     = case_when(str_detect(arm, "lay providers") ~ 22,
                                          str_detect(arm, "in charge")     ~ 23,
                                          str_detect(arm, "nurses/cos")    ~ 24))

arm_all_rows %>%
    filter(condition_recorded != condition_expected | cadre_recorded != cadre_from_id) %>%
    select(pt_id, part_id, wave, redcap_event_name, fac_from_id)

## Rows per arm and visit vs HCWs enrolled in screening (should match) ------
arm_all_rows %>% count(condition_recorded, wave)

## Arm check on all HCW survey rows (Nov 24, 2026):
##   1 misentry: 23160220 (in-charge, Usigu, intervention) 12-month survey
##   entered under Arm 5 (Control - in charge) -> intervention-only forms
##   (incl. AIM/IAM/FIM) not administered at that visit. Query data team.
##   24090143 (nurse/CO, Miriu): no 12-month HCW survey record. Query data team.
##   All other rows match the arm expected from facility and cadre.

#######################################################################################

# Culture, stress and effort (CFIR Inner Setting) ------------
## Source: inner_setting_culture_stress form, HCW survey database.
## Items: culture_1-9, cstress_1-6, ceffort_1-5.
## Response: 1 Strongly disagree ... 5 Strongly agree; 99 Prefer not to answer.

cse_items <- c(paste0("culture_", 1:9), paste0("cstress_", 1:6), paste0("ceffort_", 1:5))

cse_all <- hcw_is_df %>%
    mutate(wave  = event_to_wave(redcap_event_name),
           arm   = str_extract(redcap_event_name, "(?<=: ).+(?=\\))"),
           pt_id = as.numeric(pt_id)) %>%
    select(part_id, pt_id, wave, arm, pt_type, all_of(cse_items),
           form_status = inner_setting_culture_stress_complete)

## (1) Confirm: completed only at 6 months ------
## Expect "Complete" only at wave 6 (about 198); other waves blank
cse_all %>% count(wave, form_status)

# CHANGED: keep the 6-month visit only
cse_raw <- cse_all %>% filter(wave == 6)

# Enrolled at 6 months but no completed form. Expect 0 rows
screen_key %>%
    filter(wave == 6) %>%
    select(pt_id = screen_id) %>%
    anti_join(cse_raw %>% filter(form_status == "Complete"), by = "pt_id")

# Completed by arm (in-charges, lay providers, nurses/COs; each condition)
cse_raw %>% count(arm, form_status)

## (2) Every response value that appears ------
cse_raw %>%
    pivot_longer(all_of(cse_items), names_to = "item", values_to = "value") %>%
    count(value, sort = TRUE)

## (3) Missing and "Prefer not to answer", per item ------
cse_raw %>%
    filter(form_status == "Complete") %>%
    pivot_longer(all_of(cse_items), names_to = "item", values_to = "value") %>%
    group_by(item) %>%
    summarise(n          = n(),
              missing    = sum(is.na(value) | value == ""),
              prefer_not = sum(str_detect(value, "Prefer not"), na.rm = TRUE),
              .groups    = "drop") %>%
    print(n = Inf)

## (4) Completed forms that link to the cleaned HCW list. Expect 0 rows ------
cse_raw %>%
    filter(form_status == "Complete") %>%
    anti_join(demog_master, by = "pt_id") %>%
    select(part_id, pt_id, wave, arm)

## ## Recode responses to 1-5 
## Culture items are exported as "4: Agree"; stress/effort items as "Agree".
## Strip the "n: " prefix so both formats match, then map to 1-5.
## "Prefer not to answer" (99) becomes NA.
likert_lev <- c("Strongly disagree" = 1, "Disagree" = 2, "Neutral" = 3,
                "Agree" = 4, "Strongly agree" = 5)

to_likert <- function(x) {
    x <- str_trim(str_remove(x, "^\\d+:\\s*"))   # "4: Agree" -> "Agree"
    unname(likert_lev[x])                        # "Prefer not to answer" -> NA
}

cse_num <- cse_raw %>%
    filter(form_status == "Complete") %>%
    mutate(across(all_of(cse_items), to_likert))

## Check: every original label mapped as intended ------
## Expect 10 label types -> 1 to 5, and "Prefer not to answer" -> NA only
cse_raw %>%
    filter(form_status == "Complete") %>%
    pivot_longer(all_of(cse_items), names_to = "item", values_to = "label") %>%
    mutate(value = to_likert(label)) %>%
    count(label, value) %>%
    arrange(value)

## Which HCWs and items have "Prefer not to answer" (4 responses) 
cse_num %>%
    pivot_longer(all_of(cse_items), names_to = "item", values_to = "value") %>%
    filter(is.na(value)) %>%
    select(pt_id, arm, item)

## Reverse-worded items: 6 - x 
## culture_3 ("hard to get things to change"),
## ceffort_3 ("get by with doing as little as possible"),
## ceffort_5 ("do not put more effort than they have to")
rev_items <- c("culture_3", "ceffort_3", "ceffort_5")

cse_rev <- cse_num %>%
    mutate(across(all_of(rev_items), ~ 6 - .x, .names = "{.col}_r"))

# Check: original vs reversed. Expect 1<->5, 2<->4, 3<->3
cse_rev %>% count(culture_3, culture_3_r)

## Clean dataset: link to the cleaned HCW list ------
# CHANGED: starts from cse_rev (reversed items already added)
cse_clean <- cse_rev %>%
    left_join(demog_master %>%
                  select(pt_id, facility_code, facility_name, condition,
                         cadre_code, flag_swap_suspect),
              by = "pt_id") %>%
    select(pt_id, part_id, facility_code, facility_name, condition, cadre_code,
           flag_swap_suspect, all_of(cse_items), ends_with("_r"))

## Checks 
nrow(cse_clean)                          # 198
n_distinct(cse_clean$pt_id)              # 198, one row per HCW
sum(is.na(cse_clean$condition))          # 0: all linked
cse_clean %>% count(flag_swap_suspect)   # 5 TRUE if all swap suspects present

# Acceptability, appropriateness and feasibility (AIM / IAM / FIM) ------------
## Source: acceptability_appropriateness_and_feasibility form, HCW survey.
## 4 targets x 3 scales x 4 items = 48 items:
##   IPMH package (aim/iam/fim_1-4), screening (_sc_), PM+ (_pm_), telepsychiatry (_tp_)
## Response: 1 Completely disagree ... 5 Completely agree; 99 Prefer not to answer.

aif_items <- grep("^(aim|iam|fim)(_sc|_pm|_tp)?_[1-4]$", 
                  names(hcw_is_df), value = TRUE)
length(aif_items)   # expect 48

aif_all <- hcw_is_df %>%
    mutate(wave  = event_to_wave(redcap_event_name),
           arm   = str_extract(redcap_event_name, "(?<=: ).+(?=\\))"),
           pt_id = as.numeric(pt_id)) %>%
    select(part_id, pt_id, wave, arm, pt_type, all_of(aif_items),
           form_status = acceptability_appropriateness_and_feasibility_complete)

## (1) Who completed the form, by visit and arm ------
aif_all %>% count(wave, form_status)
aif_all %>%
    count(arm, wave, form_status) %>%
    pivot_wider(names_from = wave, values_from = n, names_prefix = "w", values_fill = 0)

## (2) Which targets were answered, by arm and visit ------
## Number of non-missing items per target among completed forms
aif_all %>%
    filter(form_status == "Complete") %>%
    mutate(package   = rowSums(!is.na(across(matches("^(aim|iam|fim)_[1-4]$")))),
           screening = rowSums(!is.na(across(contains("_sc_")))),
           pm_plus   = rowSums(!is.na(across(contains("_pm_")))),
           telepsych = rowSums(!is.na(across(contains("_tp_"))))) %>%
    count(arm, wave, package, screening, pm_plus, telepsych)

## (3) Every response value that appears ------
aif_all %>%
    filter(form_status == "Complete") %>%
    pivot_longer(all_of(aif_items), names_to = "item", values_to = "value") %>%
    count(value, sort = TRUE)

## (4) Intervention HCWs enrolled at 12 months but no AIM/IAM/FIM form ------
## Expect 2 (1 in-charge, 1 nurse/CO, from the counts above)
screen_key %>%
    filter(wave == 12) %>%
    transmute(pt_id = screen_id, wave) %>%
    inner_join(demog_master %>%
                   filter(condition == "Intervention") %>%
                   select(pt_id, facility_name, cadre_code),
               by = "pt_id") %>%
    anti_join(aif_all %>% filter(form_status == "Complete"), by = c("pt_id", "wave"))


## Recode responses to 1-5 ------
## Labels differ from the culture scales: "Completely disagree" ... "Completely agree"
aif_lev <- c("Completely disagree" = 1, "Disagree" = 2,
             "Neither agree nor disagree" = 3,
             "Agree" = 4, "Completely agree" = 5)

to_aif <- function(x) unname(aif_lev[str_trim(x)])   # "Prefer not to answer" -> NA

# NEW: intervention forms only; one row per HCW per visit
aif_num <- aif_all %>%
    filter(form_status == "Complete") %>%
    mutate(across(all_of(aif_items), to_aif))

## Check: every label mapped as intended 
## Expect 5 labels -> 1 to 5; "Prefer not to answer" and NA -> NA
aif_all %>%
    filter(form_status == "Complete") %>%
    pivot_longer(all_of(aif_items), names_to = "item", values_to = "label") %>%
    mutate(value = to_aif(label)) %>%
    count(label, value) %>%
    arrange(value)

## Which HCWs and items have "Prefer not to answer" (3 responses) 
aif_all %>%
    filter(form_status == "Complete") %>%
    pivot_longer(all_of(aif_items), names_to = "item", values_to = "label") %>%
    filter(label == "Prefer not to answer") %>%
    select(pt_id, wave, arm, item)

## Clean dataset: link to the cleaned HCW list ------
## One row per HCW per visit (297 rows).
## Package items at all visits; screening, PM+ and telepsychiatry at 12 months only
## (NA at Pre-launch and 6 months by design).
aif_clean <- aif_num %>%
    left_join(demog_master %>%
                  select(pt_id, facility_code, facility_name, condition,
                         cadre_code, flag_swap_suspect),
              by = "pt_id") %>%
    select(pt_id, part_id, wave, facility_code, facility_name, condition, cadre_code,
           flag_swap_suspect, all_of(aif_items))

## Checks 
nrow(aif_clean)                            # 297
aif_clean %>% count(wave)                  # 101 / 100 / 96
aif_clean %>% count(pt_id, wave) %>% filter(n > 1)   # 0 rows
sum(is.na(aif_clean$condition))            # 0: all linked
aif_clean %>% count(condition)             # Intervention only

# Sustainability: NoMAD (providers) and PSAT (facility in-charges) ------------
## Source: sustainability_nom and sustainability_psat forms, HCW survey.
## NoMAD: intervention HCWs, 6 and 12 months. PSAT: intervention in-charges, 6 and 12 months.
## Arm, facility and cadre taken from demog_master (not the REDCap event).

psat_items  <- grep("^sus_(env_sup|funding|partner|or_cap|eval|adp|com|plan)_[1-3]$",
                    names(hcw_is_df), value = TRUE)
nomad_gen   <- c("sus2_familiar", "sus2_normal_now", "sus2_normal_will")
nomad_items <- grep("^sus2_[1-4]_[1-7]$", names(hcw_is_df), value = TRUE)
length(psat_items); length(nomad_items)   # 24 and 20

sus_all <- hcw_is_df %>%
    mutate(wave  = event_to_wave(redcap_event_name),
           arm   = str_extract(redcap_event_name, "(?<=: ).+(?=\\))"),
           pt_id = as.numeric(pt_id)) %>%
    select(part_id, pt_id, wave, arm, pt_type,
           sus2_role, all_of(nomad_gen), all_of(nomad_items), all_of(psat_items),
           nom_status  = sustainability_nom_complete,
           psat_status = sustainability_psat_complete)

## (1) Who completed each form ------
sus_all %>% count(arm, wave, nom_status) %>%
    pivot_wider(names_from = wave, values_from = n, names_prefix = "w", values_fill = 0)
sus_all %>% count(arm, wave, psat_status) %>%
    pivot_wider(names_from = wave, values_from = n, names_prefix = "w", values_fill = 0)
## Result (Nov 27, 2026): NoMAD intervention HCWs only, 6m 100, 12m 96.
## PSAT intervention in-charges only, 6m 9, 12m 8. 23160220 completed both at
## 6m; none at 12m (12-month survey under control arm).

## (2) Response values ------
sus_all %>% filter(nom_status == "Complete") %>%
    pivot_longer(all_of(nomad_items), names_to = "item", values_to = "value") %>%
    count(value, sort = TRUE)
sus_all %>% filter(psat_status == "Complete") %>%
    pivot_longer(all_of(psat_items), names_to = "item", values_to = "value") %>%
    count(value, sort = TRUE)
## Result (Nov 27, 2026): NoMAD coded 1 = Strongly agree ... 5 = Strongly disagree;
## 20 "Not relevant to my role", 20 blank (one form). PSAT 1-7; 28 "Not able to answer".

## Recode 
## NoMAD construct items: 6-8 ("Not relevant...") -> NA; reversed so 5 = Strongly agree;
##   sus2_3_2 ("IPMH disrupts working relationships") reversed again (negatively worded).
## NoMAD general items (0-10) and PSAT (1-7): leading number; text-only -> NA.
nomad_rc <- function(x) {
    v <- as.integer(str_extract(x, "^\\d"))
    v[v > 5] <- NA
    6 - v
}
lead_num <- function(x) as.integer(str_extract(x, "^\\d+"))

sus_num <- sus_all %>%
    mutate(across(all_of(nomad_items), nomad_rc),
           sus2_3_2 = 6 - sus2_3_2,
           across(all_of(nomad_gen), lead_num),
           across(all_of(psat_items), lead_num))

# Check: original vs recoded (expect 1->5 ... 5->1; "Not relevant" -> NA)
sus_all %>% filter(nom_status == "Complete") %>% select(pt_id, wave, sus2_1_1) %>%
    left_join(sus_num %>% select(pt_id, wave, new = sus2_1_1), by = c("pt_id", "wave")) %>%
    count(sus2_1_1, new)

## Missing responses per item 
sus_num %>% filter(nom_status == "Complete") %>%
    summarise(across(all_of(nomad_items), ~ sum(is.na(.x)))) %>%
    pivot_longer(everything(), names_to = "item", values_to = "n_missing") %>%
    filter(n_missing > 0)
sus_num %>% filter(psat_status == "Complete") %>%
    summarise(across(all_of(psat_items), ~ sum(is.na(.x)))) %>%
    pivot_longer(everything(), names_to = "item", values_to = "n_missing") %>%
    filter(n_missing > 0) %>% arrange(desc(n_missing))
## Result (Nov 27, 2026): NoMAD: sus2_3_6 ("Sufficient resources") missing 10,
## mostly not relevant to role. PSAT: funding items missing for 4 of 17 forms.

## Scores ------
## NoMAD constructs and total: mean of items (1-5), >= 75% answered.
## PSAT domains: mean of items (1-7), >= 2 of 3 answered; total: mean of domains, >= 6 of 8.
mean_min <- function(df, items, min_prop) {
    m <- as.matrix(df[, items]); n_ans <- rowSums(!is.na(m))
    ifelse(n_ans / length(items) >= min_prop, rowMeans(m, na.rm = TRUE), NA_real_)
}

nomad_dom <- list(coherence      = paste0("sus2_1_", 1:4),
                  cog_particip   = paste0("sus2_2_", 1:4),
                  collect_action = paste0("sus2_3_", 1:7),
                  reflex_monitor = paste0("sus2_4_", 1:5))
psat_dom  <- list(env_support = "sus_env_sup", funding = "sus_funding",
                  partnerships = "sus_partner", org_capacity = "sus_or_cap",
                  evaluation = "sus_eval", adaptation = "sus_adp",
                  communications = "sus_com", strategic_plan = "sus_plan")

nomad_clean <- sus_num %>%
    filter(nom_status == "Complete") %>%
    select(pt_id, part_id, wave, sus2_role, all_of(nomad_gen), all_of(nomad_items))
for (d in names(nomad_dom)) {
    nomad_clean[[paste0("nomad_", d)]] <- mean_min(nomad_clean, nomad_dom[[d]], 0.75)
}
nomad_clean$nomad_total <- mean_min(nomad_clean, nomad_items, 0.75)

psat_clean <- sus_num %>%
    filter(psat_status == "Complete") %>%
    select(pt_id, part_id, wave, all_of(psat_items))
for (d in names(psat_dom)) {
    psat_clean[[paste0("psat_", d)]] <- mean_min(psat_clean, paste0(psat_dom[[d]], "_", 1:3), 2/3)
}
psat_clean$psat_total <- mean_min(psat_clean, paste0("psat_", names(psat_dom)), 6/8)

## Clean dataset: Link to cleaned HCW list ------
link_vars <- demog_master %>%
    select(pt_id, facility_code, facility_name, condition, cadre_code,
           final_cadre, flag_swap_suspect)

nomad_clean <- nomad_clean %>% left_join(link_vars, by = "pt_id") %>%
    relocate(pt_id, part_id, wave, facility_code, facility_name, condition,
             cadre_code, final_cadre, flag_swap_suspect)
psat_clean  <- psat_clean  %>% left_join(link_vars, by = "pt_id") %>%
    relocate(pt_id, part_id, wave, facility_code, facility_name, condition,
             cadre_code, final_cadre, flag_swap_suspect)

## Checks ------
nomad_clean %>% count(wave)                              # 100 / 96
psat_clean  %>% count(wave)                              # 9 / 8
nomad_clean %>% count(pt_id, wave) %>% filter(n > 1)     # 0 rows
sum(is.na(nomad_clean$condition)); sum(is.na(psat_clean$condition))   # 0, 0
nomad_clean %>% summarise(across(starts_with("nomad_"), ~ sum(is.na(.x))))
psat_clean  %>% summarise(across(starts_with("psat_"),  ~ sum(is.na(.x))))
## Result (Nov 27, 2026): NoMAD scores missing for 1 form (22130167, 12m, blank);
## cognitive participation also missing for 23190261 (6m).
## PSAT: funding missing 4 (23010026 12m, 23160220 6m, 23220308 6m and 12m);
## environmental support 2; strategic planning 2 (23220308); total missing 0.

## Save ------
# write.csv(nomad_clean, file.path(ipmh_filepath, "nomad_clean.csv"), row.names = FALSE)
# write.csv(psat_clean,  file.path(ipmh_filepath, "psat_clean.csv"),  row.names = FALSE)
# saveRDS(nomad_clean,   file.path(ipmh_filepath, "nomad_clean.rds"))
# saveRDS(psat_clean,    file.path(ipmh_filepath, "psat_clean.rds"))

# Save clean datasets -------------------

# screening_clean_df <- screening_raw_df %>% 
#     select(record_id, arm, anc_num, initial_consent_date, eligible_study, 
#            exclusion_reasons, consented, enrolled, decline_reason)

