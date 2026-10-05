# Header ------------------------------------------------------------------

# Author(s): Yuwei
# Date: Oct 4, 2026
# This is a script to prepare a cleaned HCW implementation variable dataset 
# that's ready for analysis, manuscript writing, and sharing 

# Setup ------------------------------------------------------------------------
rm(list = ls())

# Reference source codes & other dependencies:
source("Dependencies.R")
source("data_import.R")
source("DataTeam_ipmh.R")

# Set up data freeze time for this report
#data_freeze <- as.Date("2026-08-17") 

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

# No one refused to enroll. But find #188 for pre-launch data missing. 
# Yuwei found the logging information, and refilled the form on Oct 4, 2026,
# so this problem has been fixed.

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

# everyone were eligible and consented.

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

## Result (Oct 4, 2026): all IDs, arms and sites correct.
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
## Entry is preferred because two forms filled at the same visit agree,
## and some follow-up forms hold another HCW's data (suspected swaps:
## 22110153/22110154; 22060091/22060093/22060094).
## Age = dob_final at the entry screening date.
## REDCap calc fields (rct_age_cal, cal_age) not used: anchored to "today".

### All DOBs recorded per HCW (one row per form) ------
dob_votes <- bind_rows(
    demog_df   %>% transmute(pt_id, wave, source = "main",   dob = ymd(dob, quiet = TRUE)),
    screen_key %>% transmute(pt_id = screen_id, wave, source = "screen", dob = rct_dob)
) %>%
    filter(!is.na(dob))

dob_votes %>% count(source)   # main = 340 (all parse); screen = 592 (2 gave age only)

### Same visit: main vs screening ------
dob_same_visit <- dob_votes %>%
    pivot_wider(id_cols = c(pt_id, wave), names_from = source, values_from = dob) %>%
    filter(!is.na(main), !is.na(screen)) %>%
    mutate(status = case_when(
        main == screen               ~ "same",
        year(main) == year(screen)   ~ "day/month differs",
        TRUE                         ~ "year differs"))

dob_same_visit %>% count(wave, status)
## Result (Oct 4, 2026): 324/340 same; 10 day/month (< 0.2 yr of age);
## 6 differ by 2 to 10 years.

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
dob_ref %>% count(dob_agreement)   # earlier: 156 all agree, 24 minor, 37 major
summary(dob_ref$age_entry)         # earlier: about 23 to 57

### Screening forms with age only (no DOB): consistent with dob_final? ------
screen_key %>%
    filter(is.na(rct_dob)) %>%
    transmute(pt_id = screen_id, wave, rct_age, screen_date) %>%
    left_join(dob_ref %>% select(pt_id, dob_final), by = "pt_id") %>%
    mutate(age_from_final = floor(time_length(interval(dob_final, screen_date), "years")),
           diff = rct_age - age_from_final)
## Result (Oct 4, 2026): 23080125 diff -1; 24220309 diff 2. Both support dob_final.

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
## Result (Oct 4, 2026): 333/340 match (all 200 at entry).
## 6 blank in main, all 6-month entrants (job question likely hidden by
## branching at the 6-month event; check in REDCap).
## 1 mismatch: 24060098 at 12-month (screen Nurse, main CO); entry matches.

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
## Result (Oct 4, 2026): 313/340 exact (190/200 at entry); per option
## agreement 97 to 99%. 22 differ by 1 option, 5 by 2 or more.
## Largest mismatches cluster at facility 09, 12-month
## (22090136, 22090137, 22090138, 22090140); entry not affected.
## Within person, department set changed for 57% (main, entry vs 12m)
## and 69% (screening, across waves): mostly real rotation.

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
swap_ids       <- c(22110153, 22110154, 22060091, 22060093, 22060094)
fac09_ids      <- c(22090136, 22090137, 22090138, 22090140)


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
            flag_dpt_entry | flag_months_entry
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


## Save (run when ready; contains DOBs, keep on the secure drive) ------
# write.csv(demog_master,  file.path(ipmh_filepath, "demog_master.csv"),        row.names = FALSE)
# write.csv(datateam_list, file.path(ipmh_filepath, "demog_datateam_list.csv"), row.names = FALSE)

# Demographics: Table and figures ------------
## Uses final_* columns only (entry visit).
## Columns: Overall, Intervention, Control. Categorical = n (%);
## age = mean (SD); months and hours = median [IQR].

part_wave <- screen_key %>%
    transmute(pt_id = screen_id, wave) %>%
    left_join(demog_master %>%
                  select(pt_id, part_id, facility_code, facility_name,
                         condition, cadre_code),
              by = "pt_id")

## Table 1. Arm assignment by facility ------------
## HCWs enrolled at each visit (screening database), by facility, arm, cadre.
## Cells show "Pre-launch / 6m / 12m" counts of HCWs who answered.
cadre_lab <- c("22" = "Lay providers", "24" = "Nurses/COs", "23" = "In-charges")

tab_src <- part_wave %>%
    mutate(cadre = cadre_lab[as.character(cadre_code)])

# add arm totals and an "All cadres" column by stacking copies
tab_src <- bind_rows(tab_src,
                     tab_src %>% mutate(facility_code = 99L, facility_name = "Arm total"))
tab_src <- bind_rows(tab_src,
                     tab_src %>% mutate(cadre = "All cadres"))

fac_table <- tab_src %>%
    count(condition, facility_code, facility_name, cadre, wave) %>%
    pivot_wider(names_from = wave, values_from = n,
                names_prefix = "w", values_fill = 0) %>%
    mutate(cell = paste(w0, w6, w12, sep = " / ")) %>%
    select(-w0, -w6, -w12) %>%
    pivot_wider(names_from = cadre, values_from = cell, values_fill = "0 / 0 / 0") %>%
    left_join(tab_src %>%
                  filter(cadre == "All cadres") %>%
                  group_by(condition, facility_code) %>%
                  summarise(`HCWs (ever)` = n_distinct(pt_id), .groups = "drop"),
              by = c("condition", "facility_code")) %>%
    arrange(desc(condition), facility_code) %>%      # Intervention first
    mutate(facility = if_else(facility_code == 99, facility_name,
                              sprintf("%02d %s", facility_code, facility_name))) %>%
    select(condition, facility,
           any_of(c("Lay providers", "Nurses/COs", "In-charges", "All cadres")),
           `HCWs (ever)`)

fac_table %>% print(n = Inf)

n_int <- sum(fac_table$condition == "Intervention")

fac_table %>%
    select(-condition) %>%
    kbl(caption = "HCWs enrolled by facility, arm, cadre and visit",
        align = c("l", rep("c", 5))) %>%
    kable_styling(bootstrap_options = c("striped", "condensed"),
                  full_width = FALSE) %>%
    add_header_above(c(" " = 1, "Pre-launch / 6m / 12m" = 4, " " = 1)) %>%
    pack_rows("Intervention", 1, n_int) %>%
    pack_rows("Control", n_int + 1, nrow(fac_table)) %>%
    row_spec(which(fac_table$facility == "Arm total"), bold = TRUE)

## Table 1 by arm, with SMD 
# CHANGED: add_difference() before add_overall(); SMD instead of p-values
make_tbl1 <- function(df) {
    t1_data(df) %>%
        tbl_summary(
            by = condition,
            type = list(starts_with("Dept:") ~ "dichotomous",
                        where(is.numeric) & !starts_with("Dept:") ~ "continuous"),
            statistic = list(all_continuous()  ~ "{median} [{p25}, {p75}]",
                             `Age (years)`     ~ "{mean} ({sd})",
                             all_categorical() ~ "{n} ({p}%)"),
            missing = "ifany", missing_text = "Missing"
        ) %>%
        add_difference(test = everything() ~ "smd") %>%   # NEW
        add_overall() %>%
        bold_labels()
}

tbl1 <- make_tbl1(demog_master)
tbl1

## Figure 1: HCWs enrolled by facility, cadre and visit ------------

cadre_lab <- c("22" = "Lay providers", "24" = "Nurses/COs", "23" = "In-charges")

fig_src <- screen_key %>%
    transmute(pt_id = screen_id, wave) %>%
    left_join(demog_master %>%
                  select(pt_id, facility_code, facility_name, condition, cadre_code),
              by = "pt_id") %>%
    mutate(
        cadre     = factor(cadre_lab[as.character(cadre_code)],
                           levels = c("Lay providers", "Nurses/COs", "In-charges")),
        wave      = factor(wave, levels = c(0, 6, 12),
                           labels = c("Pre-launch", "6-month", "12-month")),
        condition = factor(condition, levels = c("Intervention", "Control")),
        facility  = sprintf("%02d %s", facility_code, facility_name),
        # facility 01 at the top of each panel
        facility  = factor(facility, levels = rev(sort(unique(facility))))
    ) %>%
    count(condition, facility, wave, cadre)

# totals for the label at the end of each bar
fig_tot <- fig_src %>%
    group_by(condition, facility, wave) %>%
    summarise(total = sum(n), .groups = "drop")

ggplot(fig_src, aes(x = n, y = facility, fill = cadre)) +
    geom_col(width = 0.7, colour = "white", linewidth = 0.4,      # thin gap between segments
             position = position_stack(reverse = TRUE)) +          # stack order = legend order
    geom_text(data = fig_tot, aes(x = total, y = facility, label = total),
              inherit.aes = FALSE, hjust = -0.3, size = 3, colour = "grey30") +
    facet_grid(condition ~ wave, scales = "free_y", space = "free_y") +
    scale_fill_manual(values = c("Lay providers" = "#2a78d6",
                                 "Nurses/COs"    = "#eb6834",
                                 "In-charges"    = "#1baf7a")) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = "HCWs enrolled by facility, cadre and visit",
         x = "Number of HCWs", y = NULL, fill = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position    = "top",
          panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank(),
          strip.text         = element_text(face = "bold"),
          plot.title         = element_text(face = "bold"))

# ggsave(file.path(ipmh_filepath, "fig_enrolled_by_facility.png"),
#        width = 10, height = 7, dpi = 300)

## Table 2. Participation flow by arm ------------
## Visit pattern per HCW, e.g. "0-6-12" = present at all three visits.
## Earlier counts: 0-6-12 = 179, 0-6 = 12, 0 = 9, 6-12 = 7, 12 = 10.

flow_src <- screen_key %>%
    arrange(screen_id, wave) %>%
    group_by(screen_id) %>%
    summarise(pattern = paste(wave, collapse = "-"), .groups = "drop") %>%
    rename(pt_id = screen_id) %>%
    left_join(demog_master %>% select(pt_id, condition), by = "pt_id")

flow_src %>% count(pattern)   # check against the earlier counts

flow_tab <- bind_rows(flow_src, flow_src %>% mutate(condition = "Overall")) %>%
    group_by(condition) %>%
    summarise(
        `Enrolled at Pre-launch`      = sum(str_starts(pattern, "0")),
        `Present at all 3 visits`     = sum(pattern == "0-6-12"),
        `Left after 6-month`          = sum(pattern == "0-6"),
        `Left after Pre-launch`       = sum(pattern == "0"),
        `Joined at 6-month`           = sum(str_starts(pattern, "6")),
        `Stayed to 12-month`          = sum(pattern == "6-12"),
        `Joined at 12-month`          = sum(pattern == "12"),
        `Present at Pre-launch`       = sum(str_detect(pattern, "^0")),
        `Present at 6-month`          = sum(str_detect(pattern, "(^|-)6(-|$)")),
        `Present at 12-month`         = sum(str_detect(pattern, "12")),
        `Total HCWs ever enrolled`    = n(),
        .groups = "drop"
    ) %>%
    pivot_longer(-condition, names_to = "Stage", values_to = "n") %>%
    pivot_wider(names_from = condition, values_from = n) %>%
    select(Stage, Intervention, Control, Overall)

flow_tab %>%
    kbl(caption = "HCW participation across visits, by arm",
        align = c("l", "c", "c", "c")) %>%
    kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE) %>%
    pack_rows("Pre-launch cohort", 1, 4) %>%
    pack_rows("Joined later", 5, 7) %>%
    pack_rows("Present at each visit", 8, 10) %>%
    add_indent(c(2, 3, 4, 6)) %>%
    row_spec(11, bold = TRUE)

## Retention of the Pre-launch cohort to 12-month, by arm (%)
flow_src %>%
    filter(str_starts(pattern, "0")) %>%
    group_by(condition) %>%
    summarise(n = n(),
              retained_12m = sum(pattern == "0-6-12"),
              pct = round(100 * retained_12m / n, 1),
              .groups = "drop")
## Figure 2: who is present at each visit, by when they joined ------
## Bars = HCWs present at each visit; colour = when the HCW joined.
## A shrinking dark segment shows attrition of the Pre-launch cohort.
fig2_src <- flow_src %>%
    mutate(joined = case_when(str_starts(pattern, "0") ~ "Pre-launch",
                              str_starts(pattern, "6") ~ "Joined at 6-month",
                              TRUE                     ~ "Joined at 12-month"),
           joined = factor(joined, levels = c("Pre-launch", "Joined at 6-month",
                                              "Joined at 12-month"))) %>%
    inner_join(screen_key %>% transmute(pt_id = screen_id, wave), by = "pt_id") %>%
    mutate(wave = factor(wave, levels = c(0, 6, 12),
                         labels = c("Pre-launch", "6-month", "12-month")),
           condition = factor(condition, levels = c("Intervention", "Control"))) %>%
    count(condition, wave, joined)

fig2_tot <- fig2_src %>%
    group_by(condition, wave) %>%
    summarise(total = sum(n), .groups = "drop")

ggplot(fig2_src, aes(x = wave, y = n, fill = joined)) +
    geom_col(width = 0.6, colour = "white", linewidth = 0.4,
             position = position_stack(reverse = TRUE)) +
    geom_text(data = fig2_tot, aes(x = wave, y = total, label = total),
              inherit.aes = FALSE, vjust = -0.5, size = 3.5, colour = "grey30") +
    facet_wrap(~ condition) +
    # ordinal blues: darker = joined earlier
    scale_fill_manual(values = c("Pre-launch"         = "#1c5cab",
                                 "Joined at 6-month"  = "#5598e7",
                                 "Joined at 12-month" = "#9ec5f4")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(title = "HCWs present at each visit, by when they joined",
         x = NULL, y = "Number of HCWs", fill = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position    = "top",
          panel.grid.major.x = element_blank(),
          panel.grid.minor   = element_blank(),
          strip.text         = element_text(face = "bold"),
          plot.title         = element_text(face = "bold"))

## Table 3. Sample composition by visit ------------
## One row per HCW per visit attended (594 rows).
## Descriptive only: the same HCWs appear at several visits, so standard
## tests (chi-square, t-test) would not be valid here.

cadre_lab <- c("22" = "Lay providers", "24" = "Nurses/COs", "23" = "In-charges")

comp_src <- screen_key %>%
    transmute(pt_id = screen_id, wave, screen_date, job_visit = rct_job) %>%
    left_join(demog_master %>%
                  select(pt_id, condition, cadre_code, final_dob,
                         final_sex, final_education),
              by = "pt_id") %>%
    mutate(age_visit = time_length(interval(final_dob, screen_date), "years"),
           cadre     = factor(cadre_lab[as.character(cadre_code)],
                              levels = c("Lay providers", "Nurses/COs", "In-charges")),
           wave      = factor(wave, levels = c(0, 6, 12),
                              labels = c("Pre-launch", "6-month", "12-month")),
           condition = factor(condition, levels = c("Intervention", "Control")))

nrow(comp_src)   # 594


## Table: arm x visit
tbl_comp <- comp_src %>%
    select(condition, wave,
           `Age at visit (years)` = age_visit,
           Sex                    = final_sex,
           Cadre                  = cadre,
           `Job at visit`         = job_visit,
           `Education (at entry)` = final_education) %>%
    tbl_strata(
        strata   = condition,
        .tbl_fun = ~ .x %>%
            tbl_summary(by = wave,
                        statistic = list(all_continuous()  ~ "{mean} ({sd})",
                                         all_categorical() ~ "{n} ({p}%)"),
                        missing = "ifany")
    )

tbl_comp %>%
    as_kable_extra(caption = "Characteristics of HCWs present at each visit, by arm") %>%
    kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)


## Figure 3: cadre mix at each visit, by arm ------
## Same cadre colours as Figure 1.
fig3_src <- comp_src %>%
    count(condition, wave, cadre) %>%
    group_by(condition, wave) %>%
    mutate(pct = 100 * n / sum(n)) %>%
    ungroup()

ggplot(fig3_src, aes(x = wave, y = pct, fill = cadre)) +
    geom_col(width = 0.6, colour = "white", linewidth = 0.4,
             position = position_stack(reverse = TRUE)) +
    geom_text(aes(label = paste0(round(pct), "%")),
              position = position_stack(vjust = 0.5, reverse = TRUE),
              size = 3, colour = "white") +
    facet_wrap(~ condition) +
    scale_fill_manual(values = c("Lay providers" = "#2a78d6",
                                 "Nurses/COs"    = "#eb6834",
                                 "In-charges"    = "#1baf7a")) +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0.02))) +
    labs(title = "Cadre mix of HCWs present at each visit",
         x = NULL, y = "% of HCWs", fill = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position    = "top",
          panel.grid.major.x = element_blank(),
          panel.grid.minor   = element_blank(),
          strip.text         = element_text(face = "bold"),
          plot.title         = element_text(face = "bold"))

## Table 4. Retention of the Pre-launch cohort ------------
## Pre-launch cohort only (n = 200); late entrants excluded.
## Groups: present at all 3 visits / left after 6-month / left after Pre-launch.
## Descriptive only (21 HCWs left; too few for meaningful tests).

ret_src <- demog_master %>%
    filter(entry_wave == 0) %>%
    left_join(flow_src %>% select(pt_id, pattern), by = "pt_id") %>%
    mutate(
        retention = factor(case_when(pattern == "0-6-12" ~ "All 3 visits",
                                     pattern == "0-6"    ~ "Left after 6-month",
                                     pattern == "0"      ~ "Left after Pre-launch"),
                           levels = c("All 3 visits", "Left after 6-month",
                                      "Left after Pre-launch")),
        cadre = factor(cadre_lab[as.character(cadre_code)],
                       levels = c("Lay providers", "Nurses/COs", "In-charges"))
    )

nrow(ret_src)               # 200
ret_src %>% count(retention) # 179 / 12 / 9


## Table: baseline characteristics by retention group 
tbl_ret <- ret_src %>%
    select(retention,
           Arm                   = condition,
           Cadre                 = cadre,
           Job                   = final_job,
           `Age (years)`         = final_age,
           Sex                   = final_sex,
           Education             = final_education,
           `Terms of engagement` = final_terms,
           `Months at facility`  = final_monthswork,
           final_dpt_anc:final_dpt_other) %>%
    rename_with(~ paste0("Dept: ", str_remove(.x, "^final_dpt_")),
                starts_with("final_dpt_")) %>%
    tbl_summary(
        by      = retention,
        percent = "row",
        type    = list(starts_with("Dept:")  ~ "dichotomous",
                       `Months at facility`  ~ "continuous"),
        statistic = list(all_continuous()  ~ "{median} [{p25}, {p75}]",
                         `Age (years)`     ~ "{mean} ({sd})",
                         all_categorical() ~ "{n} ({p}%)"),
        missing = "ifany"
    ) %>%
    add_overall()

tbl_ret %>%
    as_kable_extra(caption = "Baseline characteristics of the Pre-launch cohort, by retention") %>%
    kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)

## Figure 4: retention by facility ------
## % of each facility's Pre-launch cohort present at all 3 visits.
fig4_src <- ret_src %>%
    group_by(condition, facility_code, facility_name) %>%
    summarise(n        = n(),
              retained = sum(retention == "All 3 visits"),
              pct      = 100 * retained / n,
              .groups  = "drop") %>%
    mutate(facility  = sprintf("%02d %s", facility_code, facility_name),
           facility  = factor(facility, levels = rev(sort(unique(facility)))),
           condition = factor(condition, levels = c("Intervention", "Control")))

ggplot(fig4_src, aes(x = pct, y = facility)) +
    geom_segment(aes(x = 0, xend = pct, yend = facility),
                 colour = "grey80", linewidth = 0.8) +
    geom_point(size = 3, colour = "#2a78d6") +
    geom_text(aes(label = paste0(retained, "/", n)),
              hjust = -0.4, size = 3, colour = "grey30") +
    facet_grid(condition ~ ., scales = "free_y", space = "free_y") +
    scale_x_continuous(limits = c(0, 112), breaks = seq(0, 100, 25),
                       labels = function(x) paste0(x, "%")) +
    labs(title = "Pre-launch cohort present at all 3 visits, by facility",
         x = "% retained", y = NULL) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank(),
          strip.text         = element_text(face = "bold"),
          plot.title         = element_text(face = "bold"))

# Save clean datasets -------------------

# screening_clean_df <- screening_raw_df %>% 
#     select(record_id, arm, anc_num, initial_consent_date, eligible_study, 
#            exclusion_reasons, consented, enrolled, decline_reason)

