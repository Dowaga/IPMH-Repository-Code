# Header -----------------------------------------------------------------------
    
    # Author(s): Dowaga
    # Date: November 04, 2026
    # This is a script to prepare a cleaned RCT HIV Care and Treatment
    # that's ready for analysis, manuscript writing, and sharing 
    
    # Setup ------------------------------------------------------------------------
# Reference source codes & other dependencies:
source("Dependencies.R")
source("DataTeam_ipmh.R")
source("data_import.R")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1. only keep the databases we need----
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rm(list = setdiff(ls(), c("ppw_rct_df", "ipmh_filepath",
                          "data_freeze", "clean_dir")))


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2. Create visit type from REDCap events
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ppw_rct_df <- ppw_rct_df %>%
    mutate(
        visit_type = case_when(
            grepl("Enrollment", redcap_event_name) ~ "Enrollment",
            grepl("6 Weeks", redcap_event_name) ~ "6 Weeks",
            grepl("14 Weeks", redcap_event_name) ~ "14 Weeks",
            grepl("6 Months", redcap_event_name) ~ "6 Months",
            TRUE ~ NA_character_
        )
    )

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. Select HIV Care & Treatment Variables and create baseline HIV status
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
hivct_df <- ppw_rct_df %>%
    group_by(clt_ptid) %>%
    mutate(
        
        # Determine enrollment HIV status once per participant
        enrollment_hiv_status = case_when(
            any(
                visit_type == "Enrollment" &
                    med_pastdiag___2 == "Checked",
                na.rm = TRUE
            ) ~ "LHIV",
            TRUE ~ "HIV-negative"
        ),
        
        # Carry enrollment status forward to all visits
        baseline_hiv_status = enrollment_hiv_status
        
    ) %>%
    ungroup() %>%
    select(
        clt_ptid,
        clt_study_site,
        clt_date,
        visit_type,
        baseline_hiv_status,
        starts_with("hivct_"),
        hiv_care_and_treatment_complete
    )

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Check enrollment HIV status
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

hivct_df %>%
    filter(visit_type == "Enrollment") %>%
    tabyl(baseline_hiv_status)

# 417 participants were enrolled living with HIV
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4. Keep all records for participants LHIV at enrollment
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

lhiv_ids <- hivct_df %>%
    filter(
        visit_type == "Enrollment",
        baseline_hiv_status == "LHIV"
    ) %>%
    pull(clt_ptid)

lhiv_data <- hivct_df %>%
    filter(clt_ptid %in% lhiv_ids)



# Check: Identify participants with conflicting HIV status or diagnosis information
status_qc <- lhiv_data %>%
    group_by(clt_ptid) %>%
    summarise(
        n_status = n_distinct(hivct_status, na.rm = TRUE),
        n_diagdate = n_distinct(hivct_knowstatus_date, na.rm = TRUE),
        n_year = n_distinct(hivct_knowstatus_year, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    filter(
        n_status > 1 |
            n_diagdate > 1 |
            n_year > 1
    )

# Retrieve all records for flagged participants
status_qc_records <- hivct_df %>%
    filter(clt_ptid %in% status_qc$clt_ptid) %>%
    select(
        clt_ptid,
        visit_type,
        hivct_status,
        hivct_knowstatus_date,
        hivct_knowstatus_year,
        hivct_knowstatus_units,
        hivct_knowstatus_num,
        clt_date
    ) %>%
    arrange(clt_ptid, clt_date)


#========================================================
# HIV DIAGNOSIS VARIABLES
#========================================================

reference_date <- Sys.Date()

lhiv_data <- lhiv_data %>%
    mutate(
        #====================================================
        # Knows HIV Diagnosis Date/Year
        #====================================================
        knows_diagnosis_date = if_else(
            is.na(hivct_knowstatus_knowdate),
            "9999",
            hivct_knowstatus_knowdate
        ))

        #====================================================
        # DATE OF HIV DIAGNOSIS
        #====================================================
lhiv_data <- lhiv_data %>%
    mutate(
        hivct_diagnosis_date = case_when(
            knows_diagnosis_date == "Yes, knows date" ~
                as.Date(hivct_knowstatus_date),
            TRUE ~ as.Date(NA)
        ))

# Check: Records where the HIV diagnosis date occurred after enrollment
lhiv_data %>%
    summarise(
        future_diagnosis_dates = sum(
            hivct_diagnosis_date > clt_date,
            na.rm = TRUE
        )
    )

        
#====================================================
        # YEAR OF HIV DIAGNOSIS
#====================================================
# i. Identify and correct inconsistent diagnosis-date knowledge responses
# Recode "No" to "Yes, knows year" where a valid calendar year
# was entered in hivct_knowstatus_num and units were recorded as "Years".

lhiv_data <- lhiv_data %>%
    mutate(
        knows_diagnosis_date = case_when(
            knows_diagnosis_date == "No" &
                hivct_knowstatus_units == "Years" &
                !is.na(suppressWarnings(
                    as.numeric(as.character(hivct_knowstatus_num))
                )) &
                between(
                    suppressWarnings(
                        as.numeric(as.character(hivct_knowstatus_num))
                    ),
                    1900,
                    as.numeric(format(Sys.Date(), "%Y"))
                ) ~ "Yes, knows year",
            
            TRUE ~ knows_diagnosis_date
        )
    )


# ii. Recover the reported calendar year
# Transfer the year from hivct_knowstatus_num to hivct_knowstatus_year
# where the diagnosis year is known but the year field is missing.
# Preserve any valid year already recorded in hivct_knowstatus_year.

lhiv_data <- lhiv_data %>%
    mutate(
        hivct_knowstatus_year = case_when(
            knows_diagnosis_date == "Yes, knows year" &
                hivct_knowstatus_units == "Years" &
                is.na(suppressWarnings(
                    as.numeric(as.character(hivct_knowstatus_year))
                )) &
                !is.na(suppressWarnings(
                    as.numeric(as.character(hivct_knowstatus_num))
                )) &
                between(
                    suppressWarnings(
                        as.numeric(as.character(hivct_knowstatus_num))
                    ),
                    1900,
                    as.numeric(format(Sys.Date(), "%Y"))
                ) ~ suppressWarnings(
                    as.numeric(as.character(hivct_knowstatus_num))
                ),
            
            TRUE ~ suppressWarnings(
                as.numeric(as.character(hivct_knowstatus_year))
            )
        )
    )


# iii. Derive the final HIV diagnosis year
# Retain the reported diagnosis year for participants who know the year.
# Assign 9999 where the diagnosis year is unknown or not reported.

lhiv_data %>%filter(knows_diagnosis_date == "Yes, knows year") %>% 
    tabyl(hivct_knowstatus_year)

lhiv_data <- lhiv_data %>%
    mutate(
        hivct_diagnosis_year = case_when(
            knows_diagnosis_date == "Yes, knows year" ~
                as.numeric(as.character(hivct_knowstatus_year)),
            
            TRUE ~ 9999
        )
    )


# iv. Quality check: Identify records with an HIV diagnosis year after enrollment
# Count records where the reported diagnosis year is later than the enrollment year.

lhiv_data %>%
    summarise(
        future_diagnosis_years = sum(
            hivct_diagnosis_year != 9999 &
                hivct_diagnosis_year > lubridate::year(clt_date),
            na.rm = TRUE
        )
    )


# Clean the reported duration or year associated with the HIV diagnosis date.
# Retain numeric responses and set invalid or missing entries to NA.

lhiv_data %>% 
    filter(knows_diagnosis_date == "No") %>% 
    tabyl(hivct_knowstatus_num)

lhiv_data <- lhiv_data %>%
    mutate(
        hivct_years_lhiv = case_when(
            knows_diagnosis_date == "No" &
                !is.na(hivct_knowstatus_num) &
                trimws(as.character(hivct_knowstatus_num)) != "" ~
                suppressWarnings(
                    as.numeric(as.character(hivct_knowstatus_num))
                ),
            
            TRUE ~ NA_real_
        )
    )
        
        # Remove implausible years
lhiv_data <- lhiv_data %>%
    mutate(hivct_diagnosis_year = if_else(
            hivct_diagnosis_year < 1980 |
                hivct_diagnosis_year > year(reference_date),
            NA_real_,
            hivct_diagnosis_year
        ))

#========================================================
# HIV DIAGNOSIS DURATION UNITS
#========================================================

# Standardize reported diagnosis-duration units.
# Codes: 1 = Months, 2 = Years, -2 = No answer.

lhiv_data <- lhiv_data %>%
    mutate(
        hivct_knowstatus_units_clean = case_when(
            hivct_knowstatus_units %in% c(1, "Months (if < 1 year)") ~ "Months",
            hivct_knowstatus_units %in% c(2, "Years") ~ "Years",
            hivct_knowstatus_units %in% c(-2, "-2", "No answer") ~ "9999",
            TRUE ~ NA_character_
        )
    )


#========================================================
# DURATION SINCE PARTICIPANT KNEW HIV STATUS
#========================================================

# Convert reported duration to numeric.
lhiv_data <- lhiv_data %>%
    mutate(
        hivct_knowstatus_num_clean = suppressWarnings(
            as.numeric(as.character(hivct_knowstatus_num))
        )
    )


# Duration in months: applicable only when Months was selected.
# Accept durations from 0 to 11 months.

lhiv_data <- lhiv_data %>%
    mutate(
        hivct_months_lhiv = case_when(
            knows_diagnosis_date == "No" &
                hivct_knowstatus_units_clean == "Months" &
                !is.na(hivct_knowstatus_num_clean) &
                between(hivct_knowstatus_num_clean, 0, 11) ~
                hivct_knowstatus_num_clean,
            TRUE ~ NA_real_
        )
    )


# Duration in years: applicable only when Years was selected.
# Exclude calendar-year entries and missing-value codes.

lhiv_data <- lhiv_data %>%
    mutate(
        hivct_years_lhiv = case_when(
            knows_diagnosis_date == "No" &
                hivct_knowstatus_units_clean == "Years" &
                !is.na(hivct_knowstatus_num_clean) &
                hivct_knowstatus_num_clean > 0 &
                hivct_knowstatus_num_clean != 9999 &
                !between(
                    hivct_knowstatus_num_clean,
                    1900,
                    as.numeric(format(Sys.Date(), "%Y"))
                ) ~ hivct_knowstatus_num_clean,
            TRUE ~ NA_real_
        )
    )


# QC: Review unusually large reported durations in years

lhiv_data %>%
    summarise(
        max_duration_years = max(hivct_years_lhiv, na.rm = TRUE),
        max_duration_months = max(hivct_months_lhiv, na.rm = TRUE)
    )

#========================================================
# ESTIMATED HIV DIAGNOSIS DATE
#========================================================

# Estimate diagnosis date using participant enrollment date (clt_date)
# as the reference date for reported duration.
#
# Priority:
# 1. Known diagnosis year: use 30 June of the reported year.
# 2. Reported duration in months: subtract months from enrollment date.
# 3. Reported duration in years: subtract years from enrollment date.
# 4. Otherwise, retain missing.

lhiv_data <- lhiv_data %>%
    mutate(
        clt_date = as.Date(clt_date),
        
        hivct_diagnosis_estdate = case_when(
            
            # Known diagnosis year
            !is.na(hivct_diagnosis_year) &
                hivct_diagnosis_year != 9999 ~
                lubridate::make_date(
                    year = as.integer(hivct_diagnosis_year),
                    month = 6,
                    day = 30
                ),
            
            # Known duration in months
            !is.na(hivct_months_lhiv) &
                !is.na(clt_date) ~
                lubridate::add_with_rollback(
                    clt_date,
                    lubridate::period(
                        month = -round(hivct_months_lhiv)
                    )
                ),
            
            # Known duration in years
            !is.na(hivct_years_lhiv) &
                !is.na(clt_date) ~
                lubridate::add_with_rollback(
                    clt_date,
                    lubridate::period(
                        year = -round(hivct_years_lhiv)
                    )
                ),
            
            TRUE ~ as.Date(NA)
        )
    )

# QC: Identify estimated diagnosis dates before enrollment
# or dates earlier than 1980.

negative_date_qc <- lhiv_data %>%
    filter(
        !is.na(hivct_diagnosis_estdate) &
            (hivct_diagnosis_estdate > clt_date |
                 hivct_diagnosis_estdate < as.Date("1980-01-01"))
    ) %>%
    select(
        clt_ptid,
        clt_date,
        knows_diagnosis_date,
        hivct_knowstatus_units,
        hivct_knowstatus_num,
        hivct_diagnosis_year,
        hivct_months_lhiv,
        hivct_years_lhiv,
        hivct_diagnosis_estdate
    )


#========================================================
# FINAL HIV DIAGNOSIS DATE
#========================================================
# Prioritize the actual diagnosis date over the estimated date.
# Propagate the available diagnosis date across participant visits.

lhiv_data <- lhiv_data %>%
    group_by(clt_ptid) %>%
    mutate(
        hivct_diagnosis_known_est_date = coalesce(
            hivct_diagnosis_date,
            hivct_diagnosis_estdate
        )
    ) %>%
    fill(
        hivct_diagnosis_known_est_date,
        .direction = "downup"
    ) %>%
    ungroup()

#========================================================
# ART VARIABLES
#========================================================
lhiv_data %>% 
    tabyl(hivct_artstart_preg)

# Retain the original response on ART during pregnancy.
lhiv_data <- lhiv_data %>%
    mutate(
        hivct_artstart_preg_clean = hivct_artstart_preg
    )


enr_artstart_qc <- lhiv_data %>% 
    filter(visit_type == "Enrollment") %>% 
    filter(is.na(hivct_artstart_preg))

# Retain the original response regarding knowledge of ART start date.
lhiv_data <- lhiv_data %>%
    mutate(
        hivct_artstart_knowdate_clean = hivct_artstart_knowdate
    )


# Check: Identify participants with conflicting art start status or art start dates
artstart_qc <- lhiv_data %>%
    group_by(clt_ptid) %>%
    summarise(
        n_artstart = n_distinct(hivct_artstart_preg, na.rm = TRUE),
        n_artstart_date = n_distinct(hivct_artstart_knowdate, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    filter(
        n_artstart > 1 |
            n_artstart_date > 1 
    )

# Retrieve all records for flagged participants
artstart_qc_records <- hivct_df %>%
    filter(clt_ptid %in% artstart_qc$clt_ptid) %>%
    select(
        clt_ptid,
        visit_type,
        hivct_artstart_preg,
        hivct_artstart_knowdate,
        clt_date
    ) %>%
    arrange(clt_ptid, clt_date)


#========================================================
# ART START DATE
#========================================================

# Convert the reported ART start date to Date format.
# Dates that cannot be parsed are set to missing.

lhiv_data <- lhiv_data %>%
    mutate(
        hivct_artstart_date_clean = case_when(
            hivct_artstart_knowdate == "Yes, knows date" ~
                as.Date(hivct_artstart_date),
            TRUE ~ as.Date(NA)
        ))


#========================================================
# ART START YEAR
#========================================================

lhiv_data %>%
    filter(hivct_artstart_knowdate == "Yes, knows year") %>% 
    tabyl(hivct_artstart_year)


lhiv_data <- lhiv_data %>%
    mutate(
        hivct_artstart_year_clean = if_else(
            hivct_artstart_knowdate == "Yes, knows year" &
                hivct_artstart_year != 9999,
            as.numeric(as.character(hivct_artstart_year)),
            NA_real_
        )
    )

lhiv_data <- lhiv_data %>%
    mutate(
        hivct_artstart_year_clean = case_when(
            hivct_artstart_knowdate == "Yes, knows year" &
                !is.na(hivct_artstart_year) &
                hivct_artstart_year != 9999 &
                between(
                    as.numeric(as.character(hivct_artstart_year)),
                    1980,
                    lubridate::year(reference_date)
                ) ~
                as.numeric(as.character(hivct_artstart_year)),
            TRUE ~ NA_real_
        )
    )

#========================================================
# ART DURATION UNIT
#========================================================

# Standardize ART duration units.
# Code 1 = Months; Code 2 = Years.

lhiv_data <- lhiv_data %>%
    mutate(
        hivct_artduration_unit_clean = case_when(
            hivct_artduration_unit %in% c(1, "1", "Months") ~ "Months",
            hivct_artduration_unit %in% c(2, "2", "Years") ~ "Years",
            TRUE ~ NA_character_
        )
    )


#========================================================
# ART DURATION NUMBER ----
#========================================================


#========================================================
# MONTHS ON ART
#========================================================

# Derive ART duration in months only when Months was selected.
# Exclude missing, negative, and sentinel values.

lhiv_data <- lhiv_data %>%
    mutate(
        hivct_artduration_months = case_when(
            hivct_artstart_knowdate == "No" &
                hivct_artduration_unit_clean == "Months" &
                !is.na(hivct_artduration_num) &
                hivct_artduration_num != 9999 ~
                hivct_artduration_num,
            TRUE ~ NA_real_
        )
    )


#========================================================
# YEARS ON ART
#========================================================

# Derive ART duration in years only when Years was selected.
# Exclude calendar-year entries and invalid durations.

lhiv_data <- lhiv_data %>%
    mutate(
        hivct_artduration_years = case_when(
            hivct_artstart_knowdate == "No" &
            hivct_artduration_unit_clean == "Years" &
                !is.na(hivct_artduration_num) &
                hivct_artduration_num> 0 &
                hivct_artduration_num!= 9999 &
                !between(
                    hivct_artduration_num,
                    1900,
                    lubridate::year(reference_date)
                ) ~
                hivct_artduration_num,
            TRUE ~ NA_real_
        )
    )



#========================================================
# ESTIMATED ART START DATE----
#========================================================

# Estimate ART start date using enrollment date (clt_date)
# as the reference for reported duration.
#
# Priority:
# 1. Known ART start year
# 2. Reported ART duration in months
# 3. Reported ART duration in years
# 4. Otherwise, missing

lhiv_data <- lhiv_data %>%
    mutate(
        hivct_artstart_estdate = case_when(
            
            # Known ART start year
            !is.na(hivct_artstart_year_clean) ~
                lubridate::make_date(
                    year = as.integer(hivct_artstart_year_clean),
                    month = 6,
                    day = 30
                ),
            
            # Known duration in months
            !is.na(hivct_artduration_months) &
                !is.na(clt_date) ~
                lubridate::add_with_rollback(
                    clt_date,
                    lubridate::period(
                        month = -round(hivct_artduration_months)
                    )
                ),
            
            # Known duration in years
            !is.na(hivct_artduration_years) &
                !is.na(clt_date) ~
                lubridate::add_with_rollback(
                    clt_date,
                    lubridate::period(
                        year = -round(hivct_artduration_years)
                    )
                ),
            
            TRUE ~ as.Date(NA)
        )
    )


#========================================================
# FINAL ART START DATE----
#========================================================

# Prioritize the actual ART start date over the estimated date.

lhiv_data <- lhiv_data %>%
    mutate(
        hivct_artstart_known_est_date = coalesce(
            hivct_artstart_date_clean,
            hivct_artstart_estdate
        )
    )

#========================================================
# HIV CARE VARIABLES
#========================================================

# Retain the original HIV care response.
lhiv_data <- lhiv_data %>%
    mutate(
        hivct_hivcare_clean = hivct_hivcare
    )


# Clean HIV care facility name.
# Convert to character, remove leading/trailing spaces,
# and set empty strings to missing.

lhiv_data <- lhiv_data %>%
    mutate(
        hivct_hivcare_fac_clean = trimws(
            as.character(hivct_hivcare_fac)
        )
    )


# Clean other HIV care facility specification.
# Convert to character, remove leading/trailing spaces,
# and set empty strings to missing.

lhiv_data <- lhiv_data %>%
    mutate(
        hivct_hivcare_fac_other_clean = trimws(
            as.character(hivct_hivcare_fac_other)
        )
    )

#========================================================
# ART DURATION BEFORE/DURING PREGNANCY
#========================================================

# Retain ART duration categories only for participants who
# started ART before pregnancy or during pregnancy.
# Set responses for other ART initiation categories to missing.

lhiv_data <- lhiv_data %>%
    mutate(
        hivct_art_lngth_clean = case_when(
            hivct_artstart_preg %in% c("Before pregnancy", "During pregnancy") &
                hivct_art_lngth %in% c("< 6mo", "6 mo to 1 yr", ">1 yr") ~
                as.character(hivct_art_lngth),
            TRUE ~ NA_character_
        )
    )

#========================================================
# CURRENT ART REGIMEN
#========================================================

lhiv_data <- lhiv_data %>%
    mutate(
        art_current_regimen = case_when(
            hivct_artstart_preg %in% c("Before pregnancy", "During pregnancy") &
                hivct_artreg_curr %in% c("TDF/ 3TC/ DTG","TDF/ 3TC/ EFV", 
                                         "3TC/ TDF/ ATVr", "3TC/ TDF/ Kaletra",
                                         "ABC/ 3TC/ ATVr", "3TC/ AZT/ ATVr", 
                                         "Other") ~
                as.character(hivct_artreg_curr),
            TRUE ~ NA_character_
        )
    )

#========================================================
# OTHER CURRENT ART REGIMEN
#========================================================
lhiv_data %>% 
    filter(hivct_artreg_curr == "Other") %>% 
    tabyl(hivct_artreg_other_curr)

lhiv_data <- lhiv_data %>%
    mutate(
        other_current_regimens = case_when(
            hivct_artreg_curr == "Other" &
                !is.na(hivct_artreg_other_curr) ~
                trimws(hivct_artreg_other_curr),
            TRUE ~ NA_character_
        )
    )

#========================================================
# SWITCHED ART REGIMENS
# Did participant switch ART regimens in the past year
# 1 = Yes
# 0 = No
#========================================================

lhiv_data %>% 
    filter(hivct_artstart_preg %in% 
               c("Before pregnancy", "During pregnancy")) %>% 
    tabyl(hivct_art_switch)


lhiv_data <- lhiv_data %>%
    mutate(
        hivct_art_switch_clean = case_when(
            hivct_artstart_preg %in% c("Before pregnancy", "During pregnancy") &
                hivct_art_switch %in% c("Yes", "No") ~
                if_else(hivct_art_switch == "Yes", 1, 0),
            TRUE ~ NA_real_
        )
    )

#========================================================
# DATE OF ART SWITCH
#========================================================

lhiv_data <- lhiv_data %>%
    mutate(
        hivct_art_switch_date_clean = case_when(
            hivct_art_switch == "Yes" ~
                suppressWarnings(lubridate::dmy(hivct_art_switch_date)),
            TRUE ~ as.Date(NA)
        )
    )

#========================================================
# REGIMEN CLASS SWITCHED TO
#========================================================
lhiv_data %>% 
    filter(hivct_art_switch == "Yes") %>% 
    tabyl(hivct_artswit_regimen)


lhiv_data <- lhiv_data %>%
    mutate(
        hivct_regimen_swithto = case_when(
            hivct_art_switch == "Yes" &
                hivct_artswit_regimen %in% 
                c("NRTI", "INSTI", "NNRTI","PI","DTG") ~
                hivct_artswit_regimen,
            TRUE ~ NA_character_
        )
    )

#========================================================
# REASON FOR REGIMEN CHANGE
#========================================================
lhiv_data %>%
    filter(hivct_art_switch == "Yes") %>% 
    tabyl(hivct_art_switch_reason)

lhiv_data <- lhiv_data %>%
    mutate(
        hivct_art_switch_reason_clean = case_when(
            hivct_art_switch == 1 &
                hivct_art_switch_reason %in% 
                c("Drug resistance", "Recommendation in pregnancy", 
                  "Side effects", "Don't know", "Other") ~
                hivct_art_switch_reason,
            TRUE ~ NA_character_
        )
    )

#========================================================
# OTHER REASON FOR REGIMEN CHANGE
#========================================================
lhiv_data %>% 
    filter(hivct_art_switch_reason == "Other") %>% 
    tabyl(hivct_swreasonsp)

lhiv_data <- lhiv_data %>%
    mutate(
        hivct_swreasonsp_clean = case_when(
            hivct_art_switch_reason == -1 &
                !is.na(hivct_swreasonsp) ~
                trimws(hivct_swreasonsp),
            TRUE ~ NA_character_
        )
    )

#========================================================
# MOST RECENT VIRAL LOAD
#========================================================

lhiv_data <- lhiv_data %>%
    mutate(
        hivct_vl_clean = case_when(
            (hivct_status == "Yes"| hivct_newdiag == "Yes") &
                !is.na(hivct_vl) &
                hivct_vl != 99 ~
                as.numeric(hivct_vl),
            TRUE ~ NA_real_
        )
    )

#========================================================
# VIRAL LOAD DATE
#========================================================

lhiv_data <- lhiv_data %>%
    mutate(
        hivct_date_vl_clean = case_when(
            !is.na(hivct_vl_clean) ~
                suppressWarnings(lubridate::dmy(hivct_date_vl)),
            TRUE ~ as.Date(NA)
        )
    )


# Retain the original HIV disclosure response to partner.

lhiv_data <- lhiv_data %>%
    mutate(
        hivct_hiv_discl_partner_clean = hivct_hiv_discl_partner
    )

# Check: Identify participants with conflicting disclosure status
disclosure_status_qc <- lhiv_data %>%
    group_by(clt_ptid) %>%
    summarise(
        n_discl_status = n_distinct(hivct_hiv_discl_partner, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    filter(
        n_discl_status > 1
    )

# Retrieve all records for flagged participants
disclosure_status_qc_records <- hivct_df %>%
    filter(clt_ptid %in% disclosure_status_qc$clt_ptid) %>%
    select(
        clt_ptid,
        visit_type,
        hivct_hiv_discl_partner,
        clt_date
    ) %>%
    arrange(clt_ptid, clt_date)


#========================================================
# FINAL HIV CARE AND TREATMENT ANALYSIS DATASET
#========================================================

hiv_care_treat_clean <- lhiv_data %>%
    transmute(
        
        #====================================================
        # IDENTIFIERS
        #====================================================
        
        clt_ptid,
        visit_type,
        
        
        #====================================================
        # HIV STATUS
        #====================================================
        
        hivct_status,
        
        
        #====================================================
        # HIV DIAGNOSIS
        #====================================================
        
        # Final diagnosis date (actual or estimated)
        hiv_diagnosis_date =
            hivct_diagnosis_known_est_date,
        
        # Source of HIV diagnosis date
        hiv_diagnosis_date_source = case_when(
            !is.na(hivct_diagnosis_date) ~ "Reported date",
            !is.na(hivct_diagnosis_estdate) ~ "Estimated",
            TRUE ~ NA_character_
        ),
        
        # Diagnosis year
        hiv_diagnosis_year =
            hivct_diagnosis_year,
        
        # Duration since HIV diagnosis, in months
        hiv_diagnosis_months =
            hivct_months_lhiv,
        
        # Duration since HIV diagnosis, in years
        hiv_diagnosis_years =
            hivct_years_lhiv,
        
        
        #====================================================
        # ART
        #====================================================
        
        # Final ART start date (actual or estimated)
        art_start_date =
            hivct_artstart_known_est_date,
        
        # Source of ART start date
        art_start_date_source = case_when(
            !is.na(hivct_artstart_date_clean) ~ "Reported date",
            !is.na(hivct_artstart_estdate) ~ "Estimated",
            TRUE ~ NA_character_
        ),
        
        # ART start year
        art_start_year =
            hivct_artstart_year_clean,
        
        # ART duration in months
        art_duration_months =
            hivct_artduration_months,
        
        # ART duration in years
        art_duration_years =
            hivct_artduration_years,
        
        
        #====================================================
        # HIV CARE
        #====================================================
        
        # Whether participant is currently receiving HIV care
        hiv_care_location =
            hivct_hivcare_clean,
        
        # HIV care facility
        hiv_care_facility =
            hivct_hivcare_fac_clean,
        
        # Other HIV care facility
        hiv_care_facility_other =
            hivct_hivcare_fac_other_clean,
        
        
        #====================================================
        # HIV DISCLOSURE
        #====================================================
        
        # HIV disclosure to partner
        hiv_disclosed_partner =
            hivct_hiv_discl_partner_clean
    )
