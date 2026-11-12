# Header -----------------------------------------------------------------------

# Author(s): Dowaga
# Date: November 04, 2026
# This is a script to prepare a cleaned RCT DSM-5 Cross-CuttingSymptoms Measure 
# that's ready for analysis, manuscript writing, and sharing 

# Setup ------------------------------------------------------------------------
# Reference source codes & other dependencies:
source("Dependencies.R")
source("DataTeam_ipmh.R")
source("data_import.R")


#===============================================================================
# 1. only keep the databases we need----
#===============================================================================
rm(list = setdiff(ls(), c("ppw_rct_df", "ipmh_filepath",
                          "data_freeze", "clean_dir")))


#===============================================================================
# 2. CREATE ENROLLMENT DATASET
#===============================================================================

enrollment_df <- ppw_rct_df %>%
    mutate(
        visit_type = case_when(
            grepl("Enrollment", redcap_event_name) ~ "Enrollment",
            grepl("6 Weeks", redcap_event_name) ~ "6 Weeks",
            grepl("14 Weeks", redcap_event_name) ~ "14 Weeks",
            grepl("6 Months", redcap_event_name) ~ "6 Months",
            TRUE ~ NA_character_
        )
    ) %>%
    filter(visit_type == "Enrollment")

# Check for duplicate participants
duplicate_ids <- enrollment_df %>%
    count(clt_ptid) %>%
    filter(n > 1)


#===============================================================================
# 3. SELECT DSM-5 VARIABLES
#===============================================================================

dsm5_df <- enrollment_df %>%
    select(
        clt_ptid,
        clt_study_site,
        clt_date,
        visit_type,
        starts_with("dsm5_"),
        phq_interest,
        phq_down,
        gad7_nerve
    )

# Preserve original responses
dsm5_df_raw <- dsm5_df

#===============================================================================
# 4. IDENTIFY DSM-5 VARIABLES
#===============================================================================

copied_dsm5_cols <- c(
    "dsm5_interest",
    "dsm5_down",
    "dsm5_nervous"
)

native_dsm5_cols <- names(dsm5_df) %>%
    stringr::str_subset("^dsm5_") %>%
    setdiff(
        c(
            copied_dsm5_cols,
            "dsm5_cross_cutting_symptom_measure_complete"
        )
    )

#===============================================================================
# 5. RECODE NATIVE DSM-5 ITEMS (0-4)
#===============================================================================

dsm5_lookup <- c(
    "none (not at all)" = 0,
    "slight (less than a day or two)" = 1,
    "mild (several days)" = 2,
    "moderate (more than half the days)" = 3,
    "severe (nearly every day)" = 4
)

# Check unmatched responses

dsm5_unmatched <- dsm5_df_raw %>%
    summarise(
        across(
            all_of(native_dsm5_cols),
            ~ paste(
                setdiff(
                    unique(
                        tolower(
                            trimws(
                                as.character(.x[!is.na(.x)])
                            ))),
                    names(dsm5_lookup)
                ),
                collapse = "; "))) %>%
    pivot_longer(
        everything(),
        names_to = "variable",
        values_to = "unmatched_response"
    ) %>%
    filter(unmatched_response != "")

dsm5_unmatched

# Recode DSM-5 items

dsm5_df <- dsm5_df %>%
    mutate(
        across(
            all_of(native_dsm5_cols),
            ~ {
                x <- tolower(trimws(as.character(.x)))
                as.numeric(unname(dsm5_lookup[x]))
            }
        ))

#===============================================================================
# 6. RECODE COPIED DSM-5 ITEMS (0-3)
#===============================================================================

dsm5_df <- dsm5_df %>%
    mutate(
        across(
            all_of(copied_dsm5_cols),
            as.numeric
        )
    )

#===============================================================================
# 7. RECODE PHQ-9 ITEMS (0-3)
#===============================================================================

phq9_lookup <- c(
    "not at all" = 0,
    "several days" = 1,
    "more than half the days" = 2,
    "nearly every day" = 3
)

phq9_unmatched <- dsm5_df_raw %>%
    summarise(
        across(
            c(phq_interest, phq_down),
            ~ paste(
                setdiff(
                    unique(
                        tolower(
                            trimws(
                                as.character(.x[!is.na(.x)])
                            ))),
                    names(phq9_lookup)
                ),
                collapse = "; "
            ))) %>%
    pivot_longer(
        everything(),
        names_to = "variable",
        values_to = "unmatched_response"
    ) %>%
    filter(unmatched_response != "")

phq9_unmatched

dsm5_df <- dsm5_df %>%
    mutate(
        across(
            c(phq_interest, phq_down),
            ~ {
                x <- tolower(trimws(as.character(.x)))
                as.numeric(unname(phq9_lookup[x]))
            },
            .names = "{.col}_num"
        ))

#===============================================================================
# 8. RECODE GAD-7 ITEM (0-3)
#===============================================================================

gad7_lookup <- c(
    "not at all" = 0,
    "several days" = 1,
    "over half the days" = 2,
    "nearly every day" = 3
)


gad7_unmatched <- dsm5_df_raw %>%
    summarise(
        gad7_nerve = paste(
            setdiff(
                unique(
                    tolower(
                        trimws(
                            as.character(gad7_nerve[!is.na(gad7_nerve)])
                        )
                    )
                ),
                names(gad7_lookup)
            ),
            collapse = "; "
        ))%>%
    filter(gad7_nerve != "")


dsm5_df <- dsm5_df %>%
    mutate(
        gad7_nerve_num = {
            x <- tolower(trimws(as.character(gad7_nerve)))
            as.numeric(unname(gad7_lookup[x]))
        }
    )

#===============================================================================
# 9. VERIFY COPIED DSM-5 ITEMS MATCH PHQ/GAD ITEMS
#===============================================================================

mismatch <- dsm5_df %>%
    mutate(
        interest_mismatch =
            !is.na(phq_interest_num) &
            (is.na(dsm5_interest) |
                 dsm5_interest != phq_interest_num),
        
        down_mismatch =
            !is.na(phq_down_num) &
            (is.na(dsm5_down) |
                 dsm5_down != phq_down_num),
        
        nervous_mismatch =
            !is.na(gad7_nerve_num) &
            (is.na(dsm5_nervous) |
                 dsm5_nervous != gad7_nerve_num)
    ) %>%
    filter(
        interest_mismatch |
            down_mismatch |
            nervous_mismatch
    )

mismatch

#===============================================================================
# 10. VALIDITY CHECKS
#===============================================================================

dsm5_invalid <- dsm5_df %>%
    summarise(
        across(
            all_of(native_dsm5_cols),
            ~ sum(!is.na(.x) & !.x %in% 0:4)
        )
    ) %>%
    pivot_longer(
        everything(),
        names_to = "variable",
        values_to = "invalid_n"
    ) %>%
    filter(invalid_n > 0)

copied_invalid <- dsm5_df %>%
    summarise(
        across(
            all_of(copied_dsm5_cols),
            ~ sum(!is.na(.x) & !.x %in% 0:3)
        )
    ) %>%
    pivot_longer(
        everything(),
        names_to = "variable",
        values_to = "invalid_n"
    ) %>%
    filter(invalid_n > 0)

dsm5_invalid
copied_invalid

#===============================================================================
# 11. COMPLETENESS
#===============================================================================

dsm5_completeness <- dsm5_df %>%
    mutate(
        total_answered =
            rowSums(
                !is.na(
                    select(
                        .,
                        all_of(native_dsm5_cols)
                    )
                )
            ),
        
        total_missing =
            length(native_dsm5_cols) - total_answered,
        
        all_items_missing =
            total_answered == 0,
        
        partially_completed =
            total_answered > 0 &
            total_answered < length(native_dsm5_cols),
        
        all_items_completed =
            total_answered == length(native_dsm5_cols)
    )

dsm5_completeness_summary <- dsm5_completeness %>%
    summarise(
        total_records = n(),
        total_native_items = length(native_dsm5_cols),
        records_all_items_missing = sum(all_items_missing),
        records_with_responses = sum(total_answered > 0),
        partially_completed = sum(partially_completed),
        fully_completed = sum(all_items_completed),
        all_items_missing_pct =
            round(100 * mean(all_items_missing), 1)
    )

dsm5_completeness_summary

#===============================================================================
# 12. ITEM-LEVEL MISSINGNESS
#===============================================================================

dsm5_item_missingness <- dsm5_df %>%
    summarise(
        across(
            all_of(native_dsm5_cols),
            ~ sum(is.na(.x))
        )
    ) %>%
    pivot_longer(
        everything(),
        names_to = "variable",
        values_to = "missing_n"
    ) %>%
    mutate(
        total_records = nrow(dsm5_df),
        missing_pct =
            round(
                100 * missing_n / total_records,
                1
            )
    ) %>%
    arrange(desc(missing_n))

dsm5_item_missingness


#===============================================================================
# 13. DSM-5 DOMAIN SCORES
#===============================================================================

safe_pmax <- function(...) {
    x <- pmax(..., na.rm = TRUE)
    x[is.infinite(x)] <- NA
    x
}

dsm5_df <- dsm5_df %>%
    mutate(
        
        dsm5_depression_score =
            safe_pmax(dsm5_interest, dsm5_down),
        
        dsm5_anger_score =
            dsm5_angry,
        
        dsm5_mania_score =
            safe_pmax(
                dsm5_energy,
                dsm5_risky,
                dsm5_driven
            ),
        
        dsm5_anxiety_score =
            safe_pmax(
                dsm5_nervous,
                dsm5_panic,
                dsm5_avoid
            ),
        
        dsm5_somatic_score =
            safe_pmax(
                dsm5_aches,
                dsm5_illness
            ),
        
        dsm5_suicidal_ideation_score =
            dsm5_hurt,
        
        dsm5_psychosis_score =
            safe_pmax(
                dsm5_voices,
                dsm5_thoughts
            ),
        
        dsm5_sleep_score =
            dsm5_sleep,
        
        dsm5_memory_score =
            dsm5_memory,
        
        dsm5_repetitive_score =
            dsm5_urges,
        
        dsm5_dissociation_score =
            dsm5_detach,
        
        dsm5_personality_score =
            safe_pmax(
                dsm5_who,
                dsm5_close
            ),
        
        dsm5_substance_use_score =
            safe_pmax(
                dsm5_drink,
                dsm5_smoke,
                dsm5_drug
            )
    )

#===============================================================================
# 14. CREATE FINAL ANALYTIC DATASET
#===============================================================================

score_vars <- names(dsm5_df) %>%
    stringr::str_subset("_score$")

dsm5_clean_df <- dsm5_df %>%
    select(
        clt_ptid,
        clt_study_site,
        clt_date,
        visit_type,
        all_of(native_dsm5_cols),
        all_of(copied_dsm5_cols),
        all_of(score_vars)
    )


