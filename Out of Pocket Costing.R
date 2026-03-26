# Author(s): Dowaga
# Date: March 17, 2026
# This is a script that analyzes the Out of Pocket Costing Data.

# Setup ------------------------------------------------------------------------
rm(list = ls())
# Reference source codes & other dependencies:
source("dependencies.R")
source("DataTeam_ipmh.R")
source("data_import.R")

# load data
ppw_rct_df <- ppw_rct_df %>%
    mutate(
        visit_type = case_when(
            grepl("Enrollment", redcap_event_name) ~ "Enrollment",
            grepl("6 Weeks", redcap_event_name) ~ "6 Weeks",
            grepl("14 Weeks", redcap_event_name) ~ "14 Weeks", 
            grepl("6 Months", redcap_event_name) ~ "6 Months",
            TRUE ~ NA_character_
        ),
        arm = case_when(
            grepl("Arm 1: Intervention", redcap_event_name) ~ "Intervention",
            grepl("Arm 2: Control", redcap_event_name) ~ "Control",
            TRUE ~ "Unknown"
        )
    )

out_pocket <- ppw_rct_df %>% 
    select(clt_ptid,arm, transport_means, specific_transport,
           travel_distance, hours, minutes, travel_fare,
           transport_back, transport_means_back, specific_transport_back,
           hours_back, minutes_back, travel_back_fare,
           travel_fare_back, missed_opportunity, cost_employment, 
           payment_period, monthly_pay, weekly_pay, daily_pay, 
           current_inc_source, payment_period2, monthly_pay2, weekly_pay2,
           daily_pay2, time_off, off_duration, off_loss, loss_amount, 
           companion_cost, accompanied_by, companion, companion_off, 
           companion_job,companion_off_duration, companion_loss, 
           care_provision, care_duration, paid_caretaker, care_cost,
           service_cost, service_type___1,service_type___2, service_type___3,
           service_type___4, registration_cost, triage_cost, 
           consumable_cost, specify_service, other_costs, 
           medication_prescribtion, medication1, 
           medication2, medication3, medication4, medication5, 
           medication1_duration, medication2_duration, medication3_duration,
           medication4_duration, medication5_duration, medication1_cost,
           medication2_cost, medication3_cost, medication4_cost, 
           medication5_cost, medication_duration, circumstances_change, 
           change1, change2, change3, change4, change5, 
           change1_cost, change2_cost, change3_cost, change4_cost, 
           change5_cost, insurance_plan, plan_type, cost_payment, 
           patient_cost_questionnaire_complete) %>% 
    filter(patient_cost_questionnaire_complete == "Complete")


# Patient transport Cost----
out_pocket <- out_pocket %>%
    mutate(
        transport_means = case_when(
            transport_means == "Others {specific_transport}" & specific_transport == "Matatu " ~ "Matatu",
            transport_means == "Others {specific_transport}" & specific_transport == "Tuktuk " ~ "TukTuk",
            TRUE ~ transport_means
        ),
        specific_transport_back = str_trim(specific_transport_back),  # remove spaces
            transport_means_back = case_when(
                transport_means_back == "Others {specific_transport_back}" & specific_transport_back == "Tuktuk" ~ "Tuktuk",
            transport_means_back == "Others {specific_transport_back}" & is.na(specific_transport_back) ~ "uncertain",
            TRUE ~ transport_means_back
        )
    ) %>% 
    mutate(
        travel_distance = str_trim(as.character(travel_distance)),
        travel_distance = as.numeric(str_extract(travel_distance, "\\d*\\.?\\d+")),
        travel_fare = if_else(
            str_detect(travel_fare, "^\\d*\\.?\\d+$"),
            as.numeric(travel_fare),
            NA_real_
        )
    ) %>% 
    mutate(
        time_to_facility_min = case_when(
            is.na(hours) & !is.na(minutes) ~ minutes,                # Only minutes available
            !is.na(hours) & is.na(minutes) ~ hours * 60,             # Only hours available
            !is.na(hours) & !is.na(minutes) ~ hours * 60 + minutes,  # Both available
            TRUE ~ NA_real_                                          # Both missing
        ),
        time_from_facility_min = case_when(
            is.na(hours_back) & !is.na(minutes_back) ~ minutes,                # Only minutes available
            !is.na(hours_back) & is.na(minutes_back) ~ hours * 60,             # Only hours available
            !is.na(hours_back) & !is.na(minutes_back) ~ hours * 60 + minutes,  # Both available
            TRUE ~ NA_real_                                          # Both missing
        )
    )


patient_cost <- out_pocket %>% 
    tbl_summary(include = c(transport_means,travel_distance,
                             time_to_facility_min, travel_fare,  
                            transport_back, transport_means_back,
                            time_from_facility_min),
    type = all_continuous() ~ "continuous",
    statistic = list(
        all_continuous() ~ "{mean} ({sd})",
        all_categorical() ~ "{n} ({p}%)"
    ),
    percent = "cell",
    missing = "no",
    digits = list(
        all_continuous() ~ 1,       # continuous variables ??? 1 d.p.
        all_categorical() ~ c(0, 1) # categorical ??? 0 decimals for n, 1 d.p. for %
    ),
    label = list(
        transport_means = "Mode of Transport",
        travel_distance = "Distance to Facility (km)",
        time_to_facility_min ~ "Time to Facility (Min)",
        travel_fare = "Transport Cost (KES)",
        transport_back = "Same Mode of Transport for Return Trip (Yes)",
        transport_means_back = "Mode of Transport for Return Trip",
        time_from_facility_min = "Approximate Time back from Facility (Min)"
),
    sort = list(all_categorical() ~ "frequency")# Sort categorical levels by frequency in descending order
) %>% 
    bold_labels() %>% 
    italicize_levels()%>% 
    gtsummary::as_gt() %>% 
    gt::tab_header(
        title = "Transport and Travel Cost Summary",
        subtitle = "Summary of Transport Mode, Distance, and Costs Among Participants"
    ) %>%
    gt::tab_options(
        table.font.size = gt::px(12),
        data_row.padding = gt::px(2)
    )

patient_cost

# Productivity losses----
productivity_loses <- out_pocket %>% 
    mutate(
        across(
            ends_with("_pay"),
            ~ as.numeric(str_extract(as.character(.), "\\d+\\.?\\d*"))
        ),
        current_inc_source = str_squish(str_to_lower(current_inc_source)),
            inc_source_clean = case_when(
                is.na(current_inc_source) ~ "No Income",
                current_inc_source == "0" ~ "No Income",
                str_detect(current_inc_source, "none|no income|not doing|student|no job") ~ "No Income",
                str_detect(current_inc_source, "housewife|house wife") ~ "Homemaker",
                str_detect(current_inc_source, "partner|husband|spouse|grandmother") ~ "Dependent on Others",
                str_detect(current_inc_source, "farm|farmer|farming|farm work") ~ "Farming",
                str_detect(current_inc_source, "business|vendor|trader|selling|saloon|self employed|shop") ~ "Business / Self-employed",
                str_detect(current_inc_source, "casual|labour|laundry|chores") ~ "Casual Work",
                str_detect(current_inc_source, "pharmacist|tailoring") ~ "Skilled Work",
                TRUE ~ "Other"
            )
        ) %>% 
    mutate(
        # Clean original pay variables
            loss_amount = as.numeric(str_extract(as.character(loss_amount), "\\d+\\.?\\d*")),
            monthly_pay = as.numeric(str_extract(as.character(monthly_pay), "\\d+\\.?\\d*")),
            weekly_pay  = as.numeric(str_extract(as.character(weekly_pay), "\\d+\\.?\\d*")),
            daily_pay   = as.numeric(str_extract(as.character(daily_pay), "\\d+\\.?\\d*")),
            companion_off_duration = as.numeric(str_extract(as.character(companion_off_duration), "\\d+\\.?\\d*"))
        )%>% 
        
        # Standardize to monthly income
        mutate(
            income_monthly_est = case_when(
                !is.na(monthly_pay) ~ monthly_pay,
                !is.na(weekly_pay)  ~ weekly_pay * 4,
                !is.na(daily_pay)   ~ daily_pay * 30,
                TRUE ~ NA_real_
            )
        ) %>% 
    mutate(
        # If companion_cost is NA or "No", set all companion fields to NA
        accompanied_by   = if_else(is.na(companion_cost) | companion_cost == "No",
                                   NA_character_, accompanied_by),
        companion_off    = if_else(is.na(companion_cost) | companion_cost == "No",
                                   NA_character_, companion_off),
        companion_job    = if_else(is.na(companion_cost) | companion_cost == "No",
                                   NA_character_, companion_job),
        companion_loss   = if_else(is.na(companion_cost) | companion_cost == "No",
                                   NA_real_, companion_loss)
    ) %>% 
    mutate(job_group = case_when(
        companion_cost == "Yes" & str_detect(companion_job, regex("Not employed|No job|None", ignore_case = TRUE)) ~ "Unemployed/None",
        companion_cost == "Yes" & str_detect(companion_job, regex("Business", ignore_case = TRUE)) ~ "Business",
        companion_cost == "Yes" & str_detect(companion_job, regex("Casual labourer", ignore_case = TRUE)) ~ "Casual labour",
        companion_cost == "Yes" & str_detect(companion_job, regex("Farm work", ignore_case = TRUE)) ~ "Farm work",
        companion_cost == "Yes" & str_detect(companion_job, regex("Motorcycle rider", ignore_case = TRUE)) ~ "Transport",
        companion_cost == "Yes" & str_detect(companion_job, regex("Security officer", ignore_case = TRUE)) ~ "Security",
        companion_cost == "Yes" & str_detect(companion_job, regex("Student", ignore_case = TRUE)) ~ "Student",
        TRUE ~ NA_character_
    ),
    payment_period = case_when(
        str_detect(payment_period, regex("Per month", ignore_case = TRUE)) ~ "Monthly",
        str_detect(payment_period, regex("Per day", ignore_case = TRUE)) ~ "Daily",
        TRUE ~ NA_character_))



productivity_loses_summary <- productivity_loses %>% 
    tbl_summary(
        include = c(
            missed_opportunity, cost_employment, payment_period,
            monthly_pay, daily_pay, 
            inc_source_clean, time_off, off_loss, loss_amount, 
            companion_cost, accompanied_by, companion_off, job_group,
            companion_off_duration, companion_loss
        ),
        type = list(
            monthly_pay        ~ "continuous",
            daily_pay         ~ "continuous",
            companion_loss ~ "continuous",
            companion_off_duration ~ "continuous"
        ),
        statistic = list(
            all_continuous() ~ "{mean} ({sd})",
            all_categorical() ~ "{n} ({p}%)"
        ),
        digits = list(
            all_continuous() ~ 1,       # 1 decimal for continuous variables
            all_categorical() ~ c(0, 1) # 0 decimals for n, 1 for %
        ),
        missing = "no",
        label = list(
            missed_opportunity = "Activity Foregone to Attend Clinic",
            cost_employment = "Currently Employed(Yes)",
            inc_source_clean = "Current Source of Income",
            time_off = "Took Time Off for Visit (Yes)",
            off_loss = "Lost Pay for Time Off (Yes)",
            payment_period ~ "Payment Period",
            monthly_pay ~ "Monthly Income (KES)",
            daily_pay   ~ "Daily Income (KES)",
            loss_amount ~ "Estimated Pay Lost (KShs)",
            companion_cost ~ "Accompanied to Hospital",
            accompanied_by ~ "Who Accompanied You",
            companion_off  ~ "Did Companion Take Time Off from Work? (Yes)",
            job_group ~ "Companion Occupation",
            companion_off_duration ~ "Hours Taken Off (Companion)",
            companion_loss ~ "Approximate Loss Due to Accompaniment (Ksh)"),
        sort = list(all_categorical() ~ "frequency") # Sort categorical levels by frequency
    ) %>% 
    bold_labels() %>% 
    add_n() %>% 
    italicize_levels() %>% 
    gtsummary::as_gt() %>% 
    gt::tab_header(
        title = "Economic and Productivity Burden of Clinic Attendance",
        subtitle = "Patient and companion costs, time off work, and income lost"
    ) %>%
    gt::tab_options(
        table.font.size = gt::px(12),
        data_row.padding = gt::px(2)
    )

productivity_loses_summary

# Childcare and other dependent care costs
child_care <- out_pocket %>%
    select(care_provision, care_duration,
           paid_caretaker, care_cost, service_cost, service_type___1, 
           service_type___2, service_type___3, service_type___4, 
           triage_cost, consumable_cost, medication_prescribtion, 
           medication1, medication2, medication3, medication4,
           medication1_duration, medication2_duration, medication3_duration,
           medication4_duration, medication1_cost, medication2_cost,
           medication3_cost, medication4_cost, medication_duration, 
           circumstances_change,change1, change1_cost, insurance_plan, plan_type, cost_payment) %>% 
    mutate(
        care_duration = str_trim(as.character(care_duration)),
        hours   = as.numeric(str_extract(care_duration, "\\d+(?=\\s*h|\\s*hour)")),
        minutes = as.numeric(str_extract(care_duration, "\\d+(?=\\s*m|min)")),
        care_duration_clean = case_when(
            is.na(hours) & !is.na(minutes) ~ minutes / 60,                # Only minutes ??? convert to hours
            !is.na(hours) & is.na(minutes) ~ hours,                       # Only hours
            !is.na(hours) & !is.na(minutes) ~ hours + minutes / 60,       # Hours + minutes
            TRUE ~ NA_real_
        )
    ) %>% 
    mutate(
        paid_caretaker = case_when(
            care_provision == "No" ~ NA,
            TRUE ~ paid_caretaker
        )
    ) 


# a function to clean medication columns
clean_med <- function(x) {
    x_clean <- x %>%
        stringr::str_to_lower() %>%
        stringr::str_trim() %>%
        stringr::str_remove_all("p\\.o|mls?|mg|bd|tds|hs|dt\\.|syrp|syrup|cream") %>%
        stringr::str_squish()
    
    dplyr::case_when(
        stringr::str_detect(x_clean, "cetrizine|cetirizine") ~ "cetirizine",
        stringr::str_detect(x_clean, "loratide|loratidine|loratadine") ~ "loratadine",
        stringr::str_detect(x_clean, "amoxyl|amoxicillin") ~ "amoxicillin",
        stringr::str_detect(x_clean, "floxacillin|flucloxacillin") ~ "flucloxacillin",
        stringr::str_detect(x_clean, "metronidazole|flagyl") ~ "metronidazole",
        stringr::str_detect(x_clean, "bruffen|ibuprofen") ~ "ibuprofen",
        stringr::str_detect(x_clean, "pcm|paracetamol") ~ "paracetamol",
        stringr::str_detect(x_clean, "piriton|chlorpheniramine") ~ "chlorpheniramine",
        stringr::str_detect(x_clean, "fluconazon|fluconazole") ~ "fluconazole",
        stringr::str_detect(x_clean, "xtraderm") ~ "xtraderm",
        TRUE ~ x_clean  
    )
}
# apply the function to all medication columns
child_care <- child_care %>%
    mutate(across(
        c(medication1, medication2, medication3, medication4),
        clean_med
    ))

# Function to clean medication duration column
clean_duration <- function(x) {
    x_clean <- x %>%
        str_to_lower() %>%
        str_trim()
    
    num <- str_extract(x_clean, "\\d+") %>% as.numeric()
    
    case_when(
        str_detect(x_clean, "week") ~ num * 7,
        TRUE ~ num
    )
}

child_care <- child_care %>%
    mutate(across(
        c(medication1_duration, medication2_duration, medication3_duration, 
          medication4_duration, medication_duration),
        clean_duration))

child_care_summary <- child_care %>% tbl_summary(
        include = c(care_provision, care_duration_clean,
                    paid_caretaker, care_cost, service_cost,
                    medication_prescribtion, medication1, 
                    medication2, medication3, medication4,
                    medication1_duration, medication2_duration, 
                    medication3_duration, medication4_duration,
                    medication1_cost, medication2_cost,
                    medication3_cost, medication4_cost, medication_duration,
                    circumstances_change, change1, change1_cost, 
                    insurance_plan, plan_type, cost_payment),
        type = list(
            care_cost ~ "continuous",
            care_duration_clean ~ "continuous",
            medication_duration ~ "continuous",
            medication1_duration ~ "continuous",
            medication2_duration ~ "continuous",
            medication3_duration ~ "continuous",
            medication4_duration ~ "continuous",
            medication1_cost ~ "continuous",
            medication2_cost ~ "continuous",
            medication3_cost ~ "continuous",
            medication4_cost ~ "continuous"
        ),
        statistic = list(
            all_continuous() ~ "{mean} ({sd})",
            all_categorical() ~ "{n} ({p}%)"
        ),
        digits = list(
            all_continuous() ~ 1,       # 1 decimal for continuous variables
            all_categorical() ~ c(0, 1) # 0 decimals for n, 1 for %
        ),
        missing = "no",
        label = list(
            care_provision ~ "Needed childcare or dependent care to attend the hospital",
            care_duration_clean = "Duration of dependent care (hours)",
            paid_caretaker ~ "Paid for dependent care to attend visit",
            care_cost ~ "Cost of care (KSh)",
            service_cost ~ "Had to pay to get services today",
            circumstances_change ~ "Family situation changed since PMAD screening",
            insurance_plan = "Has insurance plan",
            plan_type = "Type of insurance plan",
            cost_payment  ~ "Insurance covered hospital visit",
            medication_prescribtion ~ "Prescribed medication today",
            medication1 ~ "First medication prescribed",
            medication2 ~ "Second medication prescribed",
            medication3 ~ "Third medication prescribed",
            medication4 ~ "Fourth medication prescribed",
            medication_duration ~ "Total duration of medications (days)",
            medication1_duration ~ "Duration of first medication (days)",
            medication2_duration ~ "Duration of second medication (days)",
            medication3_duration ~ "Duration of third medication (days)",
            medication4_duration ~ "Duration of fourth medication (days)",
            medication1_cost ~ "Amount paid for first medication (KSh)",
            medication2_cost ~ "Amount paid for Second medication (KSh)",
            medication3_cost ~ "Amount paid for third medication (KSh)",
            medication4_cost ~ "Amount paid for fourth medication (KSh)",
            change1 ~ "Family situation change",
            change1_cost ~ "Additional monthly cost due to change (KSh)"),
        sort = list(all_categorical() ~ "frequency") # Sort categorical levels by frequency
    ) %>% 
    bold_labels() %>% 
    add_n() %>% 
    italicize_levels() %>% 
    gtsummary::as_gt() %>% 
    gt::tab_header(
        title = "Financial and Care Burden Associated with Clinic Visits",
        subtitle = "Childcare needs, service utilization, medication, and insurance coverage"
    ) %>%
    gt::tab_options(
        table.font.size = gt::px(12),
        data_row.padding = gt::px(2)
    )


child_care_summary





