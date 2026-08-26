# Header ------------------------------------------------------------------

# Author(s): Dowaga
# Date: August 18, 2026
# This is a script to prepare a cleaned consent dataset 
# that's ready for analysis, manuscript writing, and sharing 

# Setup ------------------------------------------------------------------------
rm(list = ls())
# Reference source codes & other dependencies:
source("Dependencies.R")
source("data_import.R")
source("DataTeam_ipmh.R")

# Set up data freeze time for this report
data_freeze <- as.Date("2026-08-18") 

###############################################################
# 1. only keep the databases we need
###############################################################
rm(list = setdiff(ls(), c("screening_consent_df", "ipmh_filepath",
                          "data_freeze")))


##############################################################
# 2. Filter only those eligible and willing to be enrolled
##############################################################

enrolling_raw_df <- screening_consent_df %>% 
    filter(rct_eligible == "1"& rct_enrolling == "Yes")

# Unique IDs
anyDuplicated(enrolling_raw_df$record_id)

####################################################################
# 3. Select only the relevant columns
####################################################################

consent_df <- enrolling_raw_df %>%
    select(
        participant_id = partipant_id_v2,
        
        # -------------------------
        # Version 6.0
        # -------------------------
        rct_participate_v2,
        rct_re_contacted_v2,
        rct_future_contact_v2,
        rct_name_first_v2,
        rct_name_middle_v2,
        rct_last_name_v2,
        consent_date_v2,
        rct_witness_first_name_v2,
        rct_witness_middle_name_v2,
        rct_witness_last_name_v2,
        staff_firstname_v2,
        staff_lastname_v2,
        date_consented_v2,
        consent_date_auto_v2,
        ipmh_rct_enrollment_consent_v60_complete,
        
        # -------------------------
        # Version 5.0
        # -------------------------
        rct_participate,
        rct_re_contacted,
        rct_future_contact,
        rct_name_first,
        rct_name_middle,
        rct_last_name,
        consent_date,
        rct_witness_first_name,
        rct_witness_middle_name,
        rct_witness_last_name,
        staff_firstname,
        staff_lastname,
        date_consented,
        consent_date_auto,
        ipmh_rct_enrollment_consent_v50_complete
    )

####################################################################
# 3. Consent version completed
####################################################################

consent_df <- consent_df %>%
    mutate(
        
        v50_complete = !is.na(ipmh_rct_enrollment_consent_v50_complete) &
            ipmh_rct_enrollment_consent_v50_complete == "Complete",
        
        v60_complete = !is.na(ipmh_rct_enrollment_consent_v60_complete) &
            ipmh_rct_enrollment_consent_v60_complete == "Complete")
####################################################################
# 3. Consent version Status
####################################################################
consent_df <-consent_df %>% 
    mutate(
        consent_status = case_when(
            v50_complete & v60_complete ~ "V5.0 + V6.0 Re-consented",
            v50_complete & !v60_complete ~ "V5.0 Only",
            !v50_complete & v60_complete ~ "V6.0 Only",
            TRUE ~ "No Completed Consent"
        )
    )
# Check participants who should have been re-consented
reconsent_df <- consent_df %>% 
    filter(consent_status == "V5.0 Only")


#########################################################################
# 4. Create a unified agreeing to participate column:
########################################################################
consent_df <- consent_df %>%
    mutate(
        participate = coalesce(
            rct_participate_v2,
            rct_participate
        ))

# check the participation status----
participation_status <- consent_df %>% 
    tabyl(participate)

        
#########################################################################
# 5. Create a unified agreeing to be re-contacted for this study column:
########################################################################
consent_df <- consent_df %>% 
    mutate(
        re_contacted = coalesce(
            rct_re_contacted_v2,
            rct_re_contacted
        ))

# check the re-contact status----
recontact_status <- consent_df %>% 
    tabyl(re_contacted)

#########################################################################
# 6. Create a unified agreeing to be re-contacted for future studies column:
########################################################################
consent_df <- consent_df %>% 
    mutate(
        future_contact = coalesce(
            rct_future_contact_v2,
            rct_future_contact
        ))

# check the future studies re-contact status----
fu_contact_status <- consent_df %>% 
    tabyl(future_contact)

#########################################################################
# 7. Create a unified first name column:
########################################################################
consent_df <- consent_df %>% 
    mutate(
    name_first = coalesce(
            rct_name_first_v2,
            rct_name_first
        ))
# No first name check----
no_first_name <- consent_df %>% 
    filter(is.na(name_first))

#########################################################################
# 8. Create a unified middle name column:
########################################################################
consent_df <- consent_df %>% 
    mutate(
        name_middle = coalesce(
            rct_name_middle_v2,
            rct_name_middle
        ))

# No middle name check----
no_middle_name <- consent_df %>% 
    filter(is.na(name_middle))

#########################################################################
# 9. Create a unified last name column:
########################################################################
consent_df <- consent_df %>% 
    mutate(
        name_last = coalesce(
            rct_last_name_v2,
            rct_last_name
        ))

# No last name check----
no_last_name <- consent_df %>% 
    filter(is.na(name_last))

        
#########################################################################
# 10. Create a unified witness first name column:
########################################################################
consent_df <- consent_df %>% 
    mutate(
        witness_first_name = coalesce(
            rct_witness_first_name_v2,
            rct_witness_first_name
        ))

#########################################################################
# 11. Create a unified witness middle name column:
########################################################################
consent_df <- consent_df %>% 
    mutate(      
        witness_middle_name = coalesce(
            rct_witness_middle_name_v2,
            rct_witness_middle_name
        ))
        
#########################################################################
# 12. Create a unified witness last name column:
########################################################################
consent_df <- consent_df %>% 
    mutate(
        witness_last_name = coalesce(
            rct_witness_last_name_v2,
            rct_witness_last_name
        ))

#########################################################################
# 13. Create a unified staff first name column:
########################################################################
consent_df <- consent_df %>% 
    mutate(   
        staff_firstname = coalesce(
            staff_firstname_v2,
            staff_firstname
        ))

#########################################################################
# 14. Create a unified staff last name column:
########################################################################
consent_df <- consent_df %>% 
    mutate( 
        staff_lastname = 
    coalesce(
            staff_lastname_v2,
            staff_lastname
        ))

###########################################################################
# 15. Consent Version Dates
###########################################################################
consent_df <- consent_df %>%
    mutate(
        # Convert dates
        v50_consent_date = coalesce(
            ymd(date_consented),
            ymd(consent_date),
            ymd(consent_date_auto)
        ),
        
        v60_consent_date = coalesce(
            ymd(date_consented_v2),
            ymd(consent_date_v2),
            ymd(consent_date_auto_v2)
        ))

#########################################################################
# 16. Create a unified consent date:
########################################################################       
consent_df <- consent_df %>% 
    mutate(
        original_consent_date = case_when(
            
            # Originally consented using V5.0
            v50_complete ~ v50_consent_date,
            
            # First consent was V6.0
            !v50_complete & v60_complete ~ v60_consent_date,
            
            TRUE ~ as.Date(NA)
        ))

#########################################################################
# 16. Create a unified re-consent date:
######################################################################## 
consent_df <- consent_df %>% 
    mutate(
        reconsent_date = case_when(
            
            # V5.0 participant subsequently re-consented
            v50_complete & v60_complete ~ v60_consent_date,
            
            TRUE ~ as.Date(NA)
        )
    )

# Check Re-consent date before original consent----
reconsent_date_errors <- consent_df %>%
    filter(
        !is.na(original_consent_date) &
            !is.na(reconsent_date) &
            reconsent_date < original_consent_date
    )

# Check Same consent date ----
same_date_errors <- consent_df %>%
    filter(
        !is.na(original_consent_date) &
            !is.na(reconsent_date) &
            reconsent_date == original_consent_date
    )



###########################################################################
# 17. Clean consent Dataset
########################################################################

consent_clean_df <- consent_df %>% 
    select(participant_id, participate, re_contacted,
           future_contact, consent_status, )

