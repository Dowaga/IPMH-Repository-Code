# Header ------------------------------------------------------------------

# Author(s): Owaga
# Date: March 11, 2025
# Study Exit/Termination QCs
## Script is used to analyze participants who have reach six months visit 
## and have not been exited from the study for field team Querying

# Setup ------------------------------------------------------------------------
rm(list = ls())
# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")

exits_df <- ppw_rct_df%>%
    # Filter to "6 weeks" and grab the first per participant
    filter(clt_visit == "6 months post-partum")

not_exited <- exits_df %>% 
    filter(dis_today =="No")

not_exited_df <- not_exited %>% 
    select(record_id, clt_study_site, clt_staffname, clt_date, clt_visit,
           dis_today, dis_date)

write.csv(not_exited_df, "C:/Users/hp/OneDrive/Desktop/IPMH/QCs/Not Exited at Six Months.csv", row.names = FALSE)
