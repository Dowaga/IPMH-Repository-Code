# Header ------------------------------------------------------------------

# Author(s): Yuwei Wang
# Date: Jul 29, 2024
# Description: This script pulls data from the REDCap projects and exports them as .csv files to the Raw Study Data folder on One Drive.
# Update in Feb 19, 2025: Added all REDCap databases so far.

# Setup ------------------------------------------------------------------------
rm(list = ls())             

# Reference source codes & other dependencies: Use this section to reference other scripts and dependencies
source("DataTeam_ipmh.R")
source("Dependencies.R") # Where we store all needed R packages

# Where we refer to token
token_df <- read.csv(file.path (ipmh_filepath, "/Data/Tokens.csv"))
aim1qual_consenting_token <- token_df[1,2]
aim1qual_demo_token <- token_df[2,2]
aim1quant_consenting_token <- token_df[3,2]
aim1quant_data_token <-token_df[4,2]
aim1_facilitychecklist_token <- token_df[5,2]
rct_hcw_token <- token_df[6,2]
rct_hcw_consenting_token <- token_df[7,2]
rct_ppw_consenting_token <- token_df[8,2]
rct_ppw_token <- token_df[9,2]
daily_closeout_token <- token_df[10,2]
equip_hcw_token <- token_df[11,2]
equip_ro_token <- token_df[12,2]
phq2_gad2_abstract_token <- token_df[13,2]
pm_token <- token_df[14,2]
telepsych_token <- token_df[15,2]

# Display the first few rows of the dataframe to confirm successful loading
head(token_df)

############ AIM 1 ##############---------------------------------------------
# #Aim1 qual consenting database----------------
# # Set file paths: Use this section to set input and output filepaths
# data_aim1qual_consenting_dir <- file.path(ipmh_filepath, "/Data/2. Consenting database") # Aim1 qual consenting
# 
# # Create a REDCap database connection ###
# httr::set_config( httr::config( ssl_verifypeer = 0L )) ## ensuring security when creating connection
# 
# redcapcon<-redcapConnection(url='https://online.knh.or.ke:8446/redcap/api/',
#                             token = aim1qual_consenting_token) 
# 
# ### Import the REDCap dataset ###
# aim1qual_consenting <- exportRecordsTyped(redcapcon, fields = NULL, forms = NULL, 
#                                      records = NULL, events = NULL, survey = FALSE, factors = FALSE, 
#                                      dag = FALSE, checkboxLabels = TRUE)
# 
# ### Creating datafile ###
# write.csv(aim1qual_consenting, paste0(ipmh_filepath,"/Data/2. Consenting database/Aim1_qual_",Sys.Date(),".csv"))


# #Aim1 qual demographics database---------------------
# # Set file paths: Use this section to set input and output filepaths
# data_aim1qual_demo_dir <- file.path(ipmh_filepath, "/Data/3. Aim 1 qualitative demo")
# 
# # Create a REDCap database connection ###
# httr::set_config( httr::config( ssl_verifypeer = 0L )) ## ensuring security when creating connection
# 
# redcapcon<-redcapConnection(url='https://online.knh.or.ke:8446/redcap/api/',
#                             token = aim1qual_demo_token) 
# 
# ### Import the REDCap dataset ###
# aim1qual_demo <- exportRecordsTyped(redcapcon, fields = NULL, forms = NULL, 
#                                           records = NULL, events = NULL, survey = FALSE, factors = FALSE, 
#                                           dag = FALSE, checkboxLabels = TRUE)
# 
# ### Creating datafile ###
# write.csv(aim1qual_demo, paste0(ipmh_filepath,"/Data/3. Aim 1 qualitative demo/Aim1_qual_demo_",Sys.Date(),".csv"))
# 


# #Aim 1 quant consenting database----------------------
# # Set file paths: Use this section to set input and output filepaths
# data_aim1quant_consenting_dir <- file.path(ipmh_filepath, "/Data/2. Consenting database")
# 
# # Create a REDCap database connection ###
# httr::set_config( httr::config( ssl_verifypeer = 0L )) ## ensuring security when creating connection
# 
# redcapcon<-redcapConnection(url='https://online.knh.or.ke:8446/redcap/api/',
#                             token = aim1quant_consenting_token) 
# 
# ### Import the REDCap dataset ###
# aim1quant_consenting <- exportRecordsTyped(redcapcon, fields = NULL, forms = NULL, 
#                                     records = NULL, events = NULL, survey = FALSE, factors = FALSE, 
#                                     dag = FALSE, checkboxLabels = TRUE)
# 
# ### Creating datafile ###
# write.csv(aim1quant_consenting, paste0(ipmh_filepath,"/Data/2. Consenting database/Aim1_quant_",Sys.Date(),".csv"))



# #Aim 1 quant survey database-------------------------
# # Set file paths: Use this section to set input and output filepaths
# data_aim1quant_data_dir <- file.path(ipmh_filepath, "/Data/4. Aim 1 quantitative data")
# 
# # Create a REDCap database connection ###
# httr::set_config( httr::config( ssl_verifypeer = 0L )) ## ensuring security when creating connection
# 
# redcapcon<-redcapConnection(url='https://online.knh.or.ke:8446/redcap/api/',
#                             token = aim1quant_data_token) 
# 
# ### Import the REDCap dataset ###
# aim1quant_data <- exportRecordsTyped(redcapcon, fields = NULL, forms = NULL, 
#                                     records = NULL, events = NULL, survey = FALSE, factors = FALSE, 
#                                     dag = FALSE, checkboxLabels = TRUE)
# 
# ### Creating datafile ###
# write.csv(aim1quant_data, paste0(ipmh_filepath,"/Data/4. Aim 1 quantitative data/Aim1_quant_data_",Sys.Date(),".csv"))



# #Aim 1 facility checklist database--------------------
# # Set file paths: Use this section to set input and output filepaths
# data_facilitychecklist_dir <- file.path(ipmh_filepath, "/Data/1. facility checklist")
# 
# # Create a REDCap database connection ###
# httr::set_config( httr::config( ssl_verifypeer = 0L )) ## ensuring security when creating connection
# 
# redcapcon<-redcapConnection(url='https://online.knh.or.ke:8446/redcap/api/',
#                             token = aim1_facilitychecklist_token) 
# 
# ### Import the REDCap dataset ###
# aim1_facilitychecklist <- exportRecordsTyped(redcapcon, fields = NULL, forms = NULL, 
#                                     records = NULL, events = NULL, survey = FALSE, factors = FALSE, 
#                                     dag = FALSE, checkboxLabels = TRUE)
# 
# ### Creating datafile ###
# write.csv(aim1_facilitychecklist, paste0(ipmh_filepath,"/Data/1. facility checklist/Aim1_facilitychecklist_",Sys.Date(),".csv"))

############ RCT ##############---------------------------------------------
# #RCT HCW database----------------
# # Set file paths: Use this section to set input and output filepaths
# data_rct_hcw_dir <- file.path(ipmh_filepath, "/Data/5. RCT HCW data")
# 
# # Create a REDCap database connection ###
# httr::set_config( httr::config( ssl_verifypeer = 0L )) ## ensuring security when creating connection
# 
# redcapcon<-redcapConnection(url='https://online.knh.or.ke:8446/redcap/api/',
#                             token = rct_hcw_token)
# 
# ### Import the REDCap dataset ###
# rct_hcw <- exportRecordsTyped(redcapcon, fields = NULL, forms = NULL, 
#                                     records = NULL, events = NULL, survey = FALSE, factors = FALSE, 
#                                     dag = FALSE, checkboxLabels = TRUE)
# 
# ### Creating datafile ###
# write.csv(rct_hcw, paste0(ipmh_filepath,"/Data/5. RCT HCW data/RCT_HCW_",Sys.Date(),".csv"))
# 

# #RCT HCW consenting database---------------------
# # Set file paths: Use this section to set input and output filepaths
# data_rct_hcw_consenting_dir <- file.path(ipmh_filepath, "/Data/2. Consenting database")
# 
# # Create a REDCap database connection ###
# httr::set_config( httr::config( ssl_verifypeer = 0L )) ## ensuring security when creating connection
# 
# redcapcon<-redcapConnection(url='https://online.knh.or.ke:8446/redcap/api/',
#                             token = rct_hcw_consenting_token)
# 
# ### Import the REDCap dataset ###
# rct_hcw_consenting <- exportRecordsTyped(redcapcon, fields = NULL, forms = NULL, 
#                                     records = NULL, events = NULL, survey = FALSE, factors = FALSE, 
#                                     dag = FALSE, checkboxLabels = TRUE)
# 
# ### Creating datafile ###
# write.csv(rct_hcw_consenting, paste0(ipmh_filepath,"/Data/2. Consenting database/RCT_HCW_consenting_",
#                                      Sys.Date(),".csv"))

#RCT PPW database----------------
# Set file paths: Use this section to set input and output filepaths
data_rct_ppw_dir <- file.path(ipmh_filepath, "/Data/6. RCT PPW data")

# Create a REDCap database connection ###
httr::set_config( httr::config( ssl_verifypeer = 0L )) ## ensuring security when creating connection

redcapcon<-redcapConnection(url='https://online.knh.or.ke:8446/redcap/api/',
                            token = rct_ppw_token)

### Import the REDCap dataset ###
rct_ppw <- exportRecordsTyped(redcapcon, fields = NULL, forms = NULL, 
                                    records = NULL, events = NULL, 
                              survey = FALSE, factors = FALSE, 
                                    dag = FALSE, checkboxLabels = TRUE)

### Creating datafile ###
write.csv(rct_ppw, paste0(ipmh_filepath,"/Data/6. RCT PPW data/RCT_PPW_",Sys.Date(),".csv"))

#RCT PPW consenting database---------------------
# Set file paths: Use this section to set input and output filepaths
data_rct_ppw_consenting_dir <- file.path(ipmh_filepath, "/Data/2. Consenting database")

# Create a REDCap database connection ###
httr::set_config( httr::config( ssl_verifypeer = 0L )) ## ensuring security when creating connection

redcapcon<-redcapConnection(url='https://online.knh.or.ke:8446/redcap/api/',
                            token = rct_ppw_consenting_token)

### Import the REDCap dataset ###
rct_ppw_consenting <- exportRecordsTyped(redcapcon, fields = NULL, forms = NULL, 
                                    records = NULL, events = NULL, survey = FALSE, factors = FALSE, 
                                    dag = FALSE, checkboxLabels = TRUE)

### Creating datafile ###
write.csv(rct_ppw_consenting, paste0(ipmh_filepath,"/Data/2. Consenting database/RCT_PPW_consenting_",
                                     Sys.Date(),".csv"))


#Daily close-out database----------------
# Set file paths: Use this section to set input and output filepaths
data_daily_closeout_dir <- file.path(ipmh_filepath, "/Data/7. RCT admin data")

# Create a REDCap database connection ###
httr::set_config( httr::config( ssl_verifypeer = 0L )) ## ensuring security when creating connection

redcapcon <- redcapConnection(url='https://online.knh.or.ke:8446/redcap/api/',
                            token = daily_closeout_token)

### Import the REDCap dataset ###
daily_closeout <- exportRecordsTyped(redcapcon, fields = NULL, forms = NULL, 
                                    records = NULL, events = NULL, survey = FALSE, factors = FALSE, 
                                    dag = FALSE, checkboxLabels = TRUE)

### Creating datafile ###
write.csv(daily_closeout, paste0(ipmh_filepath,"/Data/7. RCT admin data/Daily_closeout_",Sys.Date(),".csv"))


############ EQUIP ##############---------------------------------------------
#Equip lay provider database----------------
# Set file paths: Use this section to set input and output filepaths
data_equip_hcw_dir <- file.path(ipmh_filepath, "/Data/8. EQUIP data")

# Create a REDCap database connection ###
httr::set_config( httr::config( ssl_verifypeer = 0L )) ## ensuring security when creating connection

redcapcon<-redcapConnection(url='https://online.knh.or.ke:8446/redcap/api/',
                            token = equip_hcw_token)

### Import the REDCap dataset ###
equip_hcw <- exportRecordsTyped(redcapcon, fields = NULL, forms = NULL, 
                                    records = NULL, events = NULL, 
                                survey = FALSE, factors = FALSE, 
                                    dag = FALSE, checkboxLabels = TRUE)

### Creating datafile ###
write.csv(equip_hcw, paste0(ipmh_filepath,"/Data/8. EQUIP data/Equip_HCW_",Sys.Date(),".csv"))

#Equip RO database----------------
# Set file paths: Use this section to set input and output filepaths
data_equip_ro_dir <- file.path(ipmh_filepath, "/Data/8. EQUIP data")

# Create a REDCap database connection ###
httr::set_config( httr::config( ssl_verifypeer = 0L )) ## ensuring security when creating connection

redcapcon<-redcapConnection(url='https://online.knh.or.ke:8446/redcap/api/',
                            token = equip_ro_token)

### Import the REDCap dataset ###
equip_ro <- exportRecordsTyped(redcapcon, fields = NULL, forms = NULL, 
                                    records = NULL, events = NULL, 
                                survey = FALSE, factors = FALSE, 
                                    dag = FALSE, checkboxLabels = TRUE)

### Creating datafile ###
write.csv(equip_ro, paste0(ipmh_filepath,"/Data/8. EQUIP data/Equip_RO_",Sys.Date(),".csv"))

############ RCT ADMIN ##############---------------------------------------------
#PHQ2/GAD2 abstraction database----------------
# Set file paths: Use this section to set input and output filepaths
data_phq2_gad2_abstract_dir <- file.path(ipmh_filepath, "/Data/7. RCT admin data")

# Create a REDCap database connection ###
httr::set_config( httr::config( ssl_verifypeer = 0L )) ## ensuring security when creating connection

redcapcon<-redcapConnection(url='https://online.knh.or.ke:8446/redcap/api/',
                            token = phq2_gad2_abstract_token)

### Import the REDCap dataset ###
phq2_gad2_abstract <- exportRecordsTyped(redcapcon, fields = NULL, forms = NULL, 
                                    records = NULL, events = NULL, 
                                survey = FALSE, factors = FALSE, 
                                    dag = FALSE, checkboxLabels = TRUE)

### Creating datafile ###
write.csv(phq2_gad2_abstract, paste0(ipmh_filepath,"/Data/7. RCT admin data/PHQ2_GAD2_abstract_",Sys.Date(),".csv"))

#PM+ database----------------
# Set file paths: Use this section to set input and output filepaths
data_pm_dir <- file.path(ipmh_filepath, "/Data/7. RCT admin data")

# Create a REDCap database connection ###
httr::set_config( httr::config( ssl_verifypeer = 0L )) ## ensuring security when creating connection

redcapcon<-redcapConnection(url='https://online.knh.or.ke:8446/redcap/api/',
                            token = pm_token)

### Import the REDCap dataset ###
pm <- exportRecordsTyped(redcapcon, fields = NULL, forms = NULL, 
                                    records = NULL, events = NULL, 
                                survey = FALSE, factors = FALSE, 
                                    dag = FALSE, checkboxLabels = TRUE)

### Creating datafile ###
write.csv(pm, paste0(ipmh_filepath,"/Data/7. RCT admin data/PM_",Sys.Date(),".csv"))


#Telepsych database----------------
# Set file paths: Use this section to set input and output filepaths
data_telepsych_dir <- file.path(ipmh_filepath, "/Data/7. RCT admin data")

# Create a REDCap database connection ###
httr::set_config( httr::config( ssl_verifypeer = 0L )) ## ensuring security when creating connection

redcapcon<-redcapConnection(url='https://online.knh.or.ke:8446/redcap/api/',
                            token = telepsych_token)

### Import the REDCap dataset ###
telepsych <- exportRecordsTyped(redcapcon, fields = NULL, forms = NULL, 
                                    records = NULL, events = NULL, 
                                survey = FALSE, factors = FALSE, 
                                    dag = FALSE, checkboxLabels = TRUE)

### Creating datafile ###
write.csv(telepsych, paste0(ipmh_filepath,"/Data/7. RCT admin data/Telepsych_",Sys.Date(),".csv"))

