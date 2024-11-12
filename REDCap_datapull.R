# Header ------------------------------------------------------------------

# Author(s): Yuwei Wang
# Date: Jul 29, 2024
# Description: This script pulls data from the REDCap projects and exports them as .csv files to the Raw Study Data folder on One Drive.

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


# Display the first few rows of the dataframe to confirm successful loading
head(token_df)

#Aim1 qual consenting database----------------
# Set file paths: Use this section to set input and output filepaths
data_aim1qual_consenting_dir <- file.path(ipmh_filepath, "/Data/2. Consenting database") # Aim1 qual consenting

# Create a REDCap database connection ###
httr::set_config( httr::config( ssl_verifypeer = 0L )) ## ensuring security when creating connection

redcapcon<-redcapConnection(url='https://online.knh.or.ke:8446/redcap/api/',
                            token = aim1qual_consenting_token) 

### Import the REDCap dataset ###
aim1qual_consenting <- exportRecordsTyped(redcapcon, fields = NULL, forms = NULL, 
                                     records = NULL, events = NULL, survey = FALSE, factors = FALSE, 
                                     dag = FALSE, checkboxLabels = TRUE)

### Creating datafile ###
write.csv(aim1qual_consenting, paste0(ipmh_filepath,"/Data/2. Consenting database/Aim1_qual_",Sys.Date(),".csv"))



#Aim1 qual demographics database---------------------
# Set file paths: Use this section to set input and output filepaths
data_aim1qual_demo_dir <- file.path(ipmh_filepath, "/Data/3. Aim 1 qualitative demo")

# Create a REDCap database connection ###
httr::set_config( httr::config( ssl_verifypeer = 0L )) ## ensuring security when creating connection

redcapcon<-redcapConnection(url='https://online.knh.or.ke:8446/redcap/api/',
                            token = aim1qual_demo_token) 

### Import the REDCap dataset ###
aim1qual_demo <- exportRecordsTyped(redcapcon, fields = NULL, forms = NULL, 
                                          records = NULL, events = NULL, survey = FALSE, factors = FALSE, 
                                          dag = FALSE, checkboxLabels = TRUE)

### Creating datafile ###
write.csv(aim1qual_demo, paste0(ipmh_filepath,"/Data/3. Aim 1 qualitative demo/Aim1_qual_demo_",Sys.Date(),".csv"))



#Aim 1 quant consenting database----------------------
# Set file paths: Use this section to set input and output filepaths
data_aim1quant_consenting_dir <- file.path(ipmh_filepath, "/Data/2. Consenting database")

# Create a REDCap database connection ###
httr::set_config( httr::config( ssl_verifypeer = 0L )) ## ensuring security when creating connection

redcapcon<-redcapConnection(url='https://online.knh.or.ke:8446/redcap/api/',
                            token = aim1quant_consenting_token) 

### Import the REDCap dataset ###
aim1quant_consenting <- exportRecordsTyped(redcapcon, fields = NULL, forms = NULL, 
                                    records = NULL, events = NULL, survey = FALSE, factors = FALSE, 
                                    dag = FALSE, checkboxLabels = TRUE)

### Creating datafile ###
write.csv(aim1quant_consenting, paste0(ipmh_filepath,"/Data/2. Consenting database/Aim1_quant_",Sys.Date(),".csv"))



#Aim 1 quant survey database-------------------------
# Set file paths: Use this section to set input and output filepaths
data_aim1quant_data_dir <- file.path(ipmh_filepath, "/Data/4. Aim 1 quantitative data")

# Create a REDCap database connection ###
httr::set_config( httr::config( ssl_verifypeer = 0L )) ## ensuring security when creating connection

redcapcon<-redcapConnection(url='https://online.knh.or.ke:8446/redcap/api/',
                            token = aim1quant_data_token) 

### Import the REDCap dataset ###
aim1quant_data <- exportRecordsTyped(redcapcon, fields = NULL, forms = NULL, 
                                    records = NULL, events = NULL, survey = FALSE, factors = FALSE, 
                                    dag = FALSE, checkboxLabels = TRUE)

### Creating datafile ###
write.csv(aim1quant_data, paste0(ipmh_filepath,"/Data/4. Aim 1 quantitative data/Aim1_quant_data_",Sys.Date(),".csv"))



#Aim 1 facility checklist database--------------------
# Set file paths: Use this section to set input and output filepaths
data_facilitychecklist_dir <- file.path(ipmh_filepath, "/Data/1. facility checklist")

# Create a REDCap database connection ###
httr::set_config( httr::config( ssl_verifypeer = 0L )) ## ensuring security when creating connection

redcapcon<-redcapConnection(url='https://online.knh.or.ke:8446/redcap/api/',
                            token = aim1_facilitychecklist_token) 

### Import the REDCap dataset ###
aim1_facilitychecklist <- exportRecordsTyped(redcapcon, fields = NULL, forms = NULL, 
                                    records = NULL, events = NULL, survey = FALSE, factors = FALSE, 
                                    dag = FALSE, checkboxLabels = TRUE)

### Creating datafile ###
write.csv(aim1_facilitychecklist, paste0(ipmh_filepath,"/Data/1. facility checklist/Aim1_facilitychecklist_",Sys.Date(),".csv"))



