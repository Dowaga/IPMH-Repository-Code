### Dependencies ###

# Please add here any packages required for running the code!
packages <- c("redcapAPI", 
              "dplyr",
              "magrittr",
              "zoo",
              "lubridate",
              "ggplot2",
              "openxlsx", 
              "png",
              "knitr",
              "DiagrammeR", 
              "tidyverse", 
              "eeptools", 
              "stringr", 
              "haven", 
              "reshape2", 
              "here", 
              "Hmisc", 
              "anthro", 
              "devtools", 
              "kableExtra", 
              "foreign",
              "gdata", 
              "httr",
              "table1",
              "compareGroups", 
              #              "taskscheduleR", 
              "janitor", 
              "gmodels",
              "data.table",
              "consort",
              "flowchart")

# Check for duplicates in list 
if(length(packages[duplicated(packages)])>0){
    message("These packages are duplicated in the packages list:\n",
            paste0(packages[duplicated(packages)], sep = " "),
            "\n\nPlease delete any duplicates.")
}

# Check if there are any new packages to install
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]

# Install any new packages
if(length(new.packages) > 0){
    message("installing new packages!\n", paste0(new.packages, sep = " "))
    install.packages(new.packages)
}

# Load all packages
for(p in packages){
    library(p, character.only = T)
}
