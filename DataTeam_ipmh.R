### Data Team Info ###

da_users <- data.frame(Analyst = c("David Owaga", "Lincoln Pothan", "Yuwei Wang", "David OWaga", "Olivia Schultes"),
                       github_user = c("Dowaga", "lpothan", "yuwei-eve", "Dowaga", "oschultes"),
                       git_filepath = c("C:/Program Files/Git/cmd/git.exe", 
                                        "/usr/local/bin/git", 
                                        "usr/local/bin/git", 
                                        "C:/Program Files/Git/cmd/git.exe",
                                        "C:/Program Files/Git/cmd/git.exe"),
                       ipmh_filepath = c("C:/Users/DAMARIS/UW/ngumbau - IPMH study", 
                                         "/Users/Lincolnpothan/Library/CloudStorage/OneDrive-UW/IPMH study",
                                         "/Users/yuweiwang_1997/Library/CloudStorage/OneDrive-UW/IPMH study",
                                         "C:/Users/hp/UW/ngumbau - IPMH study",
                                         "C:/Users/Olivia Schultes/OneDrive - UW/ngumbau's files - IPMH study"),
                       machinetype = c("PC", "Mac", "Mac", "PC", "PC"))

### Defining OneDrive data filepath based on analyst ###
current_wd <- getwd()

if (grepl("DAMARIS", current_wd)) {
  ipmh_filepath <- "C:/Users/DAMARIS/UW/ngumbau - IPMH study"
} else if (grepl("Lincolnpothan", current_wd)) {
    ipmh_filepath <- "/Users/Lincolnpothan/Library/CloudStorage/OneDrive-UW/IPMH study"
} else if (grepl("yuweiwang_1997", current_wd)) { 
    ipmh_filepath <- "/Users/yuweiwang_1997/Library/CloudStorage/OneDrive-UW/IPMH study" 
}else if (grepl("hp", current_wd)) { 
    ipmh_filepath <- "C:/Users/hp/UW/ngumbau - IPMH study"
} else if (grepl("Olivia Schultes", current_wd)) {
    ipmh_filepath <- "C:/Users/Olivia Schultes/OneDrive - UW/ngumbau's files - IPMH study"
} else {
 print("No filepath")
}

### Working Directory Check ###

if(endsWith(current_wd, "IPMH Repository Code")) {ipmh_wd <- current_wd
} else {
  print("WARNING: Incorrect Working Directory")
} 

