### Data Team Info ###

da_users <- data.frame(Analyst = c("Owaga", "Lincoln Pothan", "Yuwei Wang"),
                       github_user = c("Dowaga", "lpothan", "yuwei-eve"),
                       git_filepath = c("C:/Program Files/Git/", "/usr/local/bin/git", "usr/local/bin/git"),
                       ipmh_filepath = c("C:/Users/Damaris/UW/ngumbau - IPMH study", 
                                         "/Users/Lincolnpothan/Library/CloudStorage/OneDrive-UW/IPMH study",
                                         "/Users/yuweiwang_1997/Library/CloudStorage/OneDrive-UW/IPMH study"),
                       machinetype = c("PC", "Mac", "Mac"))

### Defining OneDrive data filepath based on analyst ###
current_wd <- getwd()

if (grepl("Dowaga", current_wd)) {
  ipmh_filepath <- "C:/Users/Damaris/UW/ngumbau - IPMH study"
} else if (grepl("Lincolnpothan", current_wd)) {
    ipmh_filepath <- "/Users/Lincolnpothan/Library/CloudStorage/OneDrive-UW/IPMH study"
} else if (grepl("yuweiwang_1997", current_wd)) { 
    ipmh_filepath <- "/Users/yuweiwang_1997/Library/CloudStorage/OneDrive-UW/IPMH study" 
} else {
 print("No filepath")
}

### Working Directory Check ###

if(endsWith(current_wd, "IPMH Repository Code")) {ipmh_wd <- current_wd
} else {
  print("WARNING: Incorrect Working Directory")
} 

