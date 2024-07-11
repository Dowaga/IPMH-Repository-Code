### Data Team Info ###

da_users <- data.frame(Analyst = c("Owaga", "Lincoln Pothan" ),
                       github_user = c("Dowaga", "lpothan"),
                       git_filepath = c("C:/Program Files/Git/", "/usr/local/bin/git"),
                       ipmh_filepath = c("C:/Users/Damaris/uw/ngumbau-IPMH", "/Users/Lincolnpothan/Library/CloudStorage/OneDrive-UW/IPMH study"),
                       machinetype = c("PC", "Mac"))

### Defining OneDrive data filepath based on analyst ###
current_wd <- getwd()

if (grepl("Dowaga", current_wd)) {
  ipmh_filepath <- "C:/Users/Damaris/uw/ngumbau-IPMH"
} else if (grepl("Lincolnpothan", current_wd)) {
    rctact_filepath <- "/Users/Lincolnpothan/Library/CloudStorage/OneDrive-UW/Documents - chv-neo/RCT Activities"
} else {
 print("No filepath")
}

### Working Directory Check ###

if(endsWith(current_wd, "IPMH-Repository-Code")) {ipmh_wd <- current_wd
} else {
  print("WARNING: Incorrect Working Directory")
} 

