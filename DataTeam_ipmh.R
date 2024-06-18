### Data Team Info ###

da_users <- data.frame(Analyst = c("Owaga"),

                       github_user = c("Dowaga"),
                       git_filepath = c("C:/Program Files/Git/"),
                       ipmh_filepath = c("C:/Users/Damaris/uw/ngumbau-IPMH"),
                       machinetype = c("PC"))

### Defining OneDrive data filepath based on analyst ###
current_wd <- getwd() 

if (grepl("Dowaga", current_wd)) {
  rctact_filepath <- "C:/Users/DAMARIS/Desktop/IPMH Repository Code"
} else {
 print("No filepath")
}

### Working Directory Check ###

if(endsWith(current_wd, "IPMH Repository Code")) {
  chvneo_wd <- current_wd
} else {
  print("WARNING: Incorrect Working Directory")
} 



