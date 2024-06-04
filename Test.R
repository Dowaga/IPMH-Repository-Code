rm(list = ls())

pacman::p_load(knitr, tidyverse, ggplot2, psych, janitor, dplyr, 
               openxlsx, gtsummary, kableExtra)

# create column with the symptoms named, separated by semicolons
linelist_sym_1 <- linelist_cleaned %>% 
    # convert the "yes" and "no" values into the symptom name itself
    # if old value is "yes", new value is "fever", otherwise set to missing (NA)
    mutate(fever = ifelse(fever == "yes", "fever", NA), 
           chills = ifelse(chills == "yes", "chills", NA),
           cough = ifelse(cough == "yes", "cough", NA),
           aches = ifelse(aches == "yes", "aches", NA),
           vomit = ifelse(vomit == "yes", "vomit", NA)) 

linelist_sym_1 <- linelist_sym_1 %>% 
    unite(col = "all_symptoms",
          c(fever, chills, cough, aches, vomit), 
          sep = "; ",
          remove = TRUE,
          na.rm = TRUE) %>% 
    mutate(
        # make a copy of all_symptoms column, but of class "list" (which is required to use ggupset() in next step)
        all_symptoms_list = as.list(strsplit(all_symptoms, "; "))
    )

# Create the plot
ggplot(
    data = linelist_sym_1,
    mapping = aes(x = all_symptoms_list)) +
    geom_bar() +
    scale_x_upset(
        reverse = FALSE,
        n_intersections = 10,
        sets = c("fever", "chills", "cough", "aches", "vomit"))+
    labs(
        title = "Signs & symptoms",
        subtitle = "10 most frequent combinations of signs and symptoms",
        caption = "Caption here.",
        x = "Symptom combination",
        y = "Frequency in dataset")

linelist_sym_2 <- linelist_cleaned %>% 
    # convert the "yes" and "no" values into 1s and 0s
    mutate(fever = ifelse(fever == "yes", 1, 0), 
           chills = ifelse(chills == "yes", 1, 0),
           cough = ifelse(cough == "yes", 1, 0),
           aches = ifelse(aches == "yes", 1, 0),
           vomit = ifelse(vomit == "yes", 1, 0))

# Efficiently convert "yes" to 1 and 0
linelist_sym_2 <- linelist_cleaned %>% 
    # convert the "yes" and "no" values into 1s and 0s
    mutate(across(c(fever, chills, cough, aches, vomit), .fns = ~+(.x == "yes")))

# Make the plot
linelist_sym_2 %>% 
    UpSetR::upset(
        sets = c("fever", "chills", "cough", "aches", "vomit"),
        order.by = "freq",
        sets.bar.color = c("blue", "red", "yellow", "darkgreen", "orange"), # optional colors
        empty.intersections = "on",
        nsets = 3,
        number.angles = 0,
        point.size = 3.5,
        line.size = 2, 
        mainbar.y.label = "Symptoms Combinations",
        sets.x.label = "Patients with Symptom")