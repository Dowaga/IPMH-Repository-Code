# Header ------------------------------------------------------------------

# Author(s): Owaga
# Date: March 17, 2025
# Reducing Tension Checklist Table

# Setup ------------------------------------------------------------------------

# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")
# Sample data
reducing_tc_df <- ppw_rct_df %>% 
    select(record_id, rtc_1, rtc_2, rtc_3, rtc_4, rtc_5, rtc_6, 
           rtc_7, rtc_8, rtc_9, rtc_10)

# Reshaping to long format
data_long <- reducing_tc_df %>%
    pivot_longer(cols = -record_id, names_to = "Statement", values_to = "Response")

# Generate the summary table
summary_table <- data_long %>%
    group_by(Statement) %>%
    summarise(
        n = n(),  # Total responses per statement
        `All the time (%)` = round(sum(Response == "All the time (almost every day)") / n() * 100, 1),
        `Most of the time (%)` = round(sum(Response == "Most of the time (a few times per week)") / n() * 100, 1),
        `Sometimes (%)` = round(sum(Response == "Sometimes (about once a week)") / n() * 100, 1),
        `A little/rarely (%)` = round(sum(Response == "A little/rarely (once or twice in the past month)") / n() * 100, 1),
        `Not at all (%)` = round(sum(Response == "Not at all") / n() * 100, 1)
    )

summary_table <- summary_table %>% 
    mutate(Statement = recode(Statement,
                              rtc_1 = "Helped friends or family with difficulties",
                              rtc_2 = "Asked friends or family, to help with difficulties",
                              rtc_3 = "Listened to friends or family when talking \n about their stress",
                              rtc_4 = "Told friends or family about stress",
                              rtc_5 = "Time spend meeting with and talking to your \n relatives and friends",
                              rtc_6 = "Tried to do deep or slow breathing to \n manage your stress",
                              rtc_7 = "Done something that energizes you ",
                              rtc_8 = "Tried to distinguish between which of your \n problems can be solved and which of your \n problems cannot be solved",
                              rtc_9 = "Tried to brainstorm different ways to manage your problems",
                              rtc_10 = "Separated problems into small manageable steps"))


# Convert to gt
summary_table %>% 
    gt() %>%
    tab_header(
        title = "Reducing Tension Checklist Table") %>%
    tab_options(
        table.font.size = px(12)
    )

summary_table

