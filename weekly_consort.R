# Header ------------------------------------------------------------------

# Author(s): Owaga
# Date: March 10, 2025
# Consort Diagram for weekly report

# Setup ------------------------------------------------------------------------

# Reference source codes & other dependencies:
source("DataTeam_ipmh.R")
source("Dependencies.R")
source("data_import.R")

#### Define Variables ####

Attendees <- daily_closeout_df %>% 
    summarise(`ANC Attendees` = sum(rct_anc_number))

eligibility_assess <- screening_consent_df %>% 
    reframe(`Assessed for Eligibility` = n())

enrolled <- ppw_rct_df %>% 
    reframe(Enrolled = n())

n_eligible  <- screening_consent_df %>% 
    filter(rct_eligible == 0)%>% 
    summarise(`Not Eligible` = n())

### Build Consort ###
font_size = 2.5

weekly_consort <- tibble(x= 1:100, y= 1:100)

weekly_consort %>% 
  ggplot(aes(x, y)) +
  scale_x_continuous(minor_breaks = seq(10, 100, 10)) +
  scale_y_continuous(minor_breaks = seq(10, 100, 10)) +
  theme_linedraw() -> p


    ## ANC ATTENDANCE BOX & ARROW DOWN
 weekly_consort <- p +
     geom_rect(xmin = 40, xmax=60, ymin=90, ymax=100, color='black',
            fill='white', size=0.25, size=0.25) +
  annotate('text', x= 50, y=95,label= paste("ANC attendance\n(n = ",Attendees,")", sep=""), size=2.5) +
    geom_segment(
    x=50, xend=50, y=90, yend=80, 
    size=0.15, linejoin = "mitre", lineend = "butt", linetype = "dashed",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
    ###
    theme_void()
  
