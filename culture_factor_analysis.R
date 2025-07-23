# Header ------------------------------------------------------------------

# Author(s): Yuwei
# Date: July 14, 2025
# This is a script that analyzes the potential factor structure of the culture instrument.

# read packages ---------------
library(psych)
library(tidyverse)
library(gtsummary)
library(ggplot2)  
library(corrplot) 
library(car)
library(ggcorrplot)
library(additivityTests)
library(devtools)
library(gtheory)
library(EFA.dimensions)
library(GPArotation)

# Load data ---------------
source("REDCap_datapull.R")
rm(list = setdiff(ls(), c("rct_hcw")))

is_survey_df <- rct_hcw

is_survey_df <- is_survey_df %>%
    mutate(
        visit_type = case_when(
            grepl("pre", redcap_event_name, ignore.case = TRUE) ~ "Baseline",
            grepl("6-month", redcap_event_name) ~ "6 month",
            grepl("12-month", redcap_event_name) ~ "12 month",
            TRUE ~ "Other"
        )
    ) %>% 
    mutate(visit_type = factor(visit_type, levels = c("Baseline", "6 month", "12 month")))
is_survey_df <- is_survey_df %>%
    mutate(
        facility_num = str_extract(facility_id, "^[0-9]+"),  # Extracts only the leading number
        arm = case_when(
            facility_num %in% c("01", "03", "04", "07", "09", "13", "16", "17", "19", "22") ~ "Intervention",
            TRUE ~ "Control"
        ))

# demographic section ---------------------

# culture data cleaning --------------
culture_df <- is_survey_df %>% 
    filter(grepl("6-month", redcap_event_name)) %>%
    select(pt_id, facility_id, pt_type, arm, starts_with("culture_"),
           starts_with("cstress_"), starts_with("ceffort_"),
           inner_setting_culture_stress_complete, -cstress_5, -cstress_6
    ) 

culture_df <- culture_df %>%
    filter(!is.na(pt_type)) %>%  # Remove rows where pt_type is NA
    mutate(pt_type = droplevels(factor(pt_type))) %>% 
    filter(!is.na(facility_id)) %>% 
    mutate(facility_id = droplevels(factor(facility_id))) 

#scoring instruction
#reverse coding: culture_3, ceffort_5

culture_df <- culture_df %>% 
    mutate(across(starts_with("culture_"), ~ case_when(
        . == "1: Strongly disagree" ~ 1,
        . == "2: Disagree" ~ 2,
        . == "3: Neutral" ~ 3,
        . == "4: Agree" ~ 4,
        . == "5: Strongly agree" ~ 5,
        . == "Prefer not to answer" ~ NA_real_,  # Convert to NA
        TRUE ~ NA_real_  # Ensure numeric conversion
    ))) %>% 
    mutate(across(starts_with("cstress_"), ~case_when(
        . == "Strongly disagree" ~ 1,
        . == "Disagree" ~ 2,
        . == "Neutral" ~ 3,
        . == "Agree" ~ 4,
        . == "Strongly agree" ~ 5,
        . == "Prefer not to answer" ~ NA_real_,  # Convert to NA
        TRUE ~ NA_real_  # Ensure numeric conversion
    ))) %>% 
    mutate(across(starts_with("ceffort_"), ~case_when(
        . == "Strongly disagree" ~ 1,
        . == "Disagree" ~ 2,
        . == "Neutral" ~ 3,
        . == "Agree" ~ 4,
        . == "Strongly agree" ~ 5,
        . == "Prefer not to answer" ~ NA_real_,  # Convert to NA
        TRUE ~ NA_real_))) # Ensure numeric conversion

# reverse coding for some items: culture_3, ceffort_5, ceffort_3
culture_df <- culture_df %>%
    mutate(across(c(culture_3, ceffort_5, ceffort_3),
                  ~ 6- .)) 

# create domain scores
culture_df <- culture_df %>%
    mutate(
        culture = rowMeans(select(., starts_with("culture")), 
                           na.rm = TRUE),
        culture_effort = rowMeans(select(., starts_with("ceffort")), 
                                  na.rm = TRUE),
        culture_stress = rowMeans(select(., starts_with("cstress")), 
                                  na.rm = TRUE))

# writing a function to check for missingness
check_missingness <- function(df) {
    missing_summary <- sapply(df, function(x) sum(is.na(x)))
    missing_summary <- data.frame(variable = names(missing_summary), 
                                  missing_count = missing_summary)
    return(missing_summary)
}

check_missingness(culture_df)

# CTT analysis -------------------
culture_items <- c("culture_1", "culture_2", "culture_3", "culture_4", "culture_5", 
                   "culture_6", "culture_7", "culture_8", "culture_9")
cstress_items <- c("cstress_1", "cstress_2", "cstress_3", "cstress_4")
ceffort_items <- c("ceffort_1", "ceffort_2", "ceffort_3", "ceffort_4", "ceffort_5")

all_items <- c(culture_items, cstress_items, ceffort_items)

item_means <- sapply(all_items, function(x) mean(culture_df[[x]], na.rm = TRUE))
item_sds <- sapply(all_items, function(x) sd(culture_df[[x]], na.rm = TRUE))
item_descriptives <- data.frame(
    Item = all_items,
    Mean = round(item_means, 2),
    SD = round(item_sds, 2)
)

## culture ---------------
corr.test(culture_df[, culture_items], use = "pairwise.complete.obs")
corr_matrix_culture <- cor(culture_df[, culture_items], use = "pairwise.complete.obs")
ggcorrplot(corr_matrix_culture, lab=T, title = "culture")

##calculate item-total statistics
alpha_culture <- psych::alpha(culture_df[, culture_items])
item_stats_culture <- alpha_culture$item
item_stats_culture$Item <- rownames(item_stats_culture)

m <- 5
sd_max_ordinal <- sqrt((m^2 - 1) / 12)  

##compute rescaled discrimination
item_stats_culture$discrim_rescaled <- with(item_stats_culture, r.drop / sd* sd_max_ordinal )
item_stats_culture

psych::alpha(culture_df[, culture_items])
splitHalf(culture_df[, culture_items])

#spearman-brown if we expand the scale by 2 times
k <- 2
rho_xx <- as.numeric(psych::alpha(culture_df[, culture_items])$total[1]) 
SBp <- (k * rho_xx) / (1 + (k-1) * rho_xx)
SBp #0.81

## cstress ---------------
corr.test(culture_df[, cstress_items], use = "pairwise.complete.obs")
corr_matrix_cstress <- cor(culture_df[, cstress_items], use = "pairwise.complete.obs")
ggcorrplot(corr_matrix_cstress, lab=T, title = "cstress")

##calculate item-total statistics
alpha_cstress <- psych::alpha(culture_df[, cstress_items])
item_stats_cstress <- alpha_cstress$item
item_stats_cstress$Item <- rownames(item_stats_cstress)
m <- 5
sd_max_ordinal <- sqrt((m^2 - 1) / 12)

##compute rescaled discrimination
item_stats_cstress$discrim_rescaled <- with(item_stats_cstress, r.drop / sd * sd_max_ordinal)
item_stats_cstress

psych::alpha(culture_df[, cstress_items])
splitHalf(culture_df[, cstress_items])

#spearman-brown if we expand the scale by 2 times
k <- 2
rho_xx <- as.numeric(psych::alpha(culture_df[, cstress_items])$total[1])
SBp <- (k * rho_xx) / (1 + (k-1) * rho_xx)
SBp #0.87

## ceffort ---------------
corr.test(culture_df[, ceffort_items], use = "pairwise.complete.obs")
corr_matrix_ceffort <- cor(culture_df[, ceffort_items], use = "pairwise.complete.obs")
ggcorrplot(corr_matrix_ceffort, lab=T, title = "ceffort")

##calculate item-total statistics
alpha_ceffort <- psych::alpha(culture_df[, ceffort_items])
item_stats_ceffort <- alpha_ceffort$item
item_stats_ceffort$Item <- rownames(item_stats_ceffort)

m <- 5
sd_max_ordinal <- sqrt((m^2 - 1) / 12)
##compute rescaled discrimination
item_stats_ceffort$discrim_rescaled <- with(item_stats_ceffort, r.drop / sd * sd_max_ordinal)
item_stats_ceffort

psych::alpha(culture_df[, ceffort_items])
splitHalf(culture_df[, ceffort_items])

#spearman-brown if we expand the scale by 3 times
k <- 3
rho_xx <- as.numeric(psych::alpha(culture_df[, ceffort_items])$total[1])
SBp <- (k * rho_xx) / (1 + (k-1) * rho_xx)
SBp #0.80

# g-theory ==================
## culture ---------
matrix_culture <- as.matrix(culture_df[, culture_items]) 
matrix_culture_complete <- matrix_culture[complete.cases(matrix_culture), ]
tukey.test(matrix_culture_complete, alpha = 0.05, critical.value = NA)
# cannot be rejected

#one-facet random model
##data in long format
Subject <- rep(1:nrow(culture_df), times = ncol(culture_df[, culture_items]))

long_data <- data.frame(
    Subject = Subject,
    Rater = rep(colnames(culture_df[, culture_items]), 
                each = nrow(culture_df[, culture_items])),
    Score = as.vector(as.matrix(culture_df[, culture_items]))
)

colnames(long_data) <- c("Person", "Item", "Score")
gstudy_result <- gstudy(long_data, formula = Score ~ (1|Person) + (1|Item), id = "Person")
gstudy_result
dstudy_result <- dstudy(gstudy_result, colname.objects = "Person", colname.scores = "Score", data = long_data, n.facets = list(Item = 10))
dstudy_result$generalizability
dstudy_result$dependability

## cstress ---------
matrix_cstress <- as.matrix(culture_df[, cstress_items])
matrix_cstress_complete <- matrix_cstress[complete.cases(matrix_cstress), ]
tukey.test(matrix_cstress_complete, alpha = 0.05, critical.value = NA)
# was rejected

#one-facet random model
##data in long format
Subject <- rep(1:nrow(culture_df), times = ncol(culture_df[, cstress_items]))
long_data_cstress <- data.frame(
    Subject = Subject,
    Rater = rep(colnames(culture_df[, cstress_items]), 
                each = nrow(culture_df[, cstress_items])),
    Score = as.vector(as.matrix(culture_df[, cstress_items]))
)
colnames(long_data_cstress) <- c("Person", "Item", "Score")
gstudy_result_cstress <- gstudy(long_data_cstress, formula = Score ~ (1|Person) + (1|Item), id = "Person")
gstudy_result_cstress
dstudy_result_cstress <- dstudy(gstudy_result_cstress, colname.objects = "Person", colname.scores = "Score", data = long_data_cstress, n.facets = list(Item = 4))
dstudy_result_cstress$generalizability
dstudy_result_cstress$dependability

## ceffort ---------
matrix_ceffort <- as.matrix(culture_df[, ceffort_items])
matrix_ceffort_complete <- matrix_ceffort[complete.cases(matrix_ceffort), ]
tukey.test(matrix_ceffort_complete, alpha = 0.05, critical.value = NA)
## cannot be rejected

#one-facet random model
##data in long format
Subject <- rep(1:nrow(culture_df), times = ncol(culture_df[, ceffort_items]))
long_data_ceffort <- data.frame(
    Subject = Subject,
    Rater = rep(colnames(culture_df[, ceffort_items]), 
                each = nrow(culture_df[, ceffort_items])),
    Score = as.vector(as.matrix(culture_df[, ceffort_items]))
)
colnames(long_data_ceffort) <- c("Person", "Item", "Score")
gstudy_result_ceffort <- gstudy(long_data_ceffort, formula = Score ~ (1|Person) + (1|Item), id = "Person")
gstudy_result_ceffort
dstudy_result_ceffort <- dstudy(gstudy_result_ceffort, colname.objects = "Person", colname.scores = "Score", data = long_data_ceffort, n.facets = list(Item = 5))
dstudy_result_ceffort$generalizability
dstudy_result_ceffort$dependability

# factor analysis -----------------------
## culture ----------------
eigen(cor(culture_df[, culture_items], use = "pairwise.complete.obs"))$values
sum(eigen(cor(culture_df[, culture_items], use = "pairwise.complete.obs"))$values > 1) # 3
scree(culture_df[, culture_items])
RAWPAR(culture_df[, culture_items], factormodel='PCA', Ndatasets=100, percentile=95, corkind='pearson', verbose=TRUE)

df_culture <- culture_df[, culture_items]
efa1 <- fa(df_culture, nfactors = 3, rotate = "oblimin", fm="pa")
print.psych(efa1, digits=3, cut=0.3)
efa1_varimax <- fa(df_culture, nfactors=3, rotate="varimax", fm="pa") 
print.psych(efa1_varimax, digits=3, cut=0.3)
fa.diagram(efa1)
fa.diagram(efa1_varimax)
factor.congruence(efa1, efa1_varimax)
omega(df_culture, nfactors=3)$omega.tot

## cstress ----------------
eigen(cor(culture_df[, cstress_items], use = "pairwise.complete.obs"))$values
sum(eigen(cor(culture_df[, cstress_items], use = "pairwise.complete.obs"))$values > 1) # 2
scree(culture_df[, cstress_items])
RAWPAR(culture_df[, cstress_items], factormodel='PCA', Ndatasets=100, percentile=95, corkind='pearson', verbose=TRUE)

df_cstress <- culture_df[, cstress_items]
efa2 <- fa(df_cstress, nfactors = 1, rotate = "oblimin", fm="pa")
print.psych(efa2, digits=3, cut=0.3)
efa2_varimax <- fa(df_cstress, nfactors=1, rotate="varimax", fm="pa")
print.psych(efa2_varimax, digits=3, cut=0.3)
fa.diagram(efa2)
fa.diagram(efa2_varimax)
factor.congruence(efa2, efa2_varimax)
omega(df_cstress, nfactors=1)$omega.tot

## ceffort ----------------
eigen(cor(culture_df[, ceffort_items], use = "pairwise.complete.obs"))$values
sum(eigen(cor(culture_df[, ceffort_items], use = "pairwise.complete.obs"))$values > 1) # 2
scree(culture_df[, ceffort_items])
RAWPAR(culture_df[, ceffort_items], factormodel='PCA', Ndatasets=100, percentile=95, corkind='pearson', verbose=TRUE)

df_ceffort <- culture_df[, ceffort_items]
efa3 <- fa(df_ceffort, nfactors = 2, rotate = "oblimin", fm="pa")
print.psych(efa3, digits=3, cut=0.3)
efa3_varimax <- fa(df_ceffort, nfactors=2, rotate="varimax", fm="pa")
print.psych(efa3_varimax, digits=3, cut=0.3)
fa.diagram(efa3)
fa.diagram(efa3_varimax)
factor.congruence(efa3, efa3_varimax)
omega(df_ceffort, nfactors=2)$omega.tot

## multiple constructs ----------------
eigen(cor(culture_df[, all_items], use = "pairwise.complete.obs"))$values
sum(eigen(cor(culture_df[, all_items], use = "pairwise.complete.obs"))$values > 1) # 5
scree(culture_df[, all_items])
RAWPAR(culture_df[, all_items], factormodel='PCA', Ndatasets=100, percentile=95, corkind='pearson', verbose=TRUE)

df_all <- culture_df[, all_items]
efa_all <- fa(df_all, nfactors = 5, rotate = "oblimin", fm="pa")
print.psych(efa_all, digits=3, cut=0.3)
efa_all_varimax <- fa(df_all, nfactors=5, rotate="varimax", fm="pa")
print.psych(efa_all_varimax, digits=3, cut=0.3)
fa.diagram(efa_all)
fa.diagram(efa_all_varimax)
factor.congruence(efa_all, efa_all_varimax)
omega(df_all, nfactors=5)$omega.tot
