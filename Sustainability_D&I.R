# Header ------------------------------------------------------------------
# Author(s): Yuwei
# Date: Nov 27, 2026
# Sustainability analysis: provider normalization (NoMAD) vs facility
# sustainability capacity (PSAT), 6 vs 12 months, intervention facilities.
# Input: cleaned datasets saved by the cleaning script (no raw data used here).
# Submitted for D&I 2026.

rm(list = ls())
source("Dependencies.R")
source("IS variables_cleaning.R")

library(patchwork)
library(ragg)

# ---- 0. settings ------------------
# CHECK: score column names (left) and poster labels (right), in tile order
nomad_gen_lab <- c(sus2_familiar        = "Familiar",                 
                   sus2_normal_now      = "Normal part of work now",
                   sus2_normal_will     = "Will become normal")
nomad_vars    <- c(nomad_coherence      = "Coherence",
                   nomad_cog_particip   = "Cognitive participation",
                   nomad_collect_action = "Collective action",
                   nomad_reflex_monitor = "Reflexive monitoring",
                   nomad_total          = "NoMAD total")
psat_vars     <- c(psat_env_support     = "Environmental support",
                   psat_funding         = "Funding stability",
                   psat_partnerships    = "Partnerships",
                   psat_org_capacity    = "Organizational capacity",
                   psat_evaluation      = "Program evaluation",
                   psat_adaptation      = "Program adaptation",
                   psat_communications  = "Communications",
                   psat_strategic_plan  = "Strategic planning",
                   psat_total           = "PSAT total")

stopifnot(all(names(c(nomad_gen_lab, nomad_vars)) != "")) 

# theme colours (Office 2013 to 2022 theme)
col_nomad_lt <- "#A9D18E"   # green, lighter 40% (6 months)
col_nomad    <- "#70AD47"   # green, Accent 6 (12 months)
col_psat_lt  <- "#F4B183"   # orange, lighter 40% (6 months)
col_psat     <- "#ED7D31"   # orange, Accent 2 (12 months)

# analysis sample: respondents with a total score
nomad_an <- nomad_clean %>% filter(!is.na(nomad_total))
psat_an  <- psat_clean  %>% filter(!is.na(psat_total))

# ---- 1. sample characteristics (Table 1) --------------------------------
nomad_char <- nomad_an %>%
    select(pt_id, wave, final_cadre) %>%
    left_join(demog_master %>%
                  select(pt_id, final_sex, final_age, final_terms,
                         final_monthsjob, monthsjob_main_w6, monthsjob_main_w12) %>%
                  distinct(pt_id, .keep_all = TRUE),
              by = "pt_id") %>%
    mutate(months_job = case_when(
        wave == 6  ~ coalesce(monthsjob_main_w6,  final_monthsjob),
        wave == 12 ~ coalesce(monthsjob_main_w12, final_monthsjob)),
        years_job = months_job / 12)

nomad_char %>%
    select(wave, final_sex, final_age, final_cadre, final_terms, years_job) %>%
    tbl_summary(
        by = wave,
        type = list(years_job ~ "continuous"),
        statistic = list(final_age ~ "{mean} ({sd})",
                         years_job ~ "{median} [{p25}, {p75}]",
                         all_categorical() ~ "{n} ({p}%)"),
        label = list(final_sex   ~ "Sex",
                     final_age   ~ "Age",
                     final_cadre ~ "Cadre",
                     final_terms ~ "Employment terms",
                     years_job   ~ "Years in current job"),
        missing = "no"
    )

# matched providers; respondents per facility
nomad_an %>% count(pt_id) %>% summarise(n_paired = sum(n == 2))
nomad_an %>% count(wave, facility_name) %>%
    group_by(wave) %>% summarise(min = min(n), max = max(n))

# PSAT facilities per wave and matched across waves
psat_an %>% count(wave)
psat_an %>% count(facility_code) %>% summarise(n_matched = sum(n == 2))

# ---- 2. wave comparison --------------------------------------------------
compare_waves <- function(df, vars, id) {
    map_dfr(names(vars), function(v) {
        d   <- df %>% select(all_of(c(id, "wave", v))) %>% rename(y = all_of(v))
        w6  <- d %>% filter(wave == 6,  !is.na(y))
        w12 <- d %>% filter(wave == 12, !is.na(y))
        pr  <- inner_join(w6, w12, by = id, suffix = c("_6", "_12"))
        tibble(
            var       = v,
            label     = vars[[v]],
            n_6       = nrow(w6),
            mean_6    = mean(w6$y),  sd_6  = sd(w6$y),
            n_12      = nrow(w12),
            mean_12   = mean(w12$y), sd_12 = sd(w12$y),
            n_pairs   = nrow(pr),
            p_paired  = if (nrow(pr) > 1)
                wilcox.test(pr$y_12, pr$y_6, paired = TRUE, exact = FALSE)$p.value
            else NA_real_,
            p_ranksum = wilcox.test(w12$y, w6$y, exact = FALSE)$p.value
        )
    })
}

nomad_res <- compare_waves(nomad_an, c(nomad_gen_lab, nomad_vars), id = "pt_id")  # CHANGED
psat_res  <- compare_waves(psat_an,  psat_vars, id = "facility_code")             # by facility

nomad_res %>% mutate(across(where(is.double), ~ round(.x, 3))) %>% print(width = Inf)
psat_res  %>% mutate(across(where(is.double), ~ round(.x, 3))) %>% print(width = Inf)

# ---- 3. NoMAD mean change with 95% CI (matched providers) ---------------
nomad_change <- map_dfr(names(nomad_vars), function(v) {
    pr <- nomad_an %>%
        select(pt_id, wave, y = all_of(v)) %>%
        filter(!is.na(y)) %>%
        pivot_wider(names_from = wave, values_from = y, names_prefix = "w") %>%
        filter(!is.na(w6), !is.na(w12))
    tt <- t.test(pr$w12 - pr$w6)
    tibble(label = nomad_vars[[v]], n_pairs = nrow(pr),
           change = unname(tt$estimate),
           lo = tt$conf.int[1], hi = tt$conf.int[2])
})
nomad_change

# ---- 4. NoMAD by cadre (check that pooling cadres is reasonable) --------
nomad_an %>%
    group_by(wave, final_cadre) %>%
    summarise(n = n(), mean = mean(nomad_total), sd = sd(nomad_total), .groups = "drop")

# ---- 5. direction table (for a PowerPoint table) ------------------------
direction_tbl <- bind_rows(
    nomad_res %>% filter(var %in% names(nomad_vars)) %>% mutate(lens = "NoMAD"),
    psat_res  %>% filter(var %in% names(psat_vars))  %>% mutate(lens = "PSAT")
) %>%
    mutate(change    = mean_12 - mean_6,
           direction = case_when(change > 0 ~ "\u2191", change < 0 ~ "\u2193", TRUE ~ "\u2192")) %>%
    select(lens, label, mean_6, mean_12, change, direction, p_paired)

direction_tbl %>% mutate(across(where(is.double), ~ round(.x, 2))) %>% print(n = Inf)

# direction counts (constructs and domains only, totals excluded)
direction_tbl %>% filter(!str_detect(label, "total")) %>% count(lens, direction)
