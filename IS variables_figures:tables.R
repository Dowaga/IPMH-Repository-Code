# Header ------------------------------------------------------------------

# Author(s): Yuwei
# Date: Nov 25, 2026
# This is a script to generate figures and tables for data exploration.

# Setup ------------------------------------------------------------------------
rm(list = ls())

# Reference source codes & other dependencies:
source("Dependencies.R")
source("data_import.R")
source("DataTeam_ipmh.R")
source("IS variables_cleaning.R")


# Demographics: Table and figures ------------
## Uses final_* columns only (entry visit).
## Columns: Overall, Intervention, Control. Categorical = n (%);
## age = mean (SD); months and hours = median [IQR].

part_wave <- screen_key %>%
    transmute(pt_id = screen_id, wave) %>%
    left_join(demog_master %>%
                  select(pt_id, part_id, facility_code, facility_name,
                         condition, cadre_code),
              by = "pt_id")

## Table 1. Arm assignment by facility ------------
## HCWs enrolled at each visit (screening database), by facility, arm, cadre.
## Cells show "Pre-launch / 6m / 12m" counts of HCWs who answered.
cadre_lab <- c("22" = "Lay providers", "24" = "Nurses/COs", "23" = "In-charges")

tab_src <- part_wave %>%
    mutate(cadre = cadre_lab[as.character(cadre_code)])

# add arm totals and an "All cadres" column by stacking copies
tab_src <- bind_rows(tab_src,
                     tab_src %>% mutate(facility_code = 99L, facility_name = "Arm total"))
tab_src <- bind_rows(tab_src,
                     tab_src %>% mutate(cadre = "All cadres"))

fac_table <- tab_src %>%
    count(condition, facility_code, facility_name, cadre, wave) %>%
    pivot_wider(names_from = wave, values_from = n,
                names_prefix = "w", values_fill = 0) %>%
    mutate(cell = paste(w0, w6, w12, sep = " / ")) %>%
    select(-w0, -w6, -w12) %>%
    pivot_wider(names_from = cadre, values_from = cell, values_fill = "0 / 0 / 0") %>%
    left_join(tab_src %>%
                  filter(cadre == "All cadres") %>%
                  group_by(condition, facility_code) %>%
                  summarise(`HCWs (ever)` = n_distinct(pt_id), .groups = "drop"),
              by = c("condition", "facility_code")) %>%
    arrange(desc(condition), facility_code) %>%      # Intervention first
    mutate(facility = if_else(facility_code == 99, facility_name,
                              sprintf("%02d %s", facility_code, facility_name))) %>%
    select(condition, facility,
           any_of(c("Lay providers", "Nurses/COs", "In-charges", "All cadres")),
           `HCWs (ever)`)

fac_table %>% print(n = Inf)

n_int <- sum(fac_table$condition == "Intervention")

fac_table %>%
    select(-condition) %>%
    kbl(caption = "HCWs enrolled by facility, arm, cadre and visit",
        align = c("l", rep("c", 5))) %>%
    kable_styling(bootstrap_options = c("striped", "condensed"),
                  full_width = FALSE) %>%
    add_header_above(c(" " = 1, "Pre-launch / 6m / 12m" = 4, " " = 1)) %>%
    pack_rows("Intervention", 1, n_int) %>%
    pack_rows("Control", n_int + 1, nrow(fac_table)) %>%
    row_spec(which(fac_table$facility == "Arm total"), bold = TRUE)

## Table 1 by arm, with SMD 
# CHANGED: add_difference() before add_overall(); SMD instead of p-values
make_tbl1 <- function(df) {
    t1_data(df) %>%
        tbl_summary(
            by = condition,
            type = list(starts_with("Dept:") ~ "dichotomous",
                        where(is.numeric) & !starts_with("Dept:") ~ "continuous"),
            statistic = list(all_continuous()  ~ "{median} [{p25}, {p75}]",
                             `Age (years)`     ~ "{mean} ({sd})",
                             all_categorical() ~ "{n} ({p}%)"),
            missing = "ifany", missing_text = "Missing"
        ) %>%
        add_difference(test = everything() ~ "smd") %>%   # NEW
        add_overall() %>%
        bold_labels()
}

tbl1 <- make_tbl1(demog_master)
tbl1

## Figure 1: HCWs enrolled by facility, cadre and visit ------------

cadre_lab <- c("22" = "Lay providers", "24" = "Nurses/COs", "23" = "In-charges")

fig_src <- screen_key %>%
    transmute(pt_id = screen_id, wave) %>%
    left_join(demog_master %>%
                  select(pt_id, facility_code, facility_name, condition, cadre_code),
              by = "pt_id") %>%
    mutate(
        cadre     = factor(cadre_lab[as.character(cadre_code)],
                           levels = c("Lay providers", "Nurses/COs", "In-charges")),
        wave      = factor(wave, levels = c(0, 6, 12),
                           labels = c("Pre-launch", "6-month", "12-month")),
        condition = factor(condition, levels = c("Intervention", "Control")),
        facility  = sprintf("%02d %s", facility_code, facility_name),
        # facility 01 at the top of each panel
        facility  = factor(facility, levels = rev(sort(unique(facility))))
    ) %>%
    count(condition, facility, wave, cadre)

# totals for the label at the end of each bar
fig_tot <- fig_src %>%
    group_by(condition, facility, wave) %>%
    summarise(total = sum(n), .groups = "drop")

ggplot(fig_src, aes(x = n, y = facility, fill = cadre)) +
    geom_col(width = 0.7, colour = "white", linewidth = 0.4,      # thin gap between segments
             position = position_stack(reverse = TRUE)) +          # stack order = legend order
    geom_text(data = fig_tot, aes(x = total, y = facility, label = total),
              inherit.aes = FALSE, hjust = -0.3, size = 3, colour = "grey30") +
    facet_grid(condition ~ wave, scales = "free_y", space = "free_y") +
    scale_fill_manual(values = c("Lay providers" = "#2a78d6",
                                 "Nurses/COs"    = "#eb6834",
                                 "In-charges"    = "#1baf7a")) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = "HCWs enrolled by facility, cadre and visit",
         x = "Number of HCWs", y = NULL, fill = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position    = "top",
          panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank(),
          strip.text         = element_text(face = "bold"),
          plot.title         = element_text(face = "bold"))

# ggsave(file.path(ipmh_filepath, "fig_enrolled_by_facility.png"),
#        width = 10, height = 7, dpi = 300)

## Table 2. Participation flow by arm ------------
## Visit pattern per HCW, e.g. "0-6-12" = present at all three visits.
## Earlier counts: 0-6-12 = 179, 0-6 = 12, 0 = 9, 6-12 = 7, 12 = 10.

flow_src <- screen_key %>%
    arrange(screen_id, wave) %>%
    group_by(screen_id) %>%
    summarise(pattern = paste(wave, collapse = "-"), .groups = "drop") %>%
    rename(pt_id = screen_id) %>%
    left_join(demog_master %>% select(pt_id, condition), by = "pt_id")

flow_src %>% count(pattern)   # check against the earlier counts

flow_tab <- bind_rows(flow_src, flow_src %>% mutate(condition = "Overall")) %>%
    group_by(condition) %>%
    summarise(
        `Enrolled at Pre-launch`      = sum(str_starts(pattern, "0")),
        `Present at all 3 visits`     = sum(pattern == "0-6-12"),
        `Left after 6-month`          = sum(pattern == "0-6"),
        `Left after Pre-launch`       = sum(pattern == "0"),
        `Joined at 6-month`           = sum(str_starts(pattern, "6")),
        `Stayed to 12-month`          = sum(pattern == "6-12"),
        `Joined at 12-month`          = sum(pattern == "12"),
        `Present at Pre-launch`       = sum(str_detect(pattern, "^0")),
        `Present at 6-month`          = sum(str_detect(pattern, "(^|-)6(-|$)")),
        `Present at 12-month`         = sum(str_detect(pattern, "12")),
        `Total HCWs ever enrolled`    = n(),
        .groups = "drop"
    ) %>%
    pivot_longer(-condition, names_to = "Stage", values_to = "n") %>%
    pivot_wider(names_from = condition, values_from = n) %>%
    select(Stage, Intervention, Control, Overall)

flow_tab %>%
    kbl(caption = "HCW participation across visits, by arm",
        align = c("l", "c", "c", "c")) %>%
    kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE) %>%
    pack_rows("Pre-launch cohort", 1, 4) %>%
    pack_rows("Joined later", 5, 7) %>%
    pack_rows("Present at each visit", 8, 10) %>%
    add_indent(c(2, 3, 4, 6)) %>%
    row_spec(11, bold = TRUE)

## Retention of the Pre-launch cohort to 12-month, by arm (%)
flow_src %>%
    filter(str_starts(pattern, "0")) %>%
    group_by(condition) %>%
    summarise(n = n(),
              retained_12m = sum(pattern == "0-6-12"),
              pct = round(100 * retained_12m / n, 1),
              .groups = "drop")
## Figure 2: who is present at each visit, by when they joined ------
## Bars = HCWs present at each visit; colour = when the HCW joined.
## A shrinking dark segment shows attrition of the Pre-launch cohort.
fig2_src <- flow_src %>%
    mutate(joined = case_when(str_starts(pattern, "0") ~ "Pre-launch",
                              str_starts(pattern, "6") ~ "Joined at 6-month",
                              TRUE                     ~ "Joined at 12-month"),
           joined = factor(joined, levels = c("Pre-launch", "Joined at 6-month",
                                              "Joined at 12-month"))) %>%
    inner_join(screen_key %>% transmute(pt_id = screen_id, wave), by = "pt_id") %>%
    mutate(wave = factor(wave, levels = c(0, 6, 12),
                         labels = c("Pre-launch", "6-month", "12-month")),
           condition = factor(condition, levels = c("Intervention", "Control"))) %>%
    count(condition, wave, joined)

fig2_tot <- fig2_src %>%
    group_by(condition, wave) %>%
    summarise(total = sum(n), .groups = "drop")

ggplot(fig2_src, aes(x = wave, y = n, fill = joined)) +
    geom_col(width = 0.6, colour = "white", linewidth = 0.4,
             position = position_stack(reverse = TRUE)) +
    geom_text(data = fig2_tot, aes(x = wave, y = total, label = total),
              inherit.aes = FALSE, vjust = -0.5, size = 3.5, colour = "grey30") +
    facet_wrap(~ condition) +
    # ordinal blues: darker = joined earlier
    scale_fill_manual(values = c("Pre-launch"         = "#1c5cab",
                                 "Joined at 6-month"  = "#5598e7",
                                 "Joined at 12-month" = "#9ec5f4")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(title = "HCWs present at each visit, by when they joined",
         x = NULL, y = "Number of HCWs", fill = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position    = "top",
          panel.grid.major.x = element_blank(),
          panel.grid.minor   = element_blank(),
          strip.text         = element_text(face = "bold"),
          plot.title         = element_text(face = "bold"))

## Table 3. Sample composition by visit ------------
## One row per HCW per visit attended (594 rows).
## Descriptive only: the same HCWs appear at several visits, so standard
## tests (chi-square, t-test) would not be valid here.

cadre_lab <- c("22" = "Lay providers", "24" = "Nurses/COs", "23" = "In-charges")

comp_src <- screen_key %>%
    transmute(pt_id = screen_id, wave, screen_date, job_visit = rct_job) %>%
    left_join(demog_master %>%
                  select(pt_id, condition, cadre_code, final_dob,
                         final_sex, final_education),
              by = "pt_id") %>%
    mutate(age_visit = time_length(interval(final_dob, screen_date), "years"),
           cadre     = factor(cadre_lab[as.character(cadre_code)],
                              levels = c("Lay providers", "Nurses/COs", "In-charges")),
           wave      = factor(wave, levels = c(0, 6, 12),
                              labels = c("Pre-launch", "6-month", "12-month")),
           condition = factor(condition, levels = c("Intervention", "Control")))

nrow(comp_src)   # 594


## Table: arm x visit
tbl_comp <- comp_src %>%
    select(condition, wave,
           `Age at visit (years)` = age_visit,
           Sex                    = final_sex,
           Cadre                  = cadre,
           `Job at visit`         = job_visit,
           `Education (at entry)` = final_education) %>%
    tbl_strata(
        strata   = condition,
        .tbl_fun = ~ .x %>%
            tbl_summary(by = wave,
                        statistic = list(all_continuous()  ~ "{mean} ({sd})",
                                         all_categorical() ~ "{n} ({p}%)"),
                        missing = "ifany")
    )

tbl_comp %>%
    as_kable_extra(caption = "Characteristics of HCWs present at each visit, by arm") %>%
    kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)


## Figure 3: cadre mix at each visit, by arm ------
## Same cadre colours as Figure 1.
fig3_src <- comp_src %>%
    count(condition, wave, cadre) %>%
    group_by(condition, wave) %>%
    mutate(pct = 100 * n / sum(n)) %>%
    ungroup()

ggplot(fig3_src, aes(x = wave, y = pct, fill = cadre)) +
    geom_col(width = 0.6, colour = "white", linewidth = 0.4,
             position = position_stack(reverse = TRUE)) +
    geom_text(aes(label = paste0(round(pct), "%")),
              position = position_stack(vjust = 0.5, reverse = TRUE),
              size = 3, colour = "white") +
    facet_wrap(~ condition) +
    scale_fill_manual(values = c("Lay providers" = "#2a78d6",
                                 "Nurses/COs"    = "#eb6834",
                                 "In-charges"    = "#1baf7a")) +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0.02))) +
    labs(title = "Cadre mix of HCWs present at each visit",
         x = NULL, y = "% of HCWs", fill = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position    = "top",
          panel.grid.major.x = element_blank(),
          panel.grid.minor   = element_blank(),
          strip.text         = element_text(face = "bold"),
          plot.title         = element_text(face = "bold"))

## Table 4. Retention of the Pre-launch cohort ------------
## Pre-launch cohort only (n = 200); late entrants excluded.
## Groups: present at all 3 visits / left after 6-month / left after Pre-launch.
## Descriptive only (21 HCWs left; too few for meaningful tests).

ret_src <- demog_master %>%
    filter(entry_wave == 0) %>%
    left_join(flow_src %>% select(pt_id, pattern), by = "pt_id") %>%
    mutate(
        retention = factor(case_when(pattern == "0-6-12" ~ "All 3 visits",
                                     pattern == "0-6"    ~ "Left after 6-month",
                                     pattern == "0"      ~ "Left after Pre-launch"),
                           levels = c("All 3 visits", "Left after 6-month",
                                      "Left after Pre-launch")),
        cadre = factor(cadre_lab[as.character(cadre_code)],
                       levels = c("Lay providers", "Nurses/COs", "In-charges"))
    )

nrow(ret_src)               # 200
ret_src %>% count(retention) # 179 / 12 / 9


## Table: baseline characteristics by retention group 
tbl_ret <- ret_src %>%
    select(retention,
           Arm                   = condition,
           Cadre                 = cadre,
           Job                   = final_job,
           `Age (years)`         = final_age,
           Sex                   = final_sex,
           Education             = final_education,
           `Terms of engagement` = final_terms,
           `Months at facility`  = final_monthswork,
           final_dpt_anc:final_dpt_other) %>%
    rename_with(~ paste0("Dept: ", str_remove(.x, "^final_dpt_")),
                starts_with("final_dpt_")) %>%
    tbl_summary(
        by      = retention,
        percent = "row",
        type    = list(starts_with("Dept:")  ~ "dichotomous",
                       `Months at facility`  ~ "continuous"),
        statistic = list(all_continuous()  ~ "{median} [{p25}, {p75}]",
                         `Age (years)`     ~ "{mean} ({sd})",
                         all_categorical() ~ "{n} ({p}%)"),
        missing = "ifany"
    ) %>%
    add_overall()

tbl_ret %>%
    as_kable_extra(caption = "Baseline characteristics of the Pre-launch cohort, by retention") %>%
    kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)

## Figure 4: retention by facility ------
## % of each facility's Pre-launch cohort present at all 3 visits.
fig4_src <- ret_src %>%
    group_by(condition, facility_code, facility_name) %>%
    summarise(n        = n(),
              retained = sum(retention == "All 3 visits"),
              pct      = 100 * retained / n,
              .groups  = "drop") %>%
    mutate(facility  = sprintf("%02d %s", facility_code, facility_name),
           facility  = factor(facility, levels = rev(sort(unique(facility)))),
           condition = factor(condition, levels = c("Intervention", "Control")))

ggplot(fig4_src, aes(x = pct, y = facility)) +
    geom_segment(aes(x = 0, xend = pct, yend = facility),
                 colour = "grey80", linewidth = 0.8) +
    geom_point(size = 3, colour = "#2a78d6") +
    geom_text(aes(label = paste0(retained, "/", n)),
              hjust = -0.4, size = 3, colour = "grey30") +
    facet_grid(condition ~ ., scales = "free_y", space = "free_y") +
    scale_x_continuous(limits = c(0, 112), breaks = seq(0, 100, 25),
                       labels = function(x) paste0(x, "%")) +
    labs(title = "Pre-launch cohort present at all 3 visits, by facility",
         x = "% retained", y = NULL) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank(),
          strip.text         = element_text(face = "bold"),
          plot.title         = element_text(face = "bold"))

# Culture, stress and effort: tables and figures ------------
## A  Completion and data quality (table)
## B  Item responses, all HCWs (diverging bars)
## C  Item responses by arm (diverging bars)
## D  Scale scores by arm and cadre (table; PROVISIONAL scoring)
## E  % agree by facility and item (heatmap)

cadre_lab <- c("22" = "Lay providers", "24" = "Nurses/COs", "23" = "In-charges")

## Short item labels; (R) = reverse-worded
item_lab <- c(
    culture_1 = "Openly talk about what works",
    culture_2 = "Willing to change after feedback",
    culture_3 = "Hard to get things to change (R)",
    culture_4 = "Can rely on others to do their jobs",
    culture_5 = "People enjoy their work",
    culture_6 = "Problems solved face-to-face",
    culture_7 = "Regularly reflect on how we work",
    culture_8 = "Reflect after trying something new",
    culture_9 = "Operate as a real team",
    cstress_1 = "Too many pressures to do my job",
    cstress_2 = "Staff show stress and strain",
    cstress_3 = "Workload reduces quality of care",
    cstress_4 = "Staff frustration is common",
    cstress_5 = "Pulled in too many directions",
    cstress_6 = "Frequent changes in responsibilities",
    ceffort_1 = "Want to perform at their best",
    ceffort_2 = "Enthusiastic about their work",
    ceffort_3 = "Get by doing as little as possible (R)",
    ceffort_4 = "Make a special effort to do a good job",
    ceffort_5 = "No more effort than they have to (R)"
)

scale_of <- function(item) factor(case_when(str_starts(item, "culture") ~ "Culture",
                                            str_starts(item, "cstress") ~ "Stress",
                                            str_starts(item, "ceffort") ~ "Effort"),
                                  levels = c("Culture", "Stress", "Effort"))


## A. Completion and data quality ------------
cse_long_raw <- cse_raw %>%
    filter(form_status == "Complete") %>%
    pivot_longer(all_of(cse_items), names_to = "item", values_to = "label")

pnta <- cse_long_raw %>% filter(str_detect(label, "Prefer not"))

tab_a <- tibble(
    Check  = c("HCWs enrolled at 6 months", "Forms completed", "Items per form",
               "Blank items", "Prefer not to answer", "Response label formats",
               "Reverse-worded items"),
    Result = c(sum(screen_key$wave == 6),
               n_distinct(cse_long_raw$pt_id),
               "20 (culture 9, stress 6, effort 5)",
               sum(is.na(cse_long_raw$label) | cse_long_raw$label == ""),
               paste0(nrow(pnta), " responses from ", n_distinct(pnta$pt_id),
                      " HCWs (set to missing)"),
               "2 formats (\"4: Agree\" and \"Agree\"), recoded to 1-5",
               "culture_3, ceffort_3, ceffort_5 (reversed copies added as *_r)")
)

tab_a %>%
    kbl(caption = "Culture, stress and effort: completion and data quality (6-month visit)") %>%
    kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)


## Helper for B and C: response percentages and diverging positions ------
## Neutral is split half left, half right of zero.
lik_pos <- function(df, group_vars = character(0)) {
    df %>%
        select(all_of(group_vars), all_of(cse_items)) %>%
        pivot_longer(all_of(cse_items), names_to = "item", values_to = "value") %>%
        filter(!is.na(value)) %>%
        count(across(all_of(c(group_vars, "item"))), value) %>%
        group_by(across(all_of(c(group_vars, "item")))) %>%
        complete(value = 1:5, fill = list(n = 0)) %>%
        arrange(value, .by_group = TRUE) %>%
        mutate(pct   = 100 * n / sum(n),
               start = -(pct[value == 1] + pct[value == 2] + pct[value == 3] / 2),
               xmin  = start + cumsum(lag(pct, default = 0)),
               xmax  = xmin + pct,
               agree = pct[value == 4] + pct[value == 5]) %>%
        ungroup() %>%
        mutate(response   = factor(value, levels = 1:5,
                                   labels = c("Strongly disagree", "Disagree", "Neutral",
                                              "Agree", "Strongly agree")),
               scale      = scale_of(item),
               item_label = factor(item_lab[item], levels = rev(item_lab)))
}

## Diverging colours: red (disagree) - grey (neutral) - blue (agree)
lik_cols <- c("Strongly disagree" = "#e34948", "Disagree" = "#f4a9a8",
              "Neutral" = "#d9d8d4",
              "Agree" = "#86b6ef", "Strongly agree" = "#1c5cab")

# CHANGED: facet_formula can be NULL (no facets)
# CHANGED: base_size argument (default 14); bar labels scale with it
plot_lik <- function(d, facet_formula = NULL, title, base_size = 14) {
    p <- ggplot(d, aes(x = (xmin + xmax) / 2, y = item_label,
                       width = xmax - xmin, fill = response)) +
        geom_tile(height = 0.7, colour = "white", linewidth = 0.3) +
        geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.3) +
        geom_text(data = d %>% filter(value == 5),
                  mapping = aes(x = xmax + 3, y = item_label,
                                label = paste0(round(agree), "%")),
                  inherit.aes = FALSE, hjust = 0,
                  size = base_size / 3.2,                  # CHANGED: was fixed at 3
                  colour = "grey30") +
        scale_fill_manual(values = lik_cols, drop = FALSE) +
        scale_x_continuous(limits = c(-100, 118), breaks = seq(-100, 100, 50),
                           labels = function(x) paste0(abs(x), "%")) +
        labs(title = title, x = NULL, y = NULL, fill = NULL,
             caption = "Number at right = % agree or strongly agree. (R) = reverse-worded item, shown as answered.") +
        guides(fill = guide_legend(nrow = 1)) +
        theme_minimal(base_size = base_size) +           # CHANGED: was 11
        theme(legend.position    = "top",
              legend.text        = element_text(size = base_size - 1),
              axis.text.y        = element_text(size = base_size, colour = "grey15"),  # NEW: item labels
              panel.grid.major.y = element_blank(),
              panel.grid.minor   = element_blank(),
              strip.text         = element_text(face = "bold", size = base_size),
              plot.title         = element_text(face = "bold", size = base_size + 2),
              plot.caption       = element_text(colour = "grey40", size = base_size - 3))
    
    if (!is.null(facet_formula)) {
        p <- p + facet_grid(facet_formula, scales = "free_y", space = "free_y")
    }
    p
}


## B. Item responses, one figure per scale ------------
# NEW: split by scale
b_src <- lik_pos(cse_clean)

fig_b <- map(set_names(levels(b_src$scale)), function(s) {
    plot_lik(b_src %>% filter(scale == s),
             title = paste0(s, ": responses by item (6-month visit, n = 198)"))
})

fig_b$Culture
fig_b$Stress
fig_b$Effort

## C. Item responses by arm ------------
fig_c <- plot_lik(lik_pos(cse_clean %>%
                              mutate(condition = factor(condition,
                                                        levels = c("Intervention", "Control"))),
                          group_vars = "condition"),
                  scale ~ condition,
                  "Responses to culture, stress and effort items, by arm")
fig_c


## D1. Scale scores by arm ------------
## SMD: standardized mean difference (intervention vs control)
# CHANGED: split from the merged table
tab_d_arm <- d_vars %>%
    tbl_summary(by = condition,
                include = c(Culture, Stress, Effort),
                statistic = d_stat, digits = all_continuous() ~ 2,
                missing = "no") %>%
    add_overall() %>%
    add_difference(test = everything() ~ "smd")

tab_d_arm %>%
    as_kable_extra(caption = "Scale scores by arm, mean (SD), range 1-5") %>%
    kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)


## D2. Scale scores by cadre ------------
## p-value: linear mixed model with a random intercept for facility
# CHANGED: split from the merged table
tab_d_cadre <- d_vars %>%
    tbl_summary(by = cadre,
                include = c(Culture, Stress, Effort),
                statistic = d_stat, digits = all_continuous() ~ 2,
                missing = "no") %>%
    add_overall() %>%                                  # NEW: overall column, as in D1
    add_p(test = everything() ~ mixed_p)

tab_d_cadre %>%
    as_kable_extra(caption = "Scale scores by cadre, mean (SD), range 1-5") %>%
    kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)

score_mean <- function(df, items, min_prop = 0.8) {
    m     <- as.matrix(df[, items])
    n_ans <- rowSums(!is.na(m))
    ifelse(n_ans / length(items) >= min_prop, rowMeans(m, na.rm = TRUE), NA_real_)
}

culture_items <- c(paste0("culture_", c(1, 2, 4:9)), "culture_3_r")
stress_items  <- paste0("cstress_", 1:6)
effort_items  <- c(paste0("ceffort_", c(1, 2, 4)), "ceffort_3_r", "ceffort_5_r")

# CHANGED: scores computed outside mutate() (avoids cur_data(), deprecated in newer dplyr)
cse_scores <- cse_clean %>%
    mutate(
        culture_score = score_mean(cse_clean, culture_items),
        stress_score  = score_mean(cse_clean, stress_items),
        effort_score  = score_mean(cse_clean, effort_items),
        cadre     = factor(cadre_lab[as.character(cadre_code)],
                           levels = c("Lay providers", "Nurses/COs", "In-charges")),
        condition = factor(condition, levels = c("Intervention", "Control"))
    )

## E. Facility mean scores with 95% CI ------------
## One panel per scale; facilities ordered by score within each panel.
## Colour and (I)/(C) tag = arm; dashed line = overall mean.
## About 10 HCWs per facility, so CIs are wide; read differences with care.
## Requires: cse_scores (section D).
## CHANGED: replaces the heatmap (old fig_e_src / fig_e removed)

library(tidytext)   # reorder_within(): order facilities separately in each panel

## Data: one row per facility per scale 
fig_e_src <- cse_scores %>%
    select(facility_code, facility_name, condition,
           Culture = culture_score, Stress = stress_score, Effort = effort_score) %>%
    pivot_longer(c(Culture, Stress, Effort), names_to = "scale", values_to = "score") %>%
    filter(!is.na(score)) %>%
    group_by(scale, facility_code, facility_name, condition) %>%
    summarise(n    = n(),
              mean = mean(score),
              se   = sd(score) / sqrt(n),
              lo   = mean - qt(0.975, n - 1) * se,
              hi   = mean + qt(0.975, n - 1) * se,
              .groups = "drop") %>%
    mutate(scale      = factor(scale, levels = c("Culture", "Stress", "Effort")),
           arm_tag    = if_else(condition == "Intervention", "I", "C"),
           # shorter facility names so labels fit
           fac_short  = str_remove(facility_name,
                                   " Sub[- ]?[Cc]ounty Hospital| Sub-District Hospital| Health Centre.*| Level (4|IV) Hospital"),
           facility   = sprintf("%02d %s (%s)", facility_code, fac_short, arm_tag),
           facility_s = reorder_within(facility, mean, scale))

# Check: 20 facilities x 3 scales = 60 rows
nrow(fig_e_src)

## Overall mean per scale (reference line) 
overall_means <- fig_e_src %>%
    group_by(scale) %>%
    summarise(overall = weighted.mean(mean, n), .groups = "drop")

## Plot 
fig_e <- ggplot(fig_e_src, aes(x = mean, y = facility_s, colour = condition)) +
    geom_vline(data = overall_means, aes(xintercept = overall),
               linetype = "dashed", colour = "grey50") +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = 0.6) +
    geom_point(size = 2.5) +
    facet_wrap(~ scale, scales = "free_y", nrow = 1) +
    scale_y_reordered() +
    scale_colour_manual(values = c("Intervention" = "#2a78d6", "Control" = "#eb6834")) +
    scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
    labs(title = "Facility mean scores with 95% CI",
         x = "Mean score (1-5)", y = NULL, colour = NULL,
         caption = "(I) = intervention, (C) = control. Dashed line = overall mean. About 10 HCWs per facility.\nHigher = better culture, more stress, more effort.") +
    theme_minimal(base_size = 13) +
    theme(legend.position    = "top",
          panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank(),
          strip.text         = element_text(face = "bold"),
          plot.title         = element_text(face = "bold"),
          plot.caption       = element_text(colour = "grey40"))

fig_e

# AIM / IAM / FIM: tables and figures ------------
## Intervention arm only.
## IPMH package: all visits. Screening, PM+, telepsychiatry: 12 months only.
## Score = mean of 4 items per scale (range 1-5), requires >= 3 of 4 answered.
library(lmerTest)
library(tidytext)

cadre_lab <- c("22" = "Lay providers", "24" = "Nurses/COs", "23" = "In-charges")

item_txt <- c(aim_1 = "Meets my approval",   aim_2 = "Appealing to me",
              aim_3 = "I like it",           aim_4 = "I welcome it",
              iam_1 = "Seems fitting",       iam_2 = "Seems suitable",
              iam_3 = "Seems applicable",    iam_4 = "Seems like a good match",
              fim_1 = "Seems implementable", fim_2 = "Seems possible",
              fim_3 = "Seems doable",        fim_4 = "Seems easy to use")

## Long data: one row per HCW x visit x item 
aif_long <- aif_clean %>%
    pivot_longer(all_of(aif_items), names_to = "item", values_to = "value") %>%
    mutate(
        scale  = factor(toupper(str_extract(item, "^(aim|iam|fim)")),
                        levels = c("AIM", "IAM", "FIM")),
        target = factor(case_when(str_detect(item, "_sc_") ~ "Screening",
                                  str_detect(item, "_pm_") ~ "PM+",
                                  str_detect(item, "_tp_") ~ "Telepsychiatry",
                                  TRUE                    ~ "IPMH package"),
                        levels = c("IPMH package", "Screening", "PM+", "Telepsychiatry")),
        item_key   = paste0(tolower(scale), "_", str_extract(item, "\\d$")),
        item_label = factor(unname(item_txt[item_key]), levels = rev(unname(item_txt))),
        visit = factor(wave, levels = c(0, 6, 12),
                       labels = c("Pre-launch", "6-month", "12-month")),
        cadre = factor(cadre_lab[as.character(cadre_code)],
                       levels = c("Lay providers", "Nurses/COs", "In-charges"))
    ) %>%
    # components were only asked at 12 months
    filter(target == "IPMH package" | wave == 12)

## Scores: one row per HCW x visit x target x scale 
aif_scores <- aif_long %>%
    group_by(pt_id, wave, visit, facility_code, facility_name, cadre, target, scale) %>%
    summarise(n_ans = sum(!is.na(value)),
              score = if (n_ans >= 3) mean(value, na.rm = TRUE) else NA_real_,
              .groups = "drop")

# Wide by scale (AIM, IAM, FIM as columns) for the tables
aif_wide <- aif_scores %>%
    select(-n_ans) %>%
    pivot_wider(names_from = scale, values_from = score)

## Custom tests for add_p() 
# Repeated measures (visits, or components rated by the same HCW):
# random intercepts for facility and HCW
mixed_p_rep <- function(data, variable, by, ...) {
    f <- as.formula(paste0("`", variable, "` ~ ", by,
                           " + (1 | facility_code) + (1 | pt_id)"))
    m <- lmerTest::lmer(f, data = data)
    tibble::tibble(p.value = anova(m)$`Pr(>F)`[1],
                   method  = "Linear mixed model (random intercepts for facility and HCW)")
}
# One row per HCW: random intercept for facility only
mixed_p_fac <- function(data, variable, by, ...) {
    f <- as.formula(paste0("`", variable, "` ~ ", by, " + (1 | facility_code)"))
    m <- lmerTest::lmer(f, data = data)
    tibble::tibble(p.value = anova(m)$`Pr(>F)`[1],
                   method  = "Linear mixed model (random intercept for facility)")
}

stat_ms <- list(all_continuous() ~ "{mean} ({sd})")

## A. Completion and data quality ------------
n_by_visit <- aif_clean %>% count(wave) %>% pull(n)

tab_a <- tibble(
    Check  = c("Arm", "IPMH package forms (Pre-launch / 6m / 12m)",
               "Screening, PM+, telepsychiatry forms (12m)", "Items per form",
               "Partially completed targets", "Prefer not to answer",
               "Enrolled at 12m with no HCW survey record"),
    Result = c("Intervention only (form not given in control facilities)",
               paste(n_by_visit, collapse = " / "),
               n_by_visit[3],
               "12 per target (AIM 4, IAM 4, FIM 4); 48 at 12 months",
               "0",
               "3 responses from 3 HCWs (set to missing)",
               "2 (1 nurse/CO, 1 in-charge)")
)

tab_a %>%
    kbl(caption = "Acceptability, appropriateness and feasibility: completion and data quality") %>%
    kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)


## B. Item responses ------------
resp_cols <- c("Neutral or disagree" = "#d9d8d4",
               "Agree"               = "#86b6ef",
               "Completely agree"    = "#1c5cab")

b_src <- aif_long %>%
    filter(!is.na(value)) %>%
    mutate(resp = factor(case_when(value == 5 ~ "Completely agree",
                                   value == 4 ~ "Agree",
                                   TRUE       ~ "Neutral or disagree"),
                         levels = names(resp_cols))) %>%
    count(target, visit, scale, item_label, resp) %>%
    group_by(target, visit, scale, item_label) %>%
    mutate(pct = 100 * n / sum(n)) %>%
    ungroup()

plot_resp <- function(d, facet_formula, title, base_size = 14) {
    ggplot(d, aes(x = pct, y = item_label, fill = resp)) +
        geom_col(width = 0.7, colour = "white", linewidth = 0.3,
                 position = position_stack(reverse = TRUE)) +
        # % completely agree at the right end of each bar
        geom_text(data = d %>% filter(resp == "Completely agree"),
                  mapping = aes(x = 101, y = item_label, label = paste0(round(pct), "%")),
                  inherit.aes = FALSE, hjust = 0, size = base_size / 3.2, colour = "grey30") +
        facet_grid(facet_formula, scales = "free_y", space = "free_y") +
        scale_fill_manual(values = resp_cols, drop = FALSE) +
        scale_x_continuous(limits = c(0, 115), breaks = seq(0, 100, 25),
                           labels = function(x) paste0(x, "%")) +
        labs(title = title, x = NULL, y = NULL, fill = NULL,
             caption = "Number at right = % completely agree.") +
        theme_minimal(base_size = base_size) +
        theme(legend.position    = "top",
              panel.grid.major.y = element_blank(),
              panel.grid.minor   = element_blank(),
              strip.text         = element_text(face = "bold"),
              strip.text.y       = element_text(angle = 0, face = "bold"),
              plot.title         = element_text(face = "bold"),
              plot.caption       = element_text(colour = "grey40"))
}

# B1. IPMH package, by visit
fig_b1 <- plot_resp(b_src %>% filter(target == "IPMH package"), scale ~ visit,
                    "IPMH as a package: responses by item and visit")
fig_b1

# B2. Components at 12 months
fig_b2 <- plot_resp(b_src %>% filter(target != "IPMH package"), scale ~ target,
                    "IPMH components: responses by item (12-month visit)")
fig_b2


## Ceiling: % of HCWs with the maximum score (all items "Completely agree") ------
aif_scores %>%
    filter(!is.na(score)) %>%
    group_by(target, visit, scale) %>%
    summarise(pct_max = round(100 * mean(score == 5), 1), .groups = "drop") %>%
    pivot_wider(names_from = scale, values_from = pct_max) %>%
    rename(Target = target, Visit = visit) %>%
    kbl(caption = "% of HCWs scoring 5 (all items 'Completely agree')") %>%
    kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)


## C. IPMH package scores by visit ------------
tab_c <- aif_wide %>%
    filter(target == "IPMH package") %>%
    tbl_summary(by = visit, include = c(AIM, IAM, FIM),
                type = c(AIM, IAM, FIM) ~ "continuous",   # NEW: force continuous
                statistic = stat_ms, digits = all_continuous() ~ 2,
                missing = "no") %>%
    add_p(test = everything() ~ mixed_p_rep)

tab_c %>%
    as_kable_extra(caption = "IPMH package scores by visit, mean (SD), range 1-5") %>%
    kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)

# Figure: mean with 95% CI by visit
mean_ci <- function(df, ...) {
    df %>%
        filter(!is.na(score)) %>%
        group_by(...) %>%
        summarise(n = n(), mean = mean(score), se = sd(score) / sqrt(n),
                  lo = mean - qt(0.975, n - 1) * se,
                  hi = mean + qt(0.975, n - 1) * se, .groups = "drop")
}

fig_c <- aif_scores %>%
    filter(target == "IPMH package") %>%
    mean_ci(scale, visit) %>%
    ggplot(aes(x = visit, y = mean, group = 1)) +
    geom_line(colour = "#2a78d6", linewidth = 0.8) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.1, colour = "#2a78d6") +
    geom_point(size = 3, colour = "#2a78d6") +
    geom_text(aes(label = sprintf("%.2f", mean)), vjust = -1.2, size = 4.5) +
    facet_wrap(~ scale) +
    scale_y_continuous(limits = c(3, 5)) +
    labs(title = "IPMH package scores by visit (mean, 95% CI)",
         x = NULL, y = "Mean score",
         caption = "Y-axis starts at 3. AIM = acceptability, IAM = appropriateness, FIM = feasibility.") +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(),
          strip.text       = element_text(face = "bold"),
          plot.title       = element_text(face = "bold"),
          plot.caption     = element_text(colour = "grey40"))
fig_c


## D. Scores by component at 12 months ------------
tab_d <- aif_wide %>%
    filter(wave == 12) %>%
    tbl_summary(by = target, include = c(AIM, IAM, FIM),
                type = c(AIM, IAM, FIM) ~ "continuous",   # NEW: force continuous
                statistic = stat_ms, digits = all_continuous() ~ 2,
                missing = "no") %>%
    add_p(test = everything() ~ mixed_p_rep)

tab_d %>%
    as_kable_extra(caption = "Scores by IPMH component at 12 months, mean (SD), range 1-5") %>%
    kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)

fig_d <- aif_scores %>%
    filter(wave == 12) %>%
    mean_ci(scale, target) %>%
    mutate(target = factor(target, levels = rev(levels(target)))) %>%
    ggplot(aes(x = mean, y = target)) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, colour = "#2a78d6", linewidth = 0.8) +
    geom_point(size = 3, colour = "#2a78d6") +
    geom_text(aes(label = sprintf("%.2f", mean)), vjust = -1, size = 4.5) +
    facet_wrap(~ scale) +
    scale_x_continuous(limits = c(3, 5)) +
    labs(title = "Scores by IPMH component at 12 months (mean, 95% CI)",
         x = "Mean score", y = NULL,
         caption = "X-axis starts at 3.") +
    theme_minimal(base_size = 14) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank(),
          strip.text         = element_text(face = "bold"),
          plot.title         = element_text(face = "bold"),
          plot.caption       = element_text(colour = "grey40"))
fig_d


## E. IPMH package scores by cadre, at each visit ------------
tab_e <- aif_wide %>%
    filter(target == "IPMH package") %>%
    tbl_strata(
        strata   = visit,
        .tbl_fun = ~ .x %>%
            tbl_summary(by = cadre, include = c(AIM, IAM, FIM),
                        type = c(AIM, IAM, FIM) ~ "continuous",
                        statistic = stat_ms, digits = all_continuous() ~ 2,
                        missing = "no") %>%
            add_p(test = everything() ~ mixed_p_fac) %>%
            # NEW: drop N from column headers (N differs by visit; shown in the group row)
            modify_header(all_stat_cols() ~ "**{level}**"),
        .combine_with = "tbl_stack",                       # NEW: stack visits vertically
        .header       = "**{strata}** (N = {n})"           # NEW: visit and N as group row
    )

tab_e %>%
    as_kable_extra(caption = "IPMH package scores by cadre and visit, mean (SD)") %>%
    kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)

cadre_cols <- c("Lay providers" = "#2a78d6", "Nurses/COs" = "#eb6834",
                "In-charges" = "#1baf7a")
pd <- position_dodge(width = 0.25)

fig_e <- aif_scores %>%
    filter(target == "IPMH package") %>%
    mean_ci(scale, cadre, visit) %>%
    ggplot(aes(x = visit, y = mean, colour = cadre, group = cadre)) +
    geom_line(linewidth = 0.8, position = pd) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.15, position = pd) +
    geom_point(size = 3, position = pd) +
    facet_wrap(~ scale) +
    scale_colour_manual(values = cadre_cols) +
    # coord_cartesian (not limits) so CIs above 5 are cut, not dropped
    coord_cartesian(ylim = c(3, 5.1)) +
    labs(title = "IPMH package scores by cadre and visit (mean, 95% CI)",
         x = NULL, y = "Mean score", colour = NULL,
         caption = "Y-axis starts at 3. In-charges: 8 to 9 HCWs per visit.") +
    theme_minimal(base_size = 14) +
    theme(legend.position  = "top",
          panel.grid.minor = element_blank(),
          strip.text       = element_text(face = "bold"),
          plot.title       = element_text(face = "bold"),
          plot.caption     = element_text(colour = "grey40"))
fig_e

## F. Facility mean package scores by visit ------------
visit_cols <- c("Pre-launch" = "#9ec5f4", "6-month" = "#5598e7", "12-month" = "#1c5cab")

fig_f <- aif_scores %>%
    filter(target == "IPMH package") %>%
    mean_ci(scale, facility_code, facility_name, visit) %>%
    mutate(fac_short = str_remove(facility_name,
                                  " Sub[- ]?[Cc]ounty Hospital| Sub-District Hospital| Health Centre.*| Level (4|IV) Hospital"),
           facility  = factor(sprintf("%02d %s", facility_code, fac_short)),
           facility  = factor(facility, levels = rev(levels(facility)))) %>%
    ggplot(aes(x = mean, y = facility, colour = visit)) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = 0.6,
                   position = position_dodge(width = 0.6)) +
    geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
    facet_wrap(~ scale, nrow = 1) +
    scale_colour_manual(values = visit_cols) +
    scale_x_continuous(limits = c(3, 5.2)) +
    labs(title = "IPMH package: facility mean scores by visit (95% CI)",
         x = "Mean score", y = NULL, colour = NULL,
         caption = "Intervention facilities only. About 10 HCWs per facility. X-axis starts at 3.") +
    guides(colour = guide_legend(reverse = FALSE)) +
    theme_minimal(base_size = 14) +
    theme(legend.position    = "top",
          panel.grid.major.y = element_blank(),
          panel.grid.minor   = element_blank(),
          strip.text         = element_text(face = "bold"),
          plot.title         = element_text(face = "bold"),
          plot.caption       = element_text(colour = "grey40"))
fig_f

library(ggrepel)

fac_traj <- aif_scores %>%
    filter(target == "IPMH package", !is.na(score)) %>%
    group_by(scale, facility_code, facility_name, visit) %>%
    summarise(mean = mean(score), .groups = "drop") %>%
    mutate(fac_short = str_remove(facility_name,
                                  " Sub[- ]?[Cc]ounty Hospital| Sub-District Hospital| Health Centre.*| Level (4|IV) Hospital"))

overall_traj <- aif_scores %>%
    filter(target == "IPMH package", !is.na(score)) %>%
    group_by(scale, visit) %>%
    summarise(mean = mean(score), .groups = "drop")

fig_f2 <- ggplot(fac_traj, aes(x = visit, y = mean, group = facility_code)) +
    geom_line(colour = "grey70", linewidth = 0.6) +
    geom_point(colour = "grey60", size = 1.8) +
    geom_line(data = overall_traj, aes(group = 1), colour = "#2a78d6", linewidth = 1.4) +
    geom_point(data = overall_traj, aes(group = 1), colour = "#2a78d6", size = 3) +
    geom_text_repel(data = fac_traj %>% filter(visit == "12-month"),
                    aes(label = fac_short), size = 3.5, colour = "grey30",
                    hjust = 0, nudge_x = 0.2, direction = "y",
                    segment.colour = NA,            # CHANGED: no connector lines
                    xlim = c(3.15, NA)) +           # NEW: keep labels right of 12-month
    facet_wrap(~ scale) +
    scale_x_discrete(expand = expansion(add = c(0.3, 1.1))) +
    coord_cartesian(ylim = c(3, 5), clip = "off") +  # CHANGED: clip off so labels aren't cut
    labs(title = "IPMH package: facility mean scores over time",
         x = NULL, y = "Mean score",
         caption = "Grey = individual facilities; blue = all HCWs. Y-axis starts at 3. About 10 HCWs per facility.") +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(),
          strip.text       = element_text(face = "bold"),
          plot.title       = element_text(face = "bold"),
          plot.caption     = element_text(colour = "grey40"))
fig_f2
