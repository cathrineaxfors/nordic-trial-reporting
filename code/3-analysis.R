
# Systematic evaluation of clinical trial reporting at medical universities and university 
# hospitals in the Nordic countries 
# Cathrine Axfors
# OSF protocol: https://osf.io/uckyt


# README Script 3 ----

# Description: This script reproduces the tables, figures and results from the main paper.
# Output:
# 1.  Search flowchart (results in console only)
# 2.  Demographics table
# 3.  Timely publication 2 years, per institution (figure and appendix table)
# 4.  Summary results < 1 year, per institution (appendix table)
# 5.  Any results reported, per institution (figure and appendix table)
# 6.  Time to reported results, overall sample (figure: Kaplan-Meier plot)
#     Median reporting time, per institution (appendix table)
# 7.  Secondary outcomes
# 8.  Sensitivity analyses
# 9.  Subgroup analyses
# 10. Other calculations for the paper (results in console only)


# Load packages ----

library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(ggsurvfit)
library(ggplot2)
library(RColorBrewer)
#libraries purrr, tibble, scales, readxl, writexl, readr are also used


# Hard code preparations ----

today <- "" #Enter today's date YYYY-MM-DD (our date: 2023-11-28)


## Folder path ----
#Enter here the path to the folder where you saved the "data" and "code" folders (see our GitHub)
folder_path <- ""


## Load data ----

#Analysis set
load(paste0(folder_path, "data/2-data-processing/output-data/", "analysis-2023-11-28.rda"))

#Trial sample
load(paste0(folder_path, "data/2-data-processing/", "final-trial-sample-2023-11-09-corrected-2024-10-01.rda"))

#Publication data
load(paste0(folder_path, "data/2-data-processing/", "publication-information-merged-2023-11-27.rda"))




# 1. Search flowchart (in console only) ----

summary(as.factor(final_trial_sample$registry))
final_sample_euctr_reg <- filter(final_trial_sample, registry == "euctr")
summary(as.factor(final_sample_euctr_reg$eligibility))

final_sample_ctgov_reg <- filter(final_trial_sample, registry == "ctgov")
summary(as.factor(final_sample_ctgov_reg$eligibility))

final_sample_crossreg_duplicates <- filter(final_trial_sample, eligibility == "Cross-registration duplicate")
summary(as.factor(final_sample_crossreg_duplicates$crossregistration_check))

summary(as.factor(final_trial_sample$eligibility))



# 2. Demographics table ----

## Recode variables for Table 1 ----

analysis <- mutate(analysis, registered_euctr = ifelse(!is.na(eudract_id), "Yes", "No"))
analysis <- mutate(analysis, registered_ctgov = ifelse(!is.na(nct_id), "Yes", "No"))

analysis$intervention_type <- recode(analysis$intervention_type, "Medicinal product (EUCTR)" = "Medicinal product")

analysis <- mutate(analysis, masking = ifelse(masking == "Yes", "Masking", "Open label"))

analysis$sponsor_type <- analysis$main_sponsor

#Combine different phase descriptions for EUCTR and CTgov
analysis <- mutate(analysis, phase_recoded = case_when(
  phase == "Early Phase 1" | phase == "1" | phase == "Phase 1" ~ "I",
  phase == "Phase 1/Phase 2" ~ "I-II",
  phase == "2" | phase == "Phase 2" ~ "II",
  phase == "Phase 2/Phase 3" ~ "II-III",
  phase == "3" | phase == "Phase 3" ~ "III",
  phase == "4" | phase == "Phase 4" ~ "IV",
  phase == "0" | phase == "Not Applicable" ~ "Not given"))

analysis <- mutate(analysis, sample_size = case_when(
  enrollment > 0 & enrollment <= 100 ~ "1-100",
  enrollment > 100 & enrollment <= 500 ~ "100-500",
  enrollment > 500 ~ ">500"
))

#Time to registration: this variable will be analyzed under secondary outcomes (not here)
analysis <- mutate_at(analysis, vars(start_date, registration_date, completion_date), ~as_date(.))
analysis <- mutate(analysis, time_to_registration = case_when(
  registration_date < start_date ~ "Prospective registration",
  registration_date < start_date + days(61) ~ "Within 60 days after trial start",
  registration_date > start_date + days(60) ~ ">60 days after trial start"
))

analysis <- mutate(analysis, recruitment_status_recoded = case_when(
  grepl("^Completed$|Any Completed, none Prematurely Ended", recruitment_status) ~ "Completed",
  grepl("^Terminated$|Any Prematurely Ended", recruitment_status) ~ "Terminated",
  grepl("Unknown status", recruitment_status) ~ recruitment_status
))

analysis <- mutate(analysis, is_rct = ifelse(is_randomised=="Yes"&is_controlled=="Yes", "Yes", "No"))


### Create and save table ----
demo_table_save <- furniture::table1(analysis, lead_country, registered_euctr, registered_ctgov,intervention_type, 
                  is_rct, masking, sponsor_type, phase_recoded, is_multicentric, sample_size,
                  completion_year, recruitment_status_recoded,
                  na.rm = F)
#To export this file to csv, add export = "file name" and it's saved in a new folder called "table1" in the
#working directory (you'll need to define the working directory first).
#You can also just copy the table content from the console into a word file.

#save(demo_table_save, file = paste0(folder_path, "data/3-analysis/output-results/tables/", "demographics-table-", 
#                             today, ".rData"))

### Create and save table (per registry) ----
demo_table_reg_save <- furniture::table1(analysis, lead_country, registered_euctr, registered_ctgov,intervention_type, 
                                     is_rct, masking, sponsor_type, phase_recoded, is_multicentric, sample_size,
                                     completion_year, recruitment_status_recoded,
                                     na.rm = F, splitby = "registered_euctr")
#To export this file to csv, add export = "file name" and it's saved in a new folder called "table1" in the
#working directory (you'll need to define the working directory first).
#You can also just copy the table content from the console into a word file.

#save(demo_table_reg_save, file = paste0(folder_path, "data/3-analysis/output-results/tables/", "demographics-table-registry-", 
#                             today, ".rData"))


# 3. Timely publication 2 years ----

#recode "Huddinge Hospital" as "Karolinska University Hospital" (n=1)
analysis <- mutate(analysis, lead_institution = 
                     ifelse(grepl("huddinge", lead_institution, ignore.case = T),
                            str_replace(lead_institution, "Huddinge Hospital", "Karolinska University Hospital"),
                            lead_institution))

#add days_to_publ and has_publ_or_summary
analysis_dataset_institutions <- analysis %>%
  mutate(days_to_publ = pmin(days_cd_to_publication,   #get minimum of days to pub or to summary result
                             days_cd_to_summary, na.rm = TRUE)) %>%
  mutate(has_publ_or_summary = (has_publication)=="Yes" | (has_summary_results=="Yes"))

#add category that counts all trials once

#list of institution names
institutions <- analysis_dataset_institutions$lead_institution %>% 
  str_split(";") %>% 
  unlist() %>% 
  unique() %>% 
  sort()

#for given institution calculate number of trials, published trials within 24 month after CD,
#and publication percentage

get_institution_statistics <- function(institution, institution_assignments, days_to_publ, years=2)
{
  institution_tot <- sum(
    (institution_assignments == institution |
    str_detect(institution_assignments, paste0(";",institution, "$")) |
    str_detect(institution_assignments, paste0("^", institution, ";"))), 
    na.rm = TRUE)
  institution_publ_years_after_CD <- sum(
    (institution_assignments == institution |
       str_detect(institution_assignments, paste0(";",institution, "$")) |
       str_detect(institution_assignments, paste0("^", institution, ";"))) &
                                    days_to_publ < years*365, na.rm = TRUE)
  institution_perc <- round(institution_publ_years_after_CD/institution_tot, 3) * 100
  institution_wilson <- DescTools::BinomCI(institution_publ_years_after_CD, institution_tot, 
                                conf.level = 0.95, method = c("wilson", "modified wilson"))
  institution_wilson_ll <- round(institution_wilson[3], 3) * 100
  institution_wilson_ul <- round(institution_wilson[5], 3) * 100
  institution_mod_wilson_ll <- round(institution_wilson[4], 3) * 100
  institution_mod_wilson_ul <- round(institution_wilson[6], 3) * 100
  

    
  institution_stat <- tibble("institution" = institution, "trials" = institution_tot,
                      !!(paste0("publ_within_", years * 12, "m_after_CD")) := institution_publ_years_after_CD,
                      "percentage" = institution_perc,
                      "conf_int_ll" = institution_wilson_ll,
                      "conf_int_ul" = institution_wilson_ul,
                      "conf_int_mod_ll" = institution_mod_wilson_ll,
                      "conf_int_mod_ul" = institution_mod_wilson_ul)
  
  institution_stat <- mutate(institution_stat, 
    conf_int_ll = ifelse((institution_publ_years_after_CD<3 | (institution_tot - institution_publ_years_after_CD) < 3),
                         conf_int_mod_ll, conf_int_ll),
    conf_int_ul = ifelse((institution_publ_years_after_CD<3 | (institution_tot - institution_publ_years_after_CD) < 3),
                         conf_int_mod_ul, conf_int_ul)
   )
  institution_stat <- select(institution_stat, !matches("mod"))
  
  return(institution_stat)
}

## Main table for 2 years ----
institution_statistics_lead_2y <- purrr::map(institutions, get_institution_statistics,
                               institution_assignments = analysis_dataset_institutions$lead_institution,
                               days_to_publ = analysis_dataset_institutions$days_to_publ)
institution_statistics_lead_2y <- do.call(rbind, institution_statistics_lead_2y)

print(institution_statistics_lead_2y, n = Inf)
institution_statistics_lead_2y$percentage <- institution_statistics_lead_2y$percentage %>% round(2)

### Add total ----
add1 <- nrow(analysis_dataset_institutions)
add2 <- sum(analysis_dataset_institutions$days_to_publ < 2*365, na.rm = T)
add3 <- round(add2/add1, 3) * 100
add_wilson <- DescTools::BinomCI(add2, add1, conf.level = 0.95, method = c("wilson", "modified wilson"))
add_wilson_ll <- round(add_wilson[3], 3) * 100
add_wilson_ul <- round(add_wilson[5], 3) * 100
 
institution_statistics_lead_2y[nrow(institution_statistics_lead_2y) + 1,] <- list("Total", add1, add2, add3, add_wilson_ll, add_wilson_ul)
rm(add1, add2, add3, add_wilson_ll, add_wilson_ul)

## Save table ----
#writexl::write_xlsx(institution_statistics_lead_2y, paste0(folder_path, "data/3-analysis/output-results/tables/", "institution-stats-2years-", today, ".xlsx"))
#readr::write_csv(institution_statistics_lead_2y, paste0(folder_path, "data/3-analysis/output-results/tables/", "institution-stats-2years-", today, ".csv"))


## Figure ----

institution_statistics_lead_2y_fig <- left_join(institution_statistics_lead_2y, select(
  filter(analysis, !duplicated(lead_institution)), 
                                              lead_institution, lead_country), by = 
                                                c("institution" = "lead_institution"))

institution_statistics_lead_2y_fig <- filter(institution_statistics_lead_2y_fig, 
                                             !institution == "Total")

scaled_sizes <- (institution_statistics_lead_2y_fig$trials + 100)*0.005

fig <- ggplot(institution_statistics_lead_2y_fig, aes(
  x = reorder(institution, -percentage), 
  y = percentage,
  color = lead_country))+
  geom_linerange(aes(ymin=conf_int_ll, 
                      ymax=conf_int_ul))+
  geom_point(aes(size=trials, fill = lead_country), shape = 21, color="black")+
  scale_size(range = c(1,5), breaks = c(1, 10, 50, 100, 250))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(y = "Published within 2 yrs (%)", x = NULL, fill="Country", color="Country", size="Number of trials")+
  scale_color_manual(values = c(brewer.pal(5, "Set1")[-6], "Black"))+
  scale_fill_manual(values = c(brewer.pal(5, "Set1")[-6], "Black"))+
  theme(strip.background = element_blank())+
  coord_flip()

### Save figure ----

#pdf(paste0(folder_path, "data/3-analysis/output-results/figures/", "figure-inst-2y-", today, ".pdf"), height = 7, width = 7)
plot(fig)
#dev.off()



# 4. Summary results < 1 year ----

#for given institution calculate number of trials, summary results reported within 12 months after CD,
#and percentage

get_summary_results_statistics <- function(institution, institution_assignments, days_to_publ, years=1)
{
  institution_tot <- sum(
    (institution_assignments == institution |
       str_detect(institution_assignments, paste0(";",institution, "$")) |
       str_detect(institution_assignments, paste0("^", institution, ";"))), 
    na.rm = TRUE)
  institution_summary_years_after_CD <- sum(
    (institution_assignments == institution |
       str_detect(institution_assignments, paste0(";",institution, "$")) |
       str_detect(institution_assignments, paste0("^", institution, ";"))) &
      days_to_publ < years*365, na.rm = TRUE)
  institution_perc <- round(institution_summary_years_after_CD/institution_tot, 3) * 100
  institution_wilson <- DescTools::BinomCI(institution_summary_years_after_CD, institution_tot, 
                                           conf.level = 0.95, method = c("wilson", "modified wilson"))
  institution_wilson_ll <- round(institution_wilson[3], 3) * 100
  institution_wilson_ul <- round(institution_wilson[5], 3) * 100
  institution_mod_wilson_ll <- round(institution_wilson[4], 3) * 100
  institution_mod_wilson_ul <- round(institution_wilson[6], 3) * 100
  
  
  
  institution_stat <- tibble("institution" = institution, "trials" = institution_tot,
                             !!(paste0("summary_within_", years * 12, "m_after_CD")) := institution_summary_years_after_CD,
                             "percentage" = institution_perc,
                             "conf_int_ll" = institution_wilson_ll,
                             "conf_int_ul" = institution_wilson_ul,
                             "conf_int_mod_ll" = institution_mod_wilson_ll,
                             "conf_int_mod_ul" = institution_mod_wilson_ul)
  
  institution_stat <- mutate(institution_stat, 
                             conf_int_ll = ifelse((institution_summary_years_after_CD<3 | (institution_tot - institution_summary_years_after_CD) < 3),
                                                  conf_int_mod_ll, conf_int_ll),
                             conf_int_ul = ifelse((institution_summary_years_after_CD<3 | (institution_tot - institution_summary_years_after_CD) < 3),
                                                  conf_int_mod_ul, conf_int_ul)
  )
  institution_stat <- select(institution_stat, !matches("mod"))
  
  return(institution_stat)
}

## Main table for 1 year ----
institution_statistics_summary_1y <- purrr::map(institutions, get_summary_results_statistics,
                                             institution_assignments = analysis_dataset_institutions$lead_institution,
                                             days_to_publ = analysis_dataset_institutions$days_cd_to_summary)
institution_statistics_summary_1y <- do.call(rbind, institution_statistics_summary_1y)

print(institution_statistics_summary_1y, n = Inf)
institution_statistics_summary_1y$percentage <- institution_statistics_summary_1y$percentage %>% round(2)

### Add total ----
add1 <- nrow(analysis_dataset_institutions)
add2 <- sum(analysis_dataset_institutions$days_cd_to_summary < 365, na.rm = T)
add3 <- round(add2/add1, 3) * 100
add_wilson <- DescTools::BinomCI(add2, add1, conf.level = 0.95, method = c("wilson", "modified wilson"))
add_wilson_ll <- round(add_wilson[3], 3) * 100
add_wilson_ul <- round(add_wilson[5], 3) * 100

institution_statistics_summary_1y[nrow(institution_statistics_summary_1y) + 1,] <- list("Total", add1, add2, add3, add_wilson_ll, add_wilson_ul)
rm(add1, add2, add3, add_wilson_ll, add_wilson_ul)

## Save table ----
#writexl::write_xlsx(institution_statistics_summary_1y, paste0(folder_path, "data/3-analysis/output-results/tables/", "institution-summary-1year-", today, ".xlsx"))
#readr::write_csv(institution_statistics_summary_1y, paste0(folder_path, "data/3-analysis/output-results/tables/", "institution-summary-1year-", today, ".csv"))


## Figure ----

institution_statistics_summary_1y_fig <- left_join(institution_statistics_summary_1y, select(
  filter(analysis, !duplicated(lead_institution)), 
  lead_institution, lead_country), by = 
    c("institution" = "lead_institution"))

institution_statistics_summary_1y_fig <- filter(institution_statistics_summary_1y_fig, 
                                             !institution == "Total")

scaled_sizes <- (institution_statistics_summary_1y_fig$trials + 100)*0.005

fig <- ggplot(institution_statistics_summary_1y_fig, aes(
  x = reorder(institution, -percentage), 
  y = percentage,
  color = lead_country))+
  geom_linerange(aes(ymin=conf_int_ll, 
                     ymax=conf_int_ul))+
  geom_point(aes(size=trials, fill = lead_country), shape = 21, color="black")+
  scale_size(range = c(1,5), breaks = c(1, 10, 50, 100, 250))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(y = "Summary results within 1 year (%)", x = NULL, fill="Country", color="Country", size="Number of trials")+
  scale_color_manual(values = c(brewer.pal(5, "Set1")[-6], "Black"))+
  scale_fill_manual(values = c(brewer.pal(5, "Set1")[-6], "Black"))+
  theme(strip.background = element_blank())+
  coord_flip()

### Save figure ----

#pdf(paste0(folder_path, "data/3-analysis/output-results/figures/", "figure-inst-summary-1yr-", today, ".pdf"), height = 7, width = 7)
plot(fig)
#dev.off()


## In-text results (console only) ----

# Any results reporting
summary(as.factor(analysis_dataset_institutions$has_summary_results))
DescTools::BinomCI(sum(analysis_dataset_institutions$has_summary_results=="Yes"), 
                   nrow(analysis_dataset_institutions), conf.level = 0.95, 
                   method = "wilson")
summary(as_date(analysis_dataset_institutions$summary_results_date))


# 5. Any results reported ----

#for given institution calculate number of trials, trials with any results reporting,
#and publication percentage

get_missing_report_statistics <- function(institution, institution_assignments, has_publ_or_summary)
{
  institution_tot <- sum(
    (institution_assignments == institution |
       str_detect(institution_assignments, paste0(";",institution, "$")) |
       str_detect(institution_assignments, paste0("^", institution, ";"))), 
    na.rm = TRUE)
  institution_any_results <- sum(
    (institution_assignments == institution |
       str_detect(institution_assignments, paste0(";",institution, "$")) |
       str_detect(institution_assignments, paste0("^", institution, ";"))) &
      has_publ_or_summary == T)
  institution_perc <- round(institution_any_results/institution_tot, 3) * 100
  institution_wilson <- DescTools::BinomCI(institution_any_results, institution_tot, 
                                           conf.level = 0.95, method = c("wilson", "modified wilson"))
  institution_wilson_ll <- round(institution_wilson[3], 3) * 100
  institution_wilson_ul <- round(institution_wilson[5], 3) * 100
  institution_mod_wilson_ll <- round(institution_wilson[4], 3) * 100
  institution_mod_wilson_ul <- round(institution_wilson[6], 3) * 100
  
  
  
  institution_stat <- tibble("institution" = institution, "trials" = institution_tot,
                             !!(paste0("any results reporting")) := institution_any_results,
                             "percentage" = institution_perc,
                             "conf_int_ll" = institution_wilson_ll,
                             "conf_int_ul" = institution_wilson_ul,
                             "conf_int_mod_ll" = institution_mod_wilson_ll,
                             "conf_int_mod_ul" = institution_mod_wilson_ul)
  
  institution_stat <- mutate(institution_stat, 
                             conf_int_ll = ifelse((institution_any_results<3 | (institution_tot - institution_any_results) < 3),
                                                  conf_int_mod_ll, conf_int_ll),
                             conf_int_ul = ifelse((institution_any_results<3 | (institution_tot - institution_any_results) < 3),
                                                  conf_int_mod_ul, conf_int_ul)
  )
  institution_stat <- select(institution_stat, !matches("mod"))
  
  return(institution_stat)
}

## Main table on any results reporting ----
institution_statistics_any_results <- purrr::map(institutions, get_missing_report_statistics,
                                             institution_assignments = analysis_dataset_institutions$lead_institution,
                                             has_publ_or_summary = analysis_dataset_institutions$has_publ_or_summary)
institution_statistics_any_results <- do.call(rbind, institution_statistics_any_results)

print(institution_statistics_any_results, n = Inf)
institution_statistics_any_results$percentage <- institution_statistics_any_results$percentage %>% round(2)

### Add total ----
add1 <- nrow(analysis_dataset_institutions)
add2 <- sum(analysis_dataset_institutions$has_publ_or_summary == T, na.rm = T)
add3 <- round(add2/add1, 3) * 100
add_wilson <- DescTools::BinomCI(add2, add1, conf.level = 0.95, method = c("wilson", "modified wilson"))
add_wilson_ll <- round(add_wilson[3], 3) * 100
add_wilson_ul <- round(add_wilson[5], 3) * 100

institution_statistics_any_results[nrow(institution_statistics_any_results) + 1,] <- list("Total", add1, add2, add3, add_wilson_ll, add_wilson_ul)
rm(add1, add2, add3, add_wilson_ll, add_wilson_ul)

## Save table ----
#writexl::write_xlsx(institution_statistics_any_results, paste0(folder_path, "data/3-analysis/output-results/tables/", "institution-stats-anyreport-", today, ".xlsx"))
#readr::write_csv(institution_statistics_any_results, paste0(folder_path, "data/3-analysis/output-results/tables/", "institution-stats-anyreport-", today, ".csv"))


## Figure ----

institution_statistics_any_results_fig <- left_join(institution_statistics_any_results, select(
  filter(analysis, !duplicated(lead_institution)), 
  lead_institution, lead_country), by = 
    c("institution" = "lead_institution"))

institution_statistics_any_results_fig <- filter(institution_statistics_any_results_fig, 
                                             !institution == "Total")

scaled_sizes <- (institution_statistics_any_results_fig$trials + 100)*0.005

fig <- ggplot(institution_statistics_any_results_fig, aes(
  x = reorder(institution, -percentage), 
  y = percentage,
  color = lead_country))+
  geom_linerange(aes(ymin=conf_int_ll, 
                     ymax=conf_int_ul))+
  geom_point(aes(size=trials, fill = lead_country), shape = 21, color="black")+
  scale_size(range = c(1,5), breaks = c(1, 10, 50, 100, 250))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(y = "Any results reporting (%)", x = NULL, fill="Country", color="Country", size="Number of trials")+
  scale_color_manual(values = c(brewer.pal(5, "Set1")[-6], "Black"))+
  scale_fill_manual(values = c(brewer.pal(5, "Set1")[-6], "Black"))+
  theme(strip.background = element_blank())+
  coord_flip()

### Save figure ----

#pdf(paste0(folder_path, "data/3-analysis/output-results/figures/", "figure-inst-anyresults-", today, ".pdf"), height = 7, width = 7)
plot(fig)
#dev.off()




# 6. Time to reported results ----


## Kaplan-Meier plot ----

#get minimum of days to pub or to summary result & time in days that one study could be tracked
analysis_KM_data <- analysis_dataset_institutions
analysis_KM_data <- mutate(analysis_KM_data, days_to_publ = ifelse(days_to_publ <= 0, 1, days_to_publ))

analysis_KM_data <- mutate(analysis_KM_data, months_obs = case_when(
  has_publ_or_summary == T ~ days_to_publ/365.25*12,
  has_publ_or_summary == F ~ as.duration(completion_date %--% extraction_date_latest) / dmonths(1)))


#Create survival object and fit curve    
KM_curve <- survival::survfit(survival::Surv(months_obs, has_publ_or_summary) ~ 1, data = analysis_KM_data)

#Plot curve
xbreaks <- c(12, 24, 36, 48, 60, 72, 84)
xlabels <- as.character(xbreaks)

ybreaks <- c(0, 0.20, 0.40, 0.60, 0.80, 1)
ylabels <- paste0(ybreaks*100, ("%"))

KM_curve_total <- survfit2(survival::Surv(months_obs, has_publ_or_summary) ~ 1, data = analysis_KM_data) %>% 
  ggsurvfit(linewidth = 0.8) +
  labs(
    x = "Months",
    y = "Unpublished studies (%)"
  ) + 
  add_confidence_interval() +
  scale_x_continuous(
    breaks = xbreaks,
    labels = xlabels,
    limits = c(1, 88),
    expand = c(0,0)) +
  scale_y_continuous(
    breaks = ybreaks,
    labels = ylabels,
    limits = c(0,1.01),
    expand = c(0,0)
    )+
  coord_cartesian(xlim = c(1, 86))+
  geom_vline(xintercept = 24, color = "gray", linetype = "dashed")  # Vertical line at x = 24


### Save figure ----
#pdf(paste0(folder_path, "data/3-analysis/output-results/figures/", "kaplan-meier-", today, ".pdf"), height = 4, width = 5)
plot(KM_curve_total)
#dev.off()


### Per country ----
KM_curve_country <- survfit2(survival::Surv(months_obs, has_publ_or_summary) ~ lead_country, data = analysis_KM_data) %>% 
  ggsurvfit(linewidth = 0.8) +
  labs(
    x = "Months",
    y = "Unpublished studies (%)"
  ) + 
  scale_color_manual(values = c(brewer.pal(5, "Set1")[-6], "Black"))+
  scale_x_continuous(
    breaks = xbreaks,
    labels = xlabels,
#    limits = c(1, 88),
    expand = c(0,0)) +
  scale_y_continuous(
    breaks = ybreaks,
    labels = ylabels,
    limits = c(0,1.01),
    expand = c(0,0)
  )+
#  coord_cartesian(xlim = c(1, 86))+
  geom_vline(xintercept = 24, color = "gray", linetype = "dashed")  # Vertical line at x = 24


#### Save figure ----
#pdf(paste0(folder_path, "data/3-analysis/output-results/figures/", "kaplan-meier-country-", today, ".pdf"), height = 4, width = 5.5)
plot(KM_curve_country)
#dev.off()


## Median time to reporting ----
#Per institution, country, total
#summary(analysis_dataset_institutions$days_to_publ)

#for given institution calculate median time to results reporting and iqr

get_time_reporting <- function(institution)
{
  institutions_KM_data <- filter(analysis_KM_data,
    (lead_institution == institution |
       str_detect(lead_institution, paste0(";",institution, "$")) |
       str_detect(lead_institution, paste0("^", institution, ";"))))
  KM_curve_institution <- survival::survfit(survival::Surv(institutions_KM_data$months_obs, institutions_KM_data$has_publ_or_summary) ~ 1)
  institution_median <- unname(summary(KM_curve_institution)$table['median'])/12*365.25
  institution_iqr <- unname(quantile(KM_curve_institution)[[1]][3] - quantile(KM_curve_institution)[[1]][1])/12*365.25

  institution_stat <- tibble("institution" = institution,
                             "median" = institution_median,
                             "iqr" = institution_iqr)
  return(institution_stat)
}


## Main table for median reporting times ----
institution_median_reporting <- purrr::map(institutions, get_time_reporting)
institution_median_reporting <- do.call(rbind, institution_median_reporting)

print(institution_median_reporting, n = Inf)

### Add country totals ----

country_names <- c("Denmark", "Finland", "Iceland", "Norway", "Sweden")

get_country_time_reporting <- function(country_names)
{
  country_KM_data <- filter(analysis_KM_data, lead_country == country_names)
  KM_curve_country <- survival::survfit(survival::Surv(country_KM_data$months_obs, country_KM_data$has_publ_or_summary) ~ 1)
  country_median <- unname(summary(KM_curve_country)$table['median'])/12*365.25
  country_iqr <- unname(quantile(KM_curve_country)[[1]][3] - quantile(KM_curve_country)[[1]][1])/12*365.25
  
  country_stat <- tibble("institution" = country_names,
                             "median" = country_median,
                             "iqr" = country_iqr)
  
  return(country_stat)
}

country_median_reporting <- purrr::map(country_names, get_country_time_reporting)
country_median_reporting <- do.call(rbind, country_median_reporting)

print(country_median_reporting, n = Inf)

institution_median_reporting <- rbind(institution_median_reporting, country_median_reporting)

### Add total ----
add1 <- unname(summary(KM_curve)$table['median'])/12*365.25
add2 <- unname(quantile(KM_curve)[[1]][3] - quantile(KM_curve)[[1]][1])/12*365.25

institution_median_reporting[nrow(institution_median_reporting) + 1,] <- list("Total", add1, add2)
rm(add1, add2)

## Save table ----
#writexl::write_xlsx(institution_median_reporting, paste0(folder_path, "data/3-analysis/output-results/tables/", "institution-median-rep-", today, ".xlsx"))
#readr::write_csv(institution_median_reporting, paste0(folder_path, "data/3-analysis/output-results/tables/", "institution-median-rep-", today, ".csv"))


## Additional calculations (console only) ----

# Number of trials with any results reporting before completion date
sum(analysis_dataset_institutions$days_to_publ<0, na.rm = T)
sum(analysis_dataset_institutions$days_to_publ<0, na.rm = T)/2113


# 7. Secondary outcomes ----


## Planned number of participants in unreported trials (console only) ----

analysis_no_results <- filter(analysis_dataset_institutions, has_publ_or_summary == F)
summary(analysis_no_results$enrollment)
IQR(analysis_no_results$enrollment, na.rm = T)
sum(analysis_no_results$enrollment, na.rm = T)

#Per country
aggregate(analysis_no_results$enrollment, by=list(Category=analysis_no_results$lead_country), FUN=sum, na.rm =T)


## Planned (or actual) number of participants in reported trials (console only) ----
analysis_results <- filter(analysis_dataset_institutions, has_publ_or_summary == T)
summary(analysis_results$enrollment)
IQR(analysis_results$enrollment, na.rm = T)
sum(analysis_results$enrollment, na.rm = T)

## Prospectively registered trials ---- 

#Use trials only registered at ClinicalTrials.gov
analysis_dataset_institutions_ctgov <- filter(analysis_dataset_institutions, registered_euctr == "No")

#In-text results (console only)
furniture::table1(analysis_dataset_institutions_ctgov, time_to_registration,
                  na.rm = F)

#for given institution calculate number of trials, prospectively reported trials,
#and percentage

get_prospective_statistics <- function(institution, institution_assignments, start_date, registration_date)
{
  institution_tot <- sum(
    (institution_assignments == institution |
       str_detect(institution_assignments, paste0(";",institution, "$")) |
       str_detect(institution_assignments, paste0("^", institution, ";"))), 
    na.rm = TRUE)
  institution_prospective <- sum(
    (institution_assignments == institution |
       str_detect(institution_assignments, paste0(";",institution, "$")) |
       str_detect(institution_assignments, paste0("^", institution, ";"))) &
     start_date > registration_date, 
     na.rm = TRUE)
  institution_perc <- round(institution_prospective/institution_tot, 3) * 100
  institution_wilson <- DescTools::BinomCI(institution_prospective, institution_tot, 
                                           conf.level = 0.95, method = c("wilson", "modified wilson"))
  institution_wilson_ll <- round(institution_wilson[3], 3) * 100
  institution_wilson_ul <- round(institution_wilson[5], 3) * 100
  institution_mod_wilson_ll <- round(institution_wilson[4], 3) * 100
  institution_mod_wilson_ul <- round(institution_wilson[6], 3) * 100
  
  
  
  institution_stat <- tibble("institution" = institution, "trials" = institution_tot,
                             "prospectively registered" = institution_prospective,
                             "percentage" = institution_perc,
                             "conf_int_ll" = institution_wilson_ll,
                             "conf_int_ul" = institution_wilson_ul,
                             "conf_int_mod_ll" = institution_mod_wilson_ll,
                             "conf_int_mod_ul" = institution_mod_wilson_ul)
  
  institution_stat <- mutate(institution_stat, 
                             conf_int_ll = ifelse((institution_prospective<3 | (institution_tot - institution_prospective) < 3),
                                                  conf_int_mod_ll, conf_int_ll),
                             conf_int_ul = ifelse((institution_prospective<3 | (institution_tot - institution_prospective) < 3),
                                                  conf_int_mod_ul, conf_int_ul)
  )
  institution_stat <- select(institution_stat, !matches("mod"))
  
  return(institution_stat)
}

### Main table for prospective registration ----
institution_statistics_prospective <- purrr::map(institutions, get_prospective_statistics,
                                                institution_assignments = analysis_dataset_institutions_ctgov$lead_institution,
                                                start_date = analysis_dataset_institutions_ctgov$start_date,
                                                registration_date = analysis_dataset_institutions_ctgov$registration_date)
institution_statistics_prospective <- do.call(rbind, institution_statistics_prospective)

print(institution_statistics_prospective, n = Inf)
institution_statistics_prospective$percentage <- institution_statistics_prospective$percentage %>% round(2)

#### Add total ----
add1 <- nrow(analysis_dataset_institutions_ctgov)
add2 <- sum(analysis_dataset_institutions_ctgov$start_date >
              analysis_dataset_institutions_ctgov$registration_date,
            na.rm = T)
add3 <- round(add2/add1, 3) * 100
add_wilson <- DescTools::BinomCI(add2, add1, conf.level = 0.95, method = c("wilson", "modified wilson"))
add_wilson_ll <- round(add_wilson[3], 3) * 100
add_wilson_ul <- round(add_wilson[5], 3) * 100

institution_statistics_prospective[nrow(institution_statistics_prospective) + 1,] <- list("Total", add1, add2, add3, add_wilson_ll, add_wilson_ul)
rm(add1, add2, add3, add_wilson_ll, add_wilson_ul)

#### Save table ----
#writexl::write_xlsx(institution_statistics_prospective, paste0(folder_path, "data/3-analysis/output-results/tables/", "institution-prospective-", today, ".xlsx"))
#readr::write_csv(institution_statistics_prospective, paste0(folder_path, "data/3-analysis/output-results/tables/", "institution-prospective-", today, ".csv"))


### Figure ----

institution_statistics_prospective_fig <- left_join(institution_statistics_prospective, select(
  filter(analysis, !duplicated(lead_institution)), 
  lead_institution, lead_country), by = 
    c("institution" = "lead_institution"))

institution_statistics_prospective_fig <- filter(institution_statistics_prospective_fig, 
                                                !institution == "Total")

scaled_sizes <- (institution_statistics_prospective_fig$trials + 100)*0.005

fig <- ggplot(institution_statistics_prospective_fig, aes(
  x = reorder(institution, -percentage), 
  y = percentage,
  color = lead_country))+
  geom_linerange(aes(ymin=conf_int_ll, 
                     ymax=conf_int_ul))+
  geom_point(aes(size=trials, fill = lead_country), shape = 21, color="black")+
  scale_size(range = c(1,5), breaks = c(1, 10, 50, 100, 250))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(y = "Prospectively registered trials (%)", x = NULL, fill="Country", color="Country", size="Number of trials")+
  scale_color_manual(values = c(brewer.pal(5, "Set1")[-6], "Black"))+
  scale_fill_manual(values = c(brewer.pal(5, "Set1")[-6], "Black"))+
  theme(strip.background = element_blank())+
  coord_flip()

#### Save figure ----

#pdf(paste0(folder_path, "data/3-analysis/output-results/figures/", "figure-inst-prospective-", today, ".pdf"), height = 7, width = 7)
plot(fig)
#dev.off()


### Kaplan-Meier plot for time to registration ----

#get days to registration (after start) & time in days that one study could be tracked
analysis_dataset_institutions_ctgov <- mutate(analysis_dataset_institutions_ctgov, days_to_reg = ifelse(days_reg_to_start >= 0, 1, -days_reg_to_start))
analysis_dataset_institutions_ctgov$has_registration <- 2 #All are "events", none is censored

#Create survival object and fit curve    
KM_curve_timetoreg <- survival::survfit(survival::Surv(days_to_reg, has_registration) ~ 1, data = analysis_dataset_institutions_ctgov)

#Plot curve
breaks <- c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000)
labels <- as.character(breaks)

KM_plot_timetoreg <- survfit2(survival::Surv(days_to_reg, has_registration) ~ 1, data = analysis_dataset_institutions_ctgov) %>% 
  ggsurvfit(linewidth = 0.8) +
  labs(
    x = "Days to registration after trial start",
    y = "Unregistered studies (%)"
  ) + 
  add_confidence_interval() +
  scale_ggsurvfit() +
  scale_x_continuous(
    trans = "log",
    breaks = breaks,
    labels = labels,
    limits = c(0.5, 8000)  # Adjusting the limits to include 0.5 to start at 1
  ) +
  coord_cartesian(xlim = c(1.5, 8000))

### Save figure ----
#pdf(paste0(folder_path, "data/3-analysis/output-results/figures/", "kaplan-meier-time-to-reg-", today, ".pdf"), height = 5, width = 7)
plot(KM_plot_timetoreg)
#dev.off()



## No cross-registration (in console only) ----

analysis_med_prod_trials <- filter(analysis_dataset_institutions, registry == "ctgov" &
                                     intervention_type == "Medicinal product" &
                                     !grepl("Phase 1", phase, ignore.case = T))

summary(as.factor(analysis_med_prod_trials$crossregistration_check))
table(analysis_med_prod_trials$crossregistration_check == "None found")
DescTools::BinomCI(sum(analysis_med_prod_trials$crossregistration_check == "None found"), 
                   nrow(analysis_med_prod_trials), conf.level = 0.95, 
                   method = "wilson")



# 8. Sensitivity analyses ----

## Main results (totals), binary primary outcomes ----
# Calculate all binary primary outcomes and present in table

get_primary_results <- function(dataset, days_to_publ, days_cd_to_summary, has_publ_or_summary)
{
  total = nrow(dataset)
  rep_2_yrs <- sum(days_to_publ < 2*365, na.rm = T)
  rep_2_yrs_perc <- round(rep_2_yrs/total, 3) * 100
  rep_2_yrs_wilson <- DescTools::BinomCI(rep_2_yrs, total, conf.level = 0.95, method = c("wilson", "modified wilson"))
  rep_2_yrs_wilson_ll <- round(rep_2_yrs_wilson[3], 3) * 100
  rep_2_yrs_wilson_ul <- round(rep_2_yrs_wilson[5], 3) * 100

  sum_1_yr <- sum(days_cd_to_summary < 365, na.rm = T)
  sum_1_yr_perc <- round(sum_1_yr/total, 3) * 100
  sum_1_yr_wilson <- DescTools::BinomCI(sum_1_yr, total, conf.level = 0.95, method = c("wilson", "modified wilson"))
  sum_1_yr_wilson_ll <- round(sum_1_yr_wilson[3], 3) * 100
  sum_1_yr_wilson_ul <- round(sum_1_yr_wilson[5], 3) * 100

  any_results <- sum(has_publ_or_summary == T, na.rm = T)
  any_results_perc <- round(any_results/total, 3) * 100
  any_results_wilson <- DescTools::BinomCI(any_results, total, conf.level = 0.95, method = c("wilson", "modified wilson"))
  any_results_wilson_ll <- round(any_results_wilson[3], 3) * 100
  any_results_wilson_ul <- round(any_results_wilson[5], 3) * 100  
  
  sens_matrix <- tibble("Reporting < 2 yrs" = paste0(rep_2_yrs, "/", total, ", ", rep_2_yrs_perc, "%", " (",
                                             "95%CI ", rep_2_yrs_wilson_ll, "-", rep_2_yrs_wilson_ul, "%)"),
                        "Summary results < 1 yr" = paste0(sum_1_yr, "/", total, ", ", sum_1_yr_perc, "%", " (",
                                                  "95%CI ", sum_1_yr_wilson_ll, "-", sum_1_yr_wilson_ul, "%)"),
                        "Any results reporting" = paste0(any_results, "/", total, ", ", any_results_perc, "%", " (",
                                                  "95%CI ", any_results_wilson_ll, "-", any_results_wilson_ul, "%)"))

  return(sens_matrix)
}


### Main results ----
sens_main <- get_primary_results(analysis_dataset_institutions, analysis_dataset_institutions$days_to_publ, 
                                        analysis_dataset_institutions$days_cd_to_summary, analysis_dataset_institutions$has_publ_or_summary)

sens_main$analysis <- "Main analysis"

### Restrict to only RCTs ----
sens_rcts <- filter(analysis_dataset_institutions, is_randomised == "Yes" & is_controlled == "Yes")

sens_rcts <- get_primary_results(sens_rcts, sens_rcts$days_to_publ, 
                                 sens_rcts$days_cd_to_summary, sens_rcts$has_publ_or_summary)

sens_rcts$analysis <- "Only RCTs"

### Restrict to only EUCTR ----
sens_euctr <- filter(analysis_dataset_institutions, !is.na(eudract_id))

sens_euctr <- get_primary_results(sens_euctr, sens_euctr$days_to_publ, 
                                  sens_euctr$days_cd_to_summary, sens_euctr$has_publ_or_summary)

sens_euctr$analysis <- "Restricted to EUCTR"

### Restrict to only CTgov ----
sens_ctgov <- filter(analysis_dataset_institutions, is.na(eudract_id))

sens_ctgov <- get_primary_results(sens_ctgov, sens_ctgov$days_to_publ, 
                                  sens_ctgov$days_cd_to_summary, sens_ctgov$has_publ_or_summary)

sens_ctgov$analysis <- "Restricted to ClinicalTrials.gov"

### Exclude trials with status missing or unknown ----
sens_known <- filter(analysis_dataset_institutions, !recruitment_status == "Unknown status")

sens_known <- get_primary_results(sens_known, sens_known$days_to_publ, 
                                  sens_known$days_cd_to_summary, sens_known$has_publ_or_summary)

sens_known$analysis <- "Exclude trials with unknown status"

### Restrict to completed trials ----
sens_completed <- filter(analysis_dataset_institutions, grepl("Completed", recruitment_status, ignore.case = T))

sens_completed <- get_primary_results(sens_completed, sens_completed$days_to_publ, 
                                      sens_completed$days_cd_to_summary, sens_completed$has_publ_or_summary)
sens_completed$analysis <- "Only completed trials"


### Use primary completion date instead of global ----
sens_primarycompletion <- filter(analysis_dataset_institutions, !is.na(primary_completion_date))

sens_primarycompletion <- sens_primarycompletion %>%
  mutate(days_to_publ_primarycompletion = pmin(days_pcd_to_publication,   #get minimum of days to pub or to summary result
                             days_pcd_to_summary, na.rm = TRUE))

sens_primarycompletion <- get_primary_results(sens_primarycompletion, sens_primarycompletion$days_to_publ_primarycompletion, 
                          sens_primarycompletion$days_pcd_to_summary, sens_primarycompletion$has_publ_or_summary)
sens_primarycompletion$analysis <- "Primary completion date"



### Restrict to CTgov-only-registered trials with "actual" global completion date (not "estimated") ----
sens_ctgov_actual <- filter(analysis_dataset_institutions, is.na(eudract_id) & completion_date_type == "Actual")

sens_ctgov_actual <- get_primary_results(sens_ctgov_actual, sens_ctgov_actual$days_to_publ, 
                                         sens_ctgov_actual$days_cd_to_summary, sens_ctgov_actual$has_publ_or_summary)
sens_ctgov_actual$analysis <- "Restricted to ClinicalTrials.gov with actual completion date"




### Combine rows to matrix ----

sens_matrix <- rbind(sens_main, sens_rcts, sens_euctr, sens_ctgov, sens_known, sens_completed, sens_primarycompletion, sens_ctgov_actual)
sens_matrix <- relocate(sens_matrix, analysis)

### Save sensitivity analysis matrix table ----

#writexl::write_xlsx(sens_matrix, paste0(folder_path, "data/3-analysis/output-results/tables/", "sens-matrix-", today, ".xlsx"))
#readr::write_csv(sens_matrix, paste0(folder_path, "data/3-analysis/output-results/tables/", "sens-matrix-", today, ".csv"))



## Count trials per institution only if single-sponsored or institution listed first among sponsors ----

### Results < 2 years ----
sens_institution_statistics_lead_2y <- purrr::map(institutions, get_institution_statistics,
                                             institution_assignments = analysis_dataset_institutions$single_sponsor,
                                             days_to_publ = analysis_dataset_institutions$days_to_publ)
sens_institution_statistics_lead_2y <- do.call(rbind, sens_institution_statistics_lead_2y)

print(sens_institution_statistics_lead_2y, n = Inf)
sens_institution_statistics_lead_2y$percentage <- sens_institution_statistics_lead_2y$percentage %>% round(2)


#### Save table ----
#writexl::write_xlsx(sens_institution_statistics_lead_2y, paste0(folder_path, "data/3-analysis/output-results/tables/", "sens-single-sponsor-institution-stats-2years-", today, ".xlsx"))
#readr::write_csv(sens_institution_statistics_lead_2y, paste0(folder_path, "data/3-analysis/output-results/tables/", "sens-single-sponsor-institution-stats-2years-", today, ".csv"))




### Summary results < 1 year ----
sens_institution_statistics_summary_1y <- purrr::map(institutions, get_summary_results_statistics,
                                                institution_assignments = analysis_dataset_institutions$single_sponsor,
                                                days_to_publ = analysis_dataset_institutions$days_cd_to_summary)
sens_institution_statistics_summary_1y <- do.call(rbind, sens_institution_statistics_summary_1y)

print(sens_institution_statistics_summary_1y, n = Inf)
sens_institution_statistics_summary_1y$percentage <- sens_institution_statistics_summary_1y$percentage %>% round(2)

#### Save table ----
#writexl::write_xlsx(sens_institution_statistics_summary_1y, paste0(folder_path, "data/3-analysis/output-results/tables/", "sens-single-sponsor-institution-summary-1year-", today, ".xlsx"))
#readr::write_csv(sens_institution_statistics_summary_1y, paste0(folder_path, "data/3-analysis/output-results/tables/", "sens-single-sponsor-institution-summary-1year-", today, ".csv"))


### Any results reporting ----
sens_institution_statistics_any_results <- purrr::map(institutions, get_missing_report_statistics,
                                                 institution_assignments = analysis_dataset_institutions$single_sponsor,
                                                 has_publ_or_summary = analysis_dataset_institutions$has_publ_or_summary)
sens_institution_statistics_any_results <- do.call(rbind, sens_institution_statistics_any_results)

print(sens_institution_statistics_any_results, n = Inf)
sens_institution_statistics_any_results$percentage <- sens_institution_statistics_any_results$percentage %>% round(2)


#### Save table ----
#writexl::write_xlsx(sens_institution_statistics_any_results, paste0(folder_path, "data/3-analysis/output-results/tables/", "sens-single-sponsor-institution-stats-anyreport-", today, ".xlsx"))
#readr::write_csv(sens_institution_statistics_any_results, paste0(folder_path, "data/3-analysis/output-results/tables/", "sens-single-sponsor-institution-stats-anyreport-", today, ".csv"))



# 9. Subgroup analyses ----

#For all subgroups, calculate binary primary outcomes and present in table

get_primary_results_subgroups <- function(category, category_var, days_to_publ, days_cd_to_summary, has_publ_or_summary)
{
  total <- sum(grepl(category, category_var, ignore.case = T), na.rm = TRUE)
  rep_2_yrs <- sum(grepl(category, category_var, ignore.case = T) & days_to_publ < 2*365, na.rm = T)
  rep_2_yrs_perc <- round(rep_2_yrs/total, 3) * 100
  rep_2_yrs_wilson <- DescTools::BinomCI(rep_2_yrs, total, conf.level = 0.95, method = c("wilson", "modified wilson"))
  rep_2_yrs_wilson_ll <- round(rep_2_yrs_wilson[3], 3) * 100
  rep_2_yrs_wilson_ul <- round(rep_2_yrs_wilson[5], 3) * 100
  
  sum_1_yr <- sum(grepl(category, category_var, ignore.case = T) & days_cd_to_summary < 365, na.rm = T)
  sum_1_yr_perc <- round(sum_1_yr/total, 3) * 100
  sum_1_yr_wilson <- DescTools::BinomCI(sum_1_yr, total, conf.level = 0.95, method = c("wilson", "modified wilson"))
  sum_1_yr_wilson_ll <- round(sum_1_yr_wilson[3], 3) * 100
  sum_1_yr_wilson_ul <- round(sum_1_yr_wilson[5], 3) * 100
  
  any_results <- sum(grepl(category, category_var, ignore.case = T) & has_publ_or_summary == T, na.rm = T)
  any_results_perc <- round(any_results/total, 3) * 100
  any_results_wilson <- DescTools::BinomCI(any_results, total, conf.level = 0.95, method = c("wilson", "modified wilson"))
  any_results_wilson_ll <- round(any_results_wilson[3], 3) * 100
  any_results_wilson_ul <- round(any_results_wilson[5], 3) * 100  
  
  subgroup_matrix <- tibble("Subgroup" = category,
                        "Reporting < 2 yrs" = paste0(rep_2_yrs, "/", total, ", ", rep_2_yrs_perc, "%", " (",
                                                     "95%CI ", rep_2_yrs_wilson_ll, "-", rep_2_yrs_wilson_ul, "%)"),
                        "Summary results < 1 yr" = paste0(sum_1_yr, "/", total, ", ", sum_1_yr_perc, "%", " (",
                                                          "95%CI ", sum_1_yr_wilson_ll, "-", sum_1_yr_wilson_ul, "%)"),
                        "Any results reporting" = paste0(any_results, "/", total, ", ", any_results_perc, "%", " (",
                                                         "95%CI ", any_results_wilson_ll, "-", any_results_wilson_ul, "%)"))
  
  subgroup_fig <- tibble("subgroup" = category, "total" = total, "rep_2_yrs" = rep_2_yrs, 
                         "rep_2_yrs_perc" = rep_2_yrs_perc, "rep_2_yrs_wilson_ll" = rep_2_yrs_wilson_ll,
                         "rep_2_yrs_wilson_ul" = rep_2_yrs_wilson_ul, "sum_1_yr" = sum_1_yr, 
                         "sum_1_yr_perc" = sum_1_yr_perc, "sum_1_yr_wilson_ll" = sum_1_yr_wilson_ll,
                         "sum_1_yr_wilson_ul" = sum_1_yr_wilson_ul, "any_results" = any_results,
                         "any_results_perc" = any_results_perc, "any_results_wilson_ll" = any_results_wilson_ll,
                         "any_results_wilson_ul" = any_results_wilson_ul
                         )
  
  return(subgroup_matrix)
}


## Country (Denmark, Finland, Iceland, Norway, Sweden) ----

### Figure ----

get_country_subgroups <- function(category, category_var, days_to_publ, days_cd_to_summary, has_publ_or_summary)
{
  total <- sum(grepl(category, category_var, ignore.case = T), na.rm = TRUE)
  rep_2_yrs <- sum(grepl(category, category_var, ignore.case = T) & days_to_publ < 2*365, na.rm = T)
  rep_2_yrs_perc <- round(rep_2_yrs/total, 3) * 100
  rep_2_yrs_wilson <- DescTools::BinomCI(rep_2_yrs, total, conf.level = 0.95, method = c("wilson", "modified wilson"))
  rep_2_yrs_wilson_ll <- round(rep_2_yrs_wilson[3], 3) * 100
  rep_2_yrs_wilson_ul <- round(rep_2_yrs_wilson[5], 3) * 100
  
  sum_1_yr <- sum(grepl(category, category_var, ignore.case = T) & days_cd_to_summary < 365, na.rm = T)
  sum_1_yr_perc <- round(sum_1_yr/total, 3) * 100
  sum_1_yr_wilson <- DescTools::BinomCI(sum_1_yr, total, conf.level = 0.95, method = c("wilson", "modified wilson"))
  sum_1_yr_wilson_ll <- round(sum_1_yr_wilson[3], 3) * 100
  sum_1_yr_wilson_ul <- round(sum_1_yr_wilson[5], 3) * 100
  
  any_results <- sum(grepl(category, category_var, ignore.case = T) & has_publ_or_summary == T, na.rm = T)
  any_results_perc <- round(any_results/total, 3) * 100
  any_results_wilson <- DescTools::BinomCI(any_results, total, conf.level = 0.95, method = c("wilson", "modified wilson"))
  any_results_wilson_ll <- round(any_results_wilson[3], 3) * 100
  any_results_wilson_ul <- round(any_results_wilson[5], 3) * 100  
  
  subgroup_fig <- tibble("subgroup" = category, "total" = total, "rep_2_yrs" = rep_2_yrs, 
                         "rep_2_yrs_perc" = rep_2_yrs_perc, "rep_2_yrs_wilson_ll" = rep_2_yrs_wilson_ll,
                         "rep_2_yrs_wilson_ul" = rep_2_yrs_wilson_ul, "sum_1_yr" = sum_1_yr, 
                         "sum_1_yr_perc" = sum_1_yr_perc, "sum_1_yr_wilson_ll" = sum_1_yr_wilson_ll,
                         "sum_1_yr_wilson_ul" = sum_1_yr_wilson_ul, "any_results" = any_results,
                         "any_results_perc" = any_results_perc, "any_results_wilson_ll" = any_results_wilson_ll,
                         "any_results_wilson_ul" = any_results_wilson_ul
  )
  
  return(subgroup_fig)
}



subgroups_country_fig <- purrr::map(country_names, get_country_subgroups,
                                category_var = analysis_dataset_institutions$lead_country,
                                days_to_publ = analysis_dataset_institutions$days_to_publ,
                                days_cd_to_summary = analysis_dataset_institutions$days_cd_to_summary,
                                has_publ_or_summary = analysis_dataset_institutions$has_publ_or_summary)
subgroups_country_fig <- do.call(rbind, subgroups_country_fig)


#### Figure rep 2 yrs ----


scaled_sizes <- (subgroups_country_fig$total + 100)*0.005

fig <- ggplot(subgroups_country_fig, aes(
  x = reorder(subgroup, -rep_2_yrs_perc), 
  y = rep_2_yrs_perc,
  color = subgroup))+
  geom_linerange(aes(ymin=rep_2_yrs_wilson_ll, 
                     ymax=rep_2_yrs_wilson_ul))+
  ylim(0,100) +
  geom_point(aes(size=total, fill = subgroup), shape = 21, color="black")+
  scale_size(range = c(1,5), breaks = c(10, 100, 250, 500, 1000))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(y = "Published within 2 yrs (%)", x = NULL, fill="Country", color="Country", size="Number of trials")+
  scale_color_manual(values = c(brewer.pal(5, "Set1")[-6], "Black"))+
  scale_fill_manual(values = c(brewer.pal(5, "Set1")[-6], "Black"))+
  theme(strip.background = element_blank(),
        aspect.ratio = 1/2)+
  coord_flip()

#### Save figure ----

#pdf(paste0(folder_path, "data/3-analysis/output-results/figures/", "country-rep-2-yrs-", today, ".pdf"), height = 4, width = 4)
plot(fig)
#dev.off()

#### Figure sum 1 yr ----

fig <- ggplot(subgroups_country_fig, aes(
  x = reorder(subgroup, -sum_1_yr_perc), 
  y = sum_1_yr_perc,
  color = subgroup))+
  geom_linerange(aes(ymin=sum_1_yr_wilson_ll, 
                     ymax=sum_1_yr_wilson_ul))+
  ylim(0,100) +
  geom_point(aes(size=total, fill = subgroup), shape = 21, color="black")+
  scale_size(range = c(1,5), breaks = c(10, 100, 250, 500, 1000))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(y = "Summary results within 1 yr (%)", x = NULL, fill="Country", color="Country", size="Number of trials")+
  scale_color_manual(values = c(brewer.pal(5, "Set1")[-6], "Black"))+
  scale_fill_manual(values = c(brewer.pal(5, "Set1")[-6], "Black"))+
  theme(strip.background = element_blank(),
        aspect.ratio = 1/2)+
  coord_flip()

#### Save figure ----

#pdf(paste0(folder_path, "data/3-analysis/output-results/figures/", "country-sum-1-yr-", today, ".pdf"), height = 4, width = 4)
plot(fig)
#dev.off()

#### Figure any results ----

fig <- ggplot(subgroups_country_fig, aes(
  x = reorder(subgroup, -any_results_perc), 
  y = any_results_perc,
  color = subgroup))+
  geom_linerange(aes(ymin=any_results_wilson_ll, 
                     ymax=any_results_wilson_ul))+
  ylim(0,100) +
  geom_point(aes(size=total, fill = subgroup), shape = 21, color="black")+
  scale_size(range = c(1,5), breaks = c(10, 100, 250, 500, 1000))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(y = "Any results reported (%)", x = NULL, fill="Country", color="Country", size="Number of trials")+
  scale_color_manual(values = c(brewer.pal(5, "Set1")[-6], "Black"))+
  scale_fill_manual(values = c(brewer.pal(5, "Set1")[-6], "Black"))+
  theme(strip.background = element_blank(),
        aspect.ratio = 1/2)+
  coord_flip()

### Save figure ----

#pdf(paste0(folder_path, "data/3-analysis/output-results/figures/", "country-any-results-", today, ".pdf"), height = 4, width = 4)
plot(fig)
#dev.off()

#Comment: The 3 country figures were edited for esthetics in external software.


### Results for appendix table ----

subgroups_country <- purrr::map(country_names, get_primary_results_subgroups,
                                                 category_var = analysis_dataset_institutions$lead_country,
                                                 days_to_publ = analysis_dataset_institutions$days_to_publ,
                                                 days_cd_to_summary = analysis_dataset_institutions$days_cd_to_summary,
                                                 has_publ_or_summary = analysis_dataset_institutions$has_publ_or_summary)
subgroups_country <- do.call(rbind, subgroups_country)


## Intervention type (medicinal products vs. other interventions)

intervention_type <- c("Medicinal product", "Not medicinal product")

subgroups_intervention <- purrr::map(intervention_type, get_primary_results_subgroups,
                                category_var = analysis_dataset_institutions$intervention_type,
                                days_to_publ = analysis_dataset_institutions$days_to_publ,
                                days_cd_to_summary = analysis_dataset_institutions$days_cd_to_summary,
                                has_publ_or_summary = analysis_dataset_institutions$has_publ_or_summary)
subgroups_intervention <- do.call(rbind, subgroups_intervention)


## Multicenter vs not multicenter trials

multicenter_cat <- c("Yes", "No")

subgroups_multicentric <- purrr::map(multicenter_cat, get_primary_results_subgroups,
                                     category_var = analysis_dataset_institutions$is_multicentric,
                                     days_to_publ = analysis_dataset_institutions$days_to_publ,
                                     days_cd_to_summary = analysis_dataset_institutions$days_cd_to_summary,
                                     has_publ_or_summary = analysis_dataset_institutions$has_publ_or_summary)
subgroups_multicentric <- do.call(rbind, subgroups_multicentric)

subgroups_multicentric <- mutate(subgroups_multicentric, Subgroup = case_when(
  Subgroup == "Yes" ~ "Multi-center",
  Subgroup == "No" ~ "Single-center"
))

## Industry-sponsored trials vs. other trials

sponsor_cat <- c("Industry or mixed", "Only non-industry")

subgroups_spons <- purrr::map(sponsor_cat, get_primary_results_subgroups,
                                     category_var = analysis_dataset_institutions$main_sponsor,
                                     days_to_publ = analysis_dataset_institutions$days_to_publ,
                                     days_cd_to_summary = analysis_dataset_institutions$days_cd_to_summary,
                                     has_publ_or_summary = analysis_dataset_institutions$has_publ_or_summary)
subgroups_spons <- do.call(rbind, subgroups_spons)


## Enrollment subgroups (tertiles), as given in registry

tertiles = quantile(analysis_dataset_institutions$enrollment, c(0:3/3), na.rm = T)

analysis_dataset_institutions$enrollment_tertiles = with(analysis_dataset_institutions, 
               cut(enrollment, 
                   tertiles, 
                   include.lowest = T, 
                   labels = c("1-30", "31-96", "97-150,000")))

tertile_cat <- c("1-30", "31-96", "97-150,000")

subgroups_enroll <- purrr::map(tertile_cat, get_primary_results_subgroups,
                              category_var = analysis_dataset_institutions$enrollment_tertiles,
                              days_to_publ = analysis_dataset_institutions$days_to_publ,
                              days_cd_to_summary = analysis_dataset_institutions$days_cd_to_summary,
                              has_publ_or_summary = analysis_dataset_institutions$has_publ_or_summary)
subgroups_enroll <- do.call(rbind, subgroups_enroll)


## Combine rows to matrix

subgroup_matrix <- rbind(subgroups_country, subgroups_intervention, subgroups_multicentric, subgroups_spons, subgroups_enroll)

### Save subgroup analysis matrix table ----

#writexl::write_xlsx(subgroup_matrix, paste0(folder_path, "data/3-analysis/output-results/tables/", "subgroup-matrix-", today, ".xlsx"))
#readr::write_csv(subgroup_matrix, paste0(folder_path, "data/3-analysis/output-results/tables/", "subgroup-matrix-", today, ".csv"))


### Post-hoc statistical significance tests (console only) ----

#We perform 15 tests and apply a Bonferroni-corrected significance threshold of 0.05/15
#That is, p-values below 0.0033333 are interpreted as a stat. significant subgroup association

#Country
subgroups_country$rep_2_yrs_1 <- as.numeric(str_extract(subgroups_country$`Reporting < 2 yrs`, "^[0-9]*"))
subgroups_country$rep_2_yrs_0 <- as.numeric(str_extract(subgroups_country$`Reporting < 2 yrs`, "(?<=/)\\d+(?=,)")) - subgroups_country$rep_2_yrs_1

subgroups_country$sum_1_yr_1 <- as.numeric(str_extract(subgroups_country$`Summary results < 1 yr`, "^[0-9]*"))
subgroups_country$sum_1_yr_0 <- as.numeric(str_extract(subgroups_country$`Summary results < 1 yr`, "(?<=/)\\d+(?=,)")) - subgroups_country$sum_1_yr_1

subgroups_country$any_res_1 <- as.numeric(str_extract(subgroups_country$`Any results reporting`, "^[0-9]*"))
subgroups_country$any_res_0 <- as.numeric(str_extract(subgroups_country$`Any results reporting`, "(?<=/)\\d+(?=,)")) - subgroups_country$any_res_1

#Here using simulated p-value (version of Fisher's exact test) since we have small cell sizes for Iceland:
chisq.test(select(subgroups_country, rep_2_yrs_0, rep_2_yrs_1), simulate.p.value = T)
chisq.test(select(subgroups_country, sum_1_yr_0, sum_1_yr_1), simulate.p.value = T)
chisq.test(select(subgroups_country, any_res_0, any_res_1), simulate.p.value = T) # p<0.05/15


#Intervention
subgroups_intervention$rep_2_yrs_1 <- as.numeric(str_extract(subgroups_intervention$`Reporting < 2 yrs`, "^[0-9]*"))
subgroups_intervention$rep_2_yrs_0 <- as.numeric(str_extract(subgroups_intervention$`Reporting < 2 yrs`, "(?<=/)\\d+(?=,)")) - subgroups_intervention$rep_2_yrs_1

subgroups_intervention$sum_1_yr_1 <- as.numeric(str_extract(subgroups_intervention$`Summary results < 1 yr`, "^[0-9]*"))
subgroups_intervention$sum_1_yr_0 <- as.numeric(str_extract(subgroups_intervention$`Summary results < 1 yr`, "(?<=/)\\d+(?=,)")) - subgroups_intervention$sum_1_yr_1

subgroups_intervention$any_res_1 <- as.numeric(str_extract(subgroups_intervention$`Any results reporting`, "^[0-9]*"))
subgroups_intervention$any_res_0 <- as.numeric(str_extract(subgroups_intervention$`Any results reporting`, "(?<=/)\\d+(?=,)")) - subgroups_intervention$any_res_1

chisq.test(select(subgroups_intervention, rep_2_yrs_0, rep_2_yrs_1))
chisq.test(select(subgroups_intervention, sum_1_yr_0, sum_1_yr_1)) # p<0.05/15
chisq.test(select(subgroups_intervention, any_res_0, any_res_1))



#Multicentric
subgroups_multicentric$rep_2_yrs_1 <- as.numeric(str_extract(subgroups_multicentric$`Reporting < 2 yrs`, "^[0-9]*"))
subgroups_multicentric$rep_2_yrs_0 <- as.numeric(str_extract(subgroups_multicentric$`Reporting < 2 yrs`, "(?<=/)\\d+(?=,)")) - subgroups_multicentric$rep_2_yrs_1

subgroups_multicentric$sum_1_yr_1 <- as.numeric(str_extract(subgroups_multicentric$`Summary results < 1 yr`, "^[0-9]*"))
subgroups_multicentric$sum_1_yr_0 <- as.numeric(str_extract(subgroups_multicentric$`Summary results < 1 yr`, "(?<=/)\\d+(?=,)")) - subgroups_multicentric$sum_1_yr_1

subgroups_multicentric$any_res_1 <- as.numeric(str_extract(subgroups_multicentric$`Any results reporting`, "^[0-9]*"))
subgroups_multicentric$any_res_0 <- as.numeric(str_extract(subgroups_multicentric$`Any results reporting`, "(?<=/)\\d+(?=,)")) - subgroups_multicentric$any_res_1

chisq.test(select(subgroups_multicentric, rep_2_yrs_0, rep_2_yrs_1))
chisq.test(select(subgroups_multicentric, sum_1_yr_0, sum_1_yr_1))
chisq.test(select(subgroups_multicentric, any_res_0, any_res_1))



#Sponsor type
subgroups_spons$rep_2_yrs_1 <- as.numeric(str_extract(subgroups_spons$`Reporting < 2 yrs`, "^[0-9]*"))
subgroups_spons$rep_2_yrs_0 <- as.numeric(str_extract(subgroups_spons$`Reporting < 2 yrs`, "(?<=/)\\d+(?=,)")) - subgroups_spons$rep_2_yrs_1

subgroups_spons$sum_1_yr_1 <- as.numeric(str_extract(subgroups_spons$`Summary results < 1 yr`, "^[0-9]*"))
subgroups_spons$sum_1_yr_0 <- as.numeric(str_extract(subgroups_spons$`Summary results < 1 yr`, "(?<=/)\\d+(?=,)")) - subgroups_spons$sum_1_yr_1

subgroups_spons$any_res_1 <- as.numeric(str_extract(subgroups_spons$`Any results reporting`, "^[0-9]*"))
subgroups_spons$any_res_0 <- as.numeric(str_extract(subgroups_spons$`Any results reporting`, "(?<=/)\\d+(?=,)")) - subgroups_spons$any_res_1

chisq.test(select(subgroups_spons, rep_2_yrs_0, rep_2_yrs_1))
chisq.test(select(subgroups_spons, sum_1_yr_0, sum_1_yr_1), simulate.p.value = T)
chisq.test(select(subgroups_spons, any_res_0, any_res_1))


#Enrollment
subgroups_enroll$rep_2_yrs_1 <- as.numeric(str_extract(subgroups_enroll$`Reporting < 2 yrs`, "^[0-9]*"))
subgroups_enroll$rep_2_yrs_0 <- as.numeric(str_extract(subgroups_enroll$`Reporting < 2 yrs`, "(?<=/)\\d+(?=,)")) - subgroups_enroll$rep_2_yrs_1

subgroups_enroll$sum_1_yr_1 <- as.numeric(str_extract(subgroups_enroll$`Summary results < 1 yr`, "^[0-9]*"))
subgroups_enroll$sum_1_yr_0 <- as.numeric(str_extract(subgroups_enroll$`Summary results < 1 yr`, "(?<=/)\\d+(?=,)")) - subgroups_enroll$sum_1_yr_1

subgroups_enroll$any_res_1 <- as.numeric(str_extract(subgroups_enroll$`Any results reporting`, "^[0-9]*"))
subgroups_enroll$any_res_0 <- as.numeric(str_extract(subgroups_enroll$`Any results reporting`, "(?<=/)\\d+(?=,)")) - subgroups_enroll$any_res_1

chisq.test(select(subgroups_enroll, rep_2_yrs_0, rep_2_yrs_1)) # p<0.05/15
chisq.test(select(subgroups_enroll, sum_1_yr_0, sum_1_yr_1))
chisq.test(select(subgroups_enroll, any_res_0, any_res_1)) # p<0.05/15


# 10. Other calculations for paper ----

## Manual search process (in console only) ----

publication_data$proj_id <- as.character(publication_data$proj_id)
publication_data <- left_join(publication_data, select(analysis, proj_id, eligibility))

publication_data <- mutate(publication_data, former_discrepancy_recoded = case_when(
  grepl("Consensus on|No publication, agreed", former_discrepancy) ~ "Consensus",
  grepl("Not consensus on earliest", former_discrepancy) ~ "Not consensus on earliest publication",
  grepl("Not consensus on whether", former_discrepancy) ~ "Not consensus on existence of publication",
  TRUE ~ NA_character_
))

#Initial discrepancies between the 2 independent reviewers
summary(as.factor(publication_data$former_discrepancy_recoded))
prop.table(table(publication_data$former_discrepancy_recoded))

#Publication types
summary(as.factor(analysis$publication_type))
prop.table(table(analysis$publication_type))
summary(as.factor(analysis$identification_step))
prop.table(table(analysis$identification_step))

#Calculate how many timely publications were via journal publication, summary results, or other
timely_pub_num <- sum(analysis$days_cd_to_publication < 2*365 & 
                        analysis$publication_type == "Journal publication" &
                        (analysis$days_cd_to_summary >= 2*365|is.na(analysis$days_cd_to_summary)),
                      na.rm = TRUE)

timely_sum_num <- sum(analysis$days_cd_to_summary < 2*365 &
                        (analysis$days_cd_to_publication >= 2*365|is.na(analysis$days_cd_to_publication)),
                      na.rm = TRUE)

timely_pub_and_sum_num <- sum(analysis$days_cd_to_summary < 2*365 & 
                                analysis$days_cd_to_publication < 2*365, na.rm = TRUE)

timely_other_num <- sum(analysis$days_cd_to_publication < 2*365 & 
                         !analysis$publication_type == "Journal publication" &
                         (analysis$days_cd_to_summary >= 2*365|is.na(analysis$days_cd_to_summary)),
                        na.rm = TRUE)
print(paste0("Publication within 24 month as journal publication only: ", timely_pub_num,
             " - as summary result only: ", timely_sum_num,
             " - as both journal publication and summary result: ", timely_pub_and_sum_num,
             " - as other publication type: ", timely_other_num))

