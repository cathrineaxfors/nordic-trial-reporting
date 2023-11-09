
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
library(KMsurv) #for Kaplan-Meier curve
library(ggplot2)
library(RColorBrewer)
#libraries purrr, tibble, scales, readxl, writexl, readr are also used


# Hard code preparations ----

today <- "" #Enter today's date YYYY-MM-DD (our date: 2023-11-09)


## Folder path ----
#Enter here the path to the folder where you saved the "data" and "code" folders (see our GitHub)
folder_path <- "C:/Users/catax386/Documents/Forskning/Egna studier/Postdoc METRICS/clinical trials dashboard/final-share/"


## Load data ----

#Analysis set
load(paste0(folder_path, "data/2-data-processing/output-data/", "analysis-2023-11-09.rda"))

#Trial sample
load(paste0(folder_path, "data/2-data-processing/", "final-trial-sample-2023-11-09.rda"))

#Publication data
load(paste0(folder_path, "data/2-data-processing/", "publication-information-merged-2023-11-07.rda"))




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

demographics <- function(categories, trial_num)
{
  demographics_tab <- cbind(categories, 100*round(categories/trial_num, 3))
  colnames(demographics_tab) <- c("Trials", "Percentage")
  return(demographics_tab)
}


demographics_table <- list()
trial_num <- dim(analysis)[1]


#Number of included trials
demographics_table[["Total"]] <- demographics(trial_num, trial_num)

#Registry
registered_euctr <- table(!is.na(analysis$eudract_id))
demographics_table[["Registered in EUCTR"]] <- demographics(registered_euctr, trial_num)

registered_ctgov <- table(!is.na(analysis$nct_id))
demographics_table[["Registered at ClinicalTrials.gov"]] <- demographics(registered_ctgov, trial_num)


#Type of intervention
analysis$intervention_type <- recode(analysis$intervention_type, "Medicinal product (EUCTR)" = "Medicinal product")
interventions <- table(analysis$intervention_type, useNA = "ifany")
demographics_table[["Intervention"]] <- demographics(interventions, trial_num)


#RCTs 
rct = table(analysis$is_randomised == "Yes" & analysis$is_controlled == "Yes")
demographics_table[["Randomized controlled trial"]] <- demographics(rct, trial_num)


#Masking
open_label = table(analysis$masking=="No")
demographics_table[["Open label"]] <- demographics(open_label, trial_num)


#Lead sponsor
lead_sponsor <- table(analysis$main_sponsor, useNA = "ifany")
demographics_table[["lead_sponsor"]] <- demographics(lead_sponsor, trial_num)


#study phase
#different ways of writing and defining the phases for CTgov and DRKS have to be combined
phase <- c("I" = sum(analysis$phase %in% c("Early Phase 1", "1", "Phase 1")),
           "I-II" = sum(analysis$phase %in% c("Phase 1/Phase 2")),
           "II" = sum(analysis$phase %in% c("2", "Phase 2")),
           "II-III" = sum(analysis$phase %in% c("Phase 2/Phase 3")),
           "III" = sum(analysis$phase %in% c("3", "Phase 3")),
           "IV" = sum(analysis$phase %in% c("4", "Phase 4")),
           "Not given" = sum(analysis$phase %in% c("0", "Not Applicable")))

demographics_table[["phase"]] <- demographics(phase, trial_num)


#mono-/multicentric
mono_multicentric <- c("Multicentric" = sum(analysis$is_multicentric == "Yes"),
                       "Monocentric" = sum(analysis$is_multicentric == "No"))
demographics_table[["Mono_Multicentric"]] <- demographics(mono_multicentric, trial_num)


#number of participants per study
sample_size <- c("1 - 100" = sum(analysis$enrollment > 0 & analysis$enrollment <= 100, na.rm = TRUE),
                 "100 - 500" = sum(analysis$enrollment > 100 & analysis$enrollment <= 500, na.rm = TRUE),
                 "> 500" = sum(analysis$enrollment > 500, na.rm = TRUE),
                 "Not given" = sum(is.na(analysis$enrollment)))

demographics_table[["Sample_size"]] <- demographics(sample_size, trial_num)


#time of registration
#prospective registration is counted if the study was registered in the month of the study start or earlier
analysis$start_date <- as_date(analysis$start_date)

analysis <- mutate_at(analysis, vars(start_date, registration_date, completion_date), ~as_date(.))

time_to_registration <- c("prospective registration" = 
                            sum((analysis$start_date %>% floor_date(unit = "month")) >= 
                                  (analysis$registration_date %>% floor_date(unit = "month")), na.rm = TRUE),
                          "after trial start" = 
                            sum((analysis$start_date %>% floor_date(unit = "month")) <
                                  (analysis$registration_date %>% floor_date(unit = "month")), na.rm = TRUE),
                          "after trial completion" = sum(analysis$days_reg_to_cd < 0, na.rm = TRUE),
                          "after publication" = sum(analysis$days_reg_to_publication < 0, na.rm = TRUE),
                          "start date not given" = sum(is.na(analysis$start_date)))

demographics_table[["time_to_registration"]] <- demographics(time_to_registration, trial_num)


#trial end (CD)
trial_end_CD <- table(year(as_date(analysis$completion_date)))

demographics_table[["trial_end_CD"]] <- demographics(trial_end_CD, trial_num)


#recruitment status
recruitment_status <- c("Completed" = sum(analysis$recruitment_status %in% c("Completed", "Any Completed, none Prematurely Ended"), na.rm = TRUE),
                        "Terminated" = sum(analysis$recruitment_status %in% c("Terminated", "Any Prematurely Ended"), na.rm = TRUE),
                        "Unknown status" = sum(analysis$recruitment_status == "Unknown status", na.rm = TRUE))

demographics_table[["recruitment_status"]] <- demographics(recruitment_status, trial_num)

#country
countries <- table(analysis$lead_country, useNA = "ifany")
demographics_table[["lead_country"]] <- demographics(countries, trial_num)

#print table

print(demographics_table)

demo_table_save <- do.call(rbind, demographics_table)
demo_table_save <- tibble(category = rownames(demo_table_save),
                          Trials = demo_table_save[,1],
                          Percentage = demo_table_save[,2] %>% round(2)) 


## Save table ----
writexl::write_xlsx(demo_table_save, paste0(folder_path, "data/3-analysis/output-results/tables/", "demographics-table-", today, ".xlsx"))
readr::write_csv(demo_table_save, paste0(folder_path, "data/3-analysis/output-results/tables/", "demographics-table-", today, ".csv"))



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
writexl::write_xlsx(institution_statistics_lead_2y, paste0(folder_path, "data/3-analysis/output-results/tables/", "institution-stats-2years-", today, ".xlsx"))
readr::write_csv(institution_statistics_lead_2y, paste0(folder_path, "data/3-analysis/output-results/tables/", "institution-stats-2years-", today, ".csv"))


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

pdf(paste0(folder_path, "data/3-analysis/output-results/figures/", "figure-inst-2y-", today, ".pdf"), height = 7, width = 7)
plot(fig)
dev.off()



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
writexl::write_xlsx(institution_statistics_summary_1y, paste0(folder_path, "data/3-analysis/output-results/tables/", "institution-summary-1year-", today, ".xlsx"))
readr::write_csv(institution_statistics_summary_1y, paste0(folder_path, "data/3-analysis/output-results/tables/", "institution-summary-1year-", today, ".csv"))


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

pdf(paste0(folder_path, "data/3-analysis/output-results/figures/", "figure-inst-summary-1yr-", today, ".pdf"), height = 7, width = 7)
plot(fig)
dev.off()


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
writexl::write_xlsx(institution_statistics_any_results, paste0(folder_path, "data/3-analysis/output-results/tables/", "institution-stats-anyreport-", today, ".xlsx"))
readr::write_csv(institution_statistics_any_results, paste0(folder_path, "data/3-analysis/output-results/tables/", "institution-stats-anyreport-", today, ".csv"))


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

pdf(paste0(folder_path, "data/3-analysis/output-results/figures/", "figure-inst-anyresults-", today, ".pdf"), height = 7, width = 7)
plot(fig)
dev.off()




# 6. Time to reported results ----


## Kaplan-Meier plot ----

#get minimum of days to pub or to summary result & time in days that one study could be tracked
analysis_KM_data <- analysis_dataset_institutions %>%
  mutate(days_obs = as.numeric(dmy("23.03.2023") - completion_date))

#sort the studies according to completion years for Kaplan-Meier curve
compl_years <- analysis_KM_data$completion_date %>% str_sub(1, 4)
subsets_KM_plot <- list("Total" = rep(TRUE, dim(analysis_KM_data)[1]) ,
                        "2016" = compl_years == "2016",
                        "2017" = compl_years == "2017",
                        "2018" = compl_years == "2018",
                        "2019" = compl_years == "2019")

#define the year range and create corresponding day vector
year_range <- 1:6
days <- 1:(365*last(year_range))


#function that summarizes the publication and censoring (= publications could not be tracked longer) events
#for one subgroup and uses the lifetab function from the KMsurv package to calculate the KM curve
get_KM_curve <- function(subset, days, days_to_publ, days_to_cens)
{
  #only take certain subset of data, e.g. different completion years
  days_to_publ <- days_to_publ[subset]
  days_to_cens <- days_to_cens[subset]
  
  #for the publications before study end set time to publ to 1 day
  days_to_publ[which(days_to_publ <= 0)] <- 1
  #if the publication takes longer than the time interval we look at,
  #set it to no publ
  days_to_publ[days_to_publ > last(days)] <- NA
  
  #if the census time is longer than the total time interval we look at,
  #set the last day of the time interval as cens day
  days_to_cens[days_to_cens > last(days)] <- last(days)
  
  #is study either censored (= no publ found) or published first
  event_or_cens <- ifelse(!is.na(days_to_publ), 1, 0)
  
  #count the publication events for each day
  pub_counts <- rep(0, length(days))
  interval_counts <- days_to_publ[days_to_publ %in% days]
  interval_counts <- table(interval_counts)
  pub_counts[as.integer(names(interval_counts))] <- interval_counts
  
  #count the censoring events for each day
  cens_counts <- rep(0, length(days))
  cens_interval_counts <- days_to_cens[days_to_cens %in% days & is.na(days_to_publ)]
  cens_interval_counts <- table(cens_interval_counts)
  cens_counts[as.integer(names(cens_interval_counts))] <- cens_interval_counts
  
  #use the lifetab function from the KMsurv package to calculate the Kaplan-Meier curve
  KL_curve <- lifetab(c(0,days), length(days_to_publ), cens_counts, pub_counts)
  
  return(KL_curve$surv)
}

#calculate cumulative distributions for subsets and make data tidy for plotting with ggplot2
cum_dist_years <- sapply(subsets_KM_plot, get_KM_curve, days, analysis_KM_data$days_to_publ, analysis_KM_data$days_obs) %>%
  as_tibble() %>%
  tibble::add_column(days) %>%
  mutate(months = time_length(days(days), unit="months")) %>%
  tail(-1) #delete first datapoint as percentage starts at 1 (which we don't want, as some publ. can be found before day 1)

#make data tidy for plotting with ggplot2
cum_dist_years <- cum_dist_years %>%
  gather(colnames(cum_dist_years) %>% head(-2) , key = "category", value = "fraction")


#Note: The above code (from the IntoValue project) stratifies by completion year. In this
#project, we instead only look at the total:
cum_dist_years_total <- filter(cum_dist_years, category == "Total")

### Figure: Kaplan-Meier curve ----

ggplot(cum_dist_years_total, aes(x = months, y = fraction, color = category)) +
  geom_step(size = 1) + #, color = "#C02942") +
  theme_minimal() +
  xlab("Months") + ylab("Unpublished studies (%)") +
  scale_color_brewer(name = "Completion\nYear" , palette = 'Dark2') +
  theme(text = element_text(size=18),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18)) +
  scale_x_continuous(name = "Months", breaks=c(0, year_range)*12,
                     labels = paste0(c(0, year_range)*12, "")) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,1)) +
  theme(axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"),
        legend.position = "none")

### Save figure ----

ggsave(paste0(folder_path, "data/3-analysis/output-results/figures/", "kaplan-meier-", today, ".pdf"), width = 18, height = 15, units = "cm", dpi = 300)



## Median time to reporting ----
#Per institution, country, total
#summary(analysis_dataset_institutions$days_to_publ)

#for given institution calculate median time to results reporting and iqr

get_time_reporting <- function(institution, days_to_publ)
{
  institution_trials <- filter(analysis_dataset_institutions,
    (lead_institution == institution |
       str_detect(lead_institution, paste0(";",institution, "$")) |
       str_detect(lead_institution, paste0("^", institution, ";"))))
  institution_tot <- sum(!is.na(institution_trials$days_to_publ))
  institution_median <- round(median(institution_trials$days_to_publ, na.rm = T), digits=0)
  institution_iqr <- round(IQR(institution_trials$days_to_publ, na.rm = T), digits=0)

  institution_stat <- tibble("institution" = institution, "trials" = institution_tot,
                             "median" = institution_median,
                             "iqr" = institution_iqr)
  return(institution_stat)
}

## Main table for median reporting times ----
institution_median_reporting <- purrr::map(institutions, get_time_reporting,
                                                days_to_publ = analysis_dataset_institutions$days_to_publ)
institution_median_reporting <- do.call(rbind, institution_median_reporting)

print(institution_median_reporting, n = Inf)

### Add country totals ----

country_names <- c("Denmark", "Finland", "Iceland", "Norway", "Sweden")

get_country_time_reporting <- function(country_names, days_to_publ)
{
  country_trials <- filter(analysis_dataset_institutions, lead_country == country_names)
  country_tot <- sum(!is.na(country_trials$days_to_publ))
  country_median <- round(median(country_trials$days_to_publ, na.rm = T), digits=0)
  country_iqr <- round(IQR(country_trials$days_to_publ, na.rm = T), digits=0)
  
  country_stat <- tibble("institution" = country_names, "trials" = country_tot,
                             "median" = country_median,
                             "iqr" = country_iqr)
  return(country_stat)
}

country_median_reporting <- purrr::map(country_names, get_country_time_reporting,
                                       days_to_publ = analysis_dataset_institutions$days_to_publ)
country_median_reporting <- do.call(rbind, country_median_reporting)

print(country_median_reporting, n = Inf)

institution_median_reporting <- rbind(institution_median_reporting, country_median_reporting)

### Add total ----
add1 <- sum(!is.na(analysis_dataset_institutions$days_to_publ))
add2 <- round(median(analysis_dataset_institutions$days_to_publ, na.rm = T), digits=0)
add3 <- round(IQR(analysis_dataset_institutions$days_to_publ, na.rm = T), digits=0)

institution_median_reporting[nrow(institution_median_reporting) + 1,] <- list("Total", add1, add2, add3)
rm(add1, add2, add3)

## Save table ----
writexl::write_xlsx(institution_median_reporting, paste0(folder_path, "data/3-analysis/output-results/tables/", "institution-median-rep-", today, ".xlsx"))
readr::write_csv(institution_median_reporting, paste0(folder_path, "data/3-analysis/output-results/tables/", "institution-median-rep-", today, ".csv"))


## Additional calculations (console only) ----

# Number of trials with any results reporting before completion date
sum(analysis_dataset_institutions$days_to_publ<0, na.rm = T)
sum(analysis_dataset_institutions$days_to_publ<0, na.rm = T)/sum(!is.na(analysis_dataset_institutions$days_to_publ), na.rm=T)

# Time to reporting, publications only
sum(!is.na(analysis_dataset_institutions$days_cd_to_publication))
round(median(analysis_dataset_institutions$days_cd_to_publication, na.rm = T), digits=0)
round(IQR(analysis_dataset_institutions$days_cd_to_publication, na.rm = T), digits=0)

# Time to reporting, summary results only
sum(!is.na(analysis_dataset_institutions$days_cd_to_summary))
round(median(analysis_dataset_institutions$days_cd_to_summary, na.rm = T), digits=0)
round(IQR(analysis_dataset_institutions$days_cd_to_summary, na.rm = T), digits=0)



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
     floor_date(start_date, unit = "month") >= floor_date(registration_date, unit = "month"), 
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

## Main table for prospective registration ----
institution_statistics_prospective <- purrr::map(institutions, get_prospective_statistics,
                                                institution_assignments = analysis_dataset_institutions$lead_institution,
                                                start_date = analysis_dataset_institutions$start_date,
                                                registration_date = analysis_dataset_institutions$registration_date)
institution_statistics_prospective <- do.call(rbind, institution_statistics_prospective)

print(institution_statistics_prospective, n = Inf)
institution_statistics_prospective$percentage <- institution_statistics_prospective$percentage %>% round(2)

### Add total ----
add1 <- nrow(analysis_dataset_institutions)
add2 <- sum(floor_date(analysis_dataset_institutions$start_date, unit = "month") >= 
              floor_date(analysis_dataset_institutions$registration_date, unit = "month"),
            na.rm = T)
add3 <- round(add2/add1, 3) * 100
add_wilson <- DescTools::BinomCI(add2, add1, conf.level = 0.95, method = c("wilson", "modified wilson"))
add_wilson_ll <- round(add_wilson[3], 3) * 100
add_wilson_ul <- round(add_wilson[5], 3) * 100

institution_statistics_prospective[nrow(institution_statistics_prospective) + 1,] <- list("Total", add1, add2, add3, add_wilson_ll, add_wilson_ul)
rm(add1, add2, add3, add_wilson_ll, add_wilson_ul)

## Save table ----
writexl::write_xlsx(institution_statistics_prospective, paste0(folder_path, "data/3-analysis/output-results/tables/", "institution-prospective-", today, ".xlsx"))
readr::write_csv(institution_statistics_prospective, paste0(folder_path, "data/3-analysis/output-results/tables/", "institution-prospective-", today, ".csv"))


## Figure ----

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

### Save figure ----

pdf(paste0(folder_path, "data/3-analysis/output-results/figures/", "figure-inst-prospective-", today, ".pdf"), height = 7, width = 7)
plot(fig)
dev.off()



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
sens_ctgov <- filter(analysis_dataset_institutions, !is.na(nct_id))

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

### Combine rows to matrix ----

sens_matrix <- rbind(sens_main, sens_rcts, sens_euctr, sens_ctgov, sens_known, sens_completed)
sens_matrix <- relocate(sens_matrix, analysis)

### Save sensitivity analysis matrix table ----

writexl::write_xlsx(sens_matrix, paste0(folder_path, "data/3-analysis/output-results/tables/", "sens-matrix-", today, ".xlsx"))
readr::write_csv(sens_matrix, paste0(folder_path, "data/3-analysis/output-results/tables/", "sens-matrix-", today, ".csv"))



## Count trials per institution only if single-sponsored or institution listed first among sponsors ----

### Results < 2 years ----
sens_institution_statistics_lead_2y <- purrr::map(institutions, get_institution_statistics,
                                             institution_assignments = analysis_dataset_institutions$single_sponsor,
                                             days_to_publ = analysis_dataset_institutions$days_to_publ)
sens_institution_statistics_lead_2y <- do.call(rbind, sens_institution_statistics_lead_2y)

print(sens_institution_statistics_lead_2y, n = Inf)
sens_institution_statistics_lead_2y$percentage <- sens_institution_statistics_lead_2y$percentage %>% round(2)


#### Save table ----
writexl::write_xlsx(sens_institution_statistics_lead_2y, paste0(folder_path, "data/3-analysis/output-results/tables/", "sens-single-sponsor-institution-stats-2years-", today, ".xlsx"))
readr::write_csv(sens_institution_statistics_lead_2y, paste0(folder_path, "data/3-analysis/output-results/tables/", "sens-single-sponsor-institution-stats-2years-", today, ".csv"))




### Summary results < 1 year ----
sens_institution_statistics_summary_1y <- purrr::map(institutions, get_summary_results_statistics,
                                                institution_assignments = analysis_dataset_institutions$single_sponsor,
                                                days_to_publ = analysis_dataset_institutions$days_cd_to_summary)
sens_institution_statistics_summary_1y <- do.call(rbind, sens_institution_statistics_summary_1y)

print(sens_institution_statistics_summary_1y, n = Inf)
sens_institution_statistics_summary_1y$percentage <- sens_institution_statistics_summary_1y$percentage %>% round(2)

#### Save table ----
writexl::write_xlsx(sens_institution_statistics_summary_1y, paste0(folder_path, "data/3-analysis/output-results/tables/", "sens-single-sponsor-institution-summary-1year-", today, ".xlsx"))
readr::write_csv(sens_institution_statistics_summary_1y, paste0(folder_path, "data/3-analysis/output-results/tables/", "sens-single-sponsor-institution-summary-1year-", today, ".csv"))


### Any results reporting ----
sens_institution_statistics_any_results <- purrr::map(institutions, get_missing_report_statistics,
                                                 institution_assignments = analysis_dataset_institutions$single_sponsor,
                                                 has_publ_or_summary = analysis_dataset_institutions$has_publ_or_summary)
sens_institution_statistics_any_results <- do.call(rbind, sens_institution_statistics_any_results)

print(sens_institution_statistics_any_results, n = Inf)
sens_institution_statistics_any_results$percentage <- sens_institution_statistics_any_results$percentage %>% round(2)


#### Save table ----
writexl::write_xlsx(sens_institution_statistics_any_results, paste0(folder_path, "data/3-analysis/output-results/tables/", "sens-single-sponsor-institution-stats-anyreport-", today, ".xlsx"))
readr::write_csv(sens_institution_statistics_any_results, paste0(folder_path, "data/3-analysis/output-results/tables/", "sens-single-sponsor-institution-stats-anyreport-", today, ".csv"))



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

pdf(paste0(folder_path, "data/3-analysis/output-results/figures/", "country-rep-2-yrs-", today, ".pdf"), height = 4, width = 4)
plot(fig)
dev.off()

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

pdf(paste0(folder_path, "data/3-analysis/output-results/figures/", "country-sum-1-yr-", today, ".pdf"), height = 4, width = 4)
plot(fig)
dev.off()

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

pdf(paste0(folder_path, "data/3-analysis/output-results/figures/", "country-any-results-", today, ".pdf"), height = 4, width = 4)
plot(fig)
dev.off()

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

writexl::write_xlsx(subgroup_matrix, paste0(folder_path, "data/3-analysis/output-results/tables/", "subgroup-matrix-", today, ".xlsx"))
readr::write_csv(subgroup_matrix, paste0(folder_path, "data/3-analysis/output-results/tables/", "subgroup-matrix-", today, ".csv"))




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

