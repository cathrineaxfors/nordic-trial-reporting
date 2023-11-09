
# Systematic evaluation of clinical trial reporting at medical universities and university 
# hospitals in the Nordic countries 
# Cathrine Axfors
# OSF protocol: https://osf.io/uckyt


# README Script 2 ----

# Description: Data cleaning of variables from ClinicalTrials.gov and EUCTR to prepare
# the analysis dataset.

# Output: creates .rda files to be used for analysis (also saved as .csv for sharing purposes).

# Load packages ----

library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)

# Hard code preparations ----

today <- "" #Enter today's date YYYY-MM-DD (our date: 2023-11-09)


## Folder path ----
#Enter here the path to the folder where you saved the "data" and "code" folders (see our GitHub)
folder_path <- "C:/Users/catax386/Documents/Forskning/Egna studier/Postdoc METRICS/clinical trials dashboard/final-share/"


## Load data ----

#Trial sample
load(paste0(folder_path, "data/2-data-processing/", "final-trial-sample-2023-11-09.rda"))
nordic_all <- final_trial_sample

#Publication information
load(paste0(folder_path, "data/2-data-processing/", "publication-information-merged-2023-11-07.rda"))

#Full dataset from CTgov
load(paste0(folder_path, "data/1-sample-generation/output-data/", "clintrialsnord_CTgov_full-2023-11-09.rda"))
ctgov <- CTgov_sample

#EUCTR results scraper
euctr_results <- read.csv(paste0(folder_path, "data/1-sample-generation/raw-data/", "euctr_data_quality_results_scrape_nov_2022.csv"), encoding = "UTF-8") #results section dataset

#EUCTR protocol scraper
euctr_all <- read.csv(paste0(folder_path, "data/1-sample-generation/raw-data/", "euctr_euctr_dump-2022-11-05-011854.csv"), encoding = "UTF-8") #protocols dataset

#Manually verified information regarding cross-registrations and summary results
load(paste0(folder_path, "data/2-data-processing/", "crossreg-manually-verified-2023-08-09.rda"))


# Merge trial and publication data ----
publication_data$proj_id <- as.character(publication_data$proj_id)
nordic_all$proj_id <- as.character(nordic_all$proj_id)

nordic_all <- left_join(nordic_all, select(publication_data, !c(reviewer_1, reviewer_2, registry, eligibility)),
                        by = "proj_id")


# Data cleaning ----

## Eligibility variables ----

#eligibility
#Levels: Cross-registration duplicate, Eligible, Ineligible (withdrawn without enrollment), Ineligible
#completion date, Ineligible design, Ineligible end date, Ineligible institution


## Analysis variables ----

#main_id
nordic_all$main_id <- nordic_all$primary_id
#character variable


#nct_id
#character variable

#eudract_id
#character variable


#registry
#Levels: ctgov, euctr


#trial_title
nordic_all$trial_title <- nordic_all$long_title
#Character variable, 33 missing in eligible sample

#trial_name
#Character variable, most are missing in eligible sample


#lead_institution
nordic_all$lead_institution <- str_replace(nordic_all$lead_institution, "; ", ";")
#Levels: see institutions list


#lead_country
nordic_all <- mutate(nordic_all, lead_country = case_when(
  grepl("Aalborg University|Aalborg University Hospital|Aarhus University|Aarhus University Hospital|Bispebjerg and Frederiksberg Hospital|Bornholms Hospital|Copenhagen University Hospital|Herlev and Gentofte Hospital|Holb.{1,2}k Hospital|Hvidovre and Amager Hospital|Mental health services in the Capital Region of Denmark|N.{1,2}stved Hospital|Nordsj.{1,2}llands Hospital|Odense University Hospital|Steno Diabetes Center Copenhagen|University of Copenhagen|University of Southern Denmark|Zealand University Hospital", 
        lead_institution) ~ "Denmark",
  grepl("Helsinki University Hospital|Kuopio University Hospital|Oulu University Hospital|Tampere University Hospital|Turku University Hospital|University of Eastern Finland|University of Helsinki|University of Oulu|University of Tampere|University of Turku", lead_institution) ~ "Finland",
  grepl("The National University Hospital of Iceland|University of Iceland", lead_institution) ~ "Iceland",
  grepl("Akershus University Hospital|Haukeland university hospital|Norwegian University of Science and Technology|Oslo University Hospital|St. Olav.s University Hospital|Stavanger University Hospital|UiT The Arctic University of Norway|University Hospital of North Norway|University of Bergen|University of Oslo", lead_institution) ~ "Norway",
  grepl("Danderyd Hospital|Gothenburg University|Huddinge Hospital|Karolinska Institutet|Karolinska University Hospital|Linkoeping University|Linkoeping University Hospital|Lund University|Sahlgrenska University Hospital|Skane University Hospital|Stockholm South General Hospital|Ume.{1,2} University|University Hospital of Ume.{1,2}|Uppsala Academic Hospital|Uppsala University|.{1,2}rebro University|.{1,2}rebro University Hospital", lead_institution) ~ "Sweden"
))
#Levels: Denmark, Finland, Iceland, Norway, Sweden


#has_publication
#Levels: No, Yes, NA


#publication_doi
#Character variable. Missing for some publications.


#publication_pmid
nordic_all$publication_pmid <- as.numeric(nordic_all$publication_pmid)
#Numeric variable. Missing for some publications.


#publication_url
#Character variable. Present for all publications.


#publication_date
nordic_all$publication_date <- as.character(nordic_all$publication_date)
#Character variable denoting dates. Present for all publications.


#identification_step
#Character variable
#Levels: Cross-reference, Linked at the registration, Other, Systematic Google search, NA


#publication_type
#Levels: Conference abstract, Doctoral thesis, Journal publication, Letter to the editor, Preprint, NA


#has_summary_results
nordic_all <- mutate(nordic_all, has_summary_results = case_when(
  has_results == 1 ~ "Yes",
  has_results == 0 ~ "No",
  were_results_reported == T ~ "Yes",
  were_results_reported == F ~ "No"
))
#Levels: No, Yes


#summary_results_date
#euctr: first_version_date
nordic_all <- left_join(nordic_all, select(euctr_results, trial_id, first_version_date), by = c(
  "main_id" = "trial_id"))
#ctgov: results_first_submitted_date
nordic_all <- left_join(nordic_all, select(ctgov, nct_id, results_first_submitted_date), by = c(
  "main_id" = "nct_id"))

nordic_all <- mutate(nordic_all, summary_results_date = case_when(
  registry == "euctr" ~ first_version_date,
  registry == "ctgov" ~ as.character(results_first_submitted_date)
))
#Character variable denoting dates



#registration_date
#euctr: date_on_which_this_record_was_first_entered_in_the_eudract_data
euctr_all <- group_by(euctr_all, eudract_number)
euctr_all <- mutate(euctr_all, date_first_entered_in_eudract = 
                      min(date_on_which_this_record_was_first_entered_in_the_eudract_data))
euctr_all <- ungroup(euctr_all)

nordic_all <- 
  left_join(nordic_all, select(filter(euctr_all, !duplicated(eudract_number)), 
      eudract_number, date_first_entered_in_eudract), 
      by = c("main_id" = "eudract_number"))

#ctgov: study_first_submitted_date
nordic_all$study_first_submitted_date <- as.character(nordic_all$study_first_submitted_date)

nordic_all <- mutate(nordic_all, registration_date = case_when(
  registry == "euctr" ~ date_first_entered_in_eudract,
  registry == "ctgov" ~ study_first_submitted_date
))
#Character variable denoting date



#start_date

#CTgov: start_date
nordic_all$start_date_ctgov <- as.character(nordic_all$start_date)

#EUCTR: 'Actual start date of recruitment', from results scraper
nordic_all <- left_join(nordic_all, select(euctr_results, trial_id, trial_start_date),
                             by = c("main_id" = "trial_id"))

nordic_all <- mutate(nordic_all, start_date = case_when(
  registry == "ctgov" ~ start_date_ctgov,
  registry == "euctr" ~ trial_start_date
))
#Character variable denoting date



#completion_date
nordic_all$completion_date_ctgov <- as.character(nordic_all$completion_date)

nordic_all <- mutate(nordic_all, completion_date = case_when(
  registry == "ctgov" ~ completion_date_ctgov,
  registry == "euctr" ~ max_end_date
))
#Character variable denoting date



#primary_completion_date
#euctr: primary_comp_date
#ctgov: primary_completion_date

nordic_all <- left_join(nordic_all, select(euctr_results, trial_id, primary_comp_date),
                        by = c("main_id" = "trial_id"))

nordic_all <- left_join(nordic_all, select(ctgov, nct_id, primary_completion_date),
                        by = c("main_id" = "nct_id"))

nordic_all$primary_completion_date_ctgov <- nordic_all$primary_completion_date

nordic_all <- mutate(nordic_all, primary_completion_date = case_when(
  registry == "ctgov" ~ as.character(primary_completion_date_ctgov),
  registry == "euctr" ~ primary_comp_date
))

#Character variable denoting date, >100 missing, as expected



#recruitment_status
nordic_all <- mutate(nordic_all, recruitment_status = case_when(
  registry == "ctgov" ~ overall_status,
  registry == "euctr" ~ trial_status_recoded
))
#Levels, euctr: Any Completed, none Prematurely Ended; Any Prematurely Ended
#Levels, ctgov: Completed, Terminated, Unknown status



#enrollment
#ctgov: nordic_all$enrollment

nordic_all$enrollment_ctgov <- nordic_all$enrollment

#euctr: 
#1) among results scraper vars EEA_subjects and global_subjects, whichever is larger
#if there are several country protocols, choose the largest number entered
#2) among protocol vars subject_in_the_member_state, subject_in_the_eea, subject_in_the_whole_clinical_trial,
#whichever is larger
#if there are several country protocols, choose the largest number entered

euctr_all <- mutate(euctr_all, protocol_planned_enrollment = 
                      pmax(subject_in_the_member_state, subject_in_the_eea, 
                           subject_in_the_whole_clinical_trial, na.rm = T))

euctr_all <- group_by(euctr_all, eudract_number)
euctr_all <- mutate(euctr_all, protocol_planned_enrollment_trial = 
                      max(protocol_planned_enrollment, na.rm = T)) #Expected warnings for groups with all missing
euctr_all <- ungroup(euctr_all)
euctr_all <- mutate(euctr_all, protocol_planned_enrollment_trial = ifelse(
  protocol_planned_enrollment_trial < 0, NA, protocol_planned_enrollment_trial))

nordic_all <- left_join(nordic_all, select(euctr_results, trial_id, EEA_subjects, global_subjects), by = c(
  "main_id" = "trial_id"))

nordic_all <- 
  left_join(nordic_all, select(filter(euctr_all, !duplicated(eudract_number)), 
                               eudract_number, protocol_planned_enrollment_trial), 
            by = c("main_id" = "eudract_number"))

nordic_all <- mutate(nordic_all, enrollment_euctr = case_when(
  !is.na(EEA_subjects)|!is.na(global_subjects) ~ pmax(EEA_subjects, global_subjects, na.rm = T),
  !is.na(protocol_planned_enrollment_trial) ~ as.integer(protocol_planned_enrollment_trial)
))

nordic_all <- mutate(nordic_all, enrollment = case_when(
  registry == "euctr" ~ enrollment_euctr,
  registry == "ctgov" ~ as.integer(enrollment_ctgov)
))
#integer variable



#is_multicentric
#euctr: trial_the_trial_involves_multiple_sites_in_the_member_state_con, 
#trial_the_trial_involves_multiple_member_states

euctr_all <- group_by(euctr_all, eudract_number)
euctr_all <- mutate(euctr_all, trial_the_trial_involves_multiple_sites_in_the_member_state_con = 
                      paste0(unique(trial_the_trial_involves_multiple_sites_in_the_member_state_con), collapse = ";"))
euctr_all <- ungroup(euctr_all)
euctr_all$trial_the_trial_involves_multiple_sites_in_the_member_state_con <- str_replace(euctr_all$trial_the_trial_involves_multiple_sites_in_the_member_state_con, "^;", "")
euctr_all$trial_the_trial_involves_multiple_sites_in_the_member_state_con <- str_replace(euctr_all$trial_the_trial_involves_multiple_sites_in_the_member_state_con, ";$", "")

euctr_all <- group_by(euctr_all, eudract_number)
euctr_all <- mutate(euctr_all, trial_the_trial_involves_multiple_member_states = 
                      paste0(unique(trial_the_trial_involves_multiple_member_states), collapse = ";"))
euctr_all <- ungroup(euctr_all)
euctr_all$trial_the_trial_involves_multiple_member_states <- str_replace(euctr_all$trial_the_trial_involves_multiple_member_states, "^;", "")
euctr_all$trial_the_trial_involves_multiple_member_states <- str_replace(euctr_all$trial_the_trial_involves_multiple_member_states, ";$", "")

euctr_all <- mutate(euctr_all, multiple_sites = case_when(
  grepl("t", trial_the_trial_involves_multiple_sites_in_the_member_state_con) ~ "Yes",
  grepl("t", trial_the_trial_involves_multiple_member_states) ~ "Yes",
  grepl("^f$", trial_the_trial_involves_multiple_sites_in_the_member_state_con) &
          grepl("^f$", trial_the_trial_involves_multiple_member_states) ~ "No",
  TRUE ~ NA_character_
))
#If one of the 2 questions was answered "Yes", or, if there are multiple country protocols, we code as "Yes"

nordic_all <- 
  left_join(nordic_all, select(filter(euctr_all, !duplicated(eudract_number)), 
                               eudract_number, multiple_sites), 
            by = c("main_id" = "eudract_number"))

#ctgov: has_single_facility
nordic_all <- left_join(nordic_all, select(ctgov, nct_id, has_single_facility),
                        by = c("main_id" = "nct_id"))

nordic_all <- mutate(nordic_all, is_multicentric = case_when(
  registry == "euctr" ~ multiple_sites,
  registry == "ctgov" & has_single_facility == T ~ "No",
  registry == "ctgov" & has_single_facility == F ~ "Yes"
))
#Levels: No, Yes


#is_multinational
#euctr: trial_the_trial_involves_multiple_member_states
euctr_all <- mutate(euctr_all, multiple_states = case_when(
  grepl("t", trial_the_trial_involves_multiple_member_states) ~ "Yes",
  grepl("^f$", trial_the_trial_involves_multiple_member_states) ~ "No",
  TRUE ~ NA_character_
))
#If that question was answered "Yes", or, if there are multiple country protocols, we code as "Yes"

nordic_all <- 
  left_join(nordic_all, select(filter(euctr_all, !duplicated(eudract_number)), 
                               eudract_number, multiple_states), 
            by = c("main_id" = "eudract_number"))

nordic_all$is_multinational <- nordic_all$multiple_states

#Levels, euctr: No, Yes
#Missing for ctgov



#main_sponsor
#euctr: from eutt, sponsor_status. 0=non-commercial, 1=commercial, 2=mixed/unclear 3=blank
#ctgov: agency_classes, one entry per sponsor

nordic_all <- mutate(nordic_all, agency_classes_cat = case_when(
  grepl("industry", agency_classes, ignore.case = T) ~ "Industry or mixed",
  grepl("other", agency_classes, ignore.case = T) ~ "Only non-industry"
))

nordic_all <- mutate(nordic_all, main_sponsor = case_when(
  sponsor_status == 0 ~ "Only non-industry",
  sponsor_status == 1 | sponsor_status == 2 ~ "Industry or mixed"
))

nordic_all <- mutate(nordic_all, main_sponsor = case_when(
  registry == "euctr" ~ main_sponsor,
  registry == "ctgov" ~ agency_classes_cat
))
#Levels: Industry or mixed; Only non-industry



#is_controlled
#euctr: trial_controlled
summary(as.factor(euctr_all$trial_controlled))

euctr_all <- group_by(euctr_all, eudract_number)
euctr_all <- mutate(euctr_all, trial_controlled = 
                      paste0(unique(trial_controlled), collapse = ";"))
euctr_all <- ungroup(euctr_all)
#We code trials as controlled if at least 1 country protocol defined it as such

nordic_all <- 
  left_join(nordic_all, select(filter(euctr_all, !duplicated(eudract_number)), 
                               eudract_number, trial_controlled), 
            by = c("main_id" = "eudract_number"))

nordic_all <- mutate(nordic_all, trial_controlled = ifelse(grepl(";", trial_controlled), "t", trial_controlled))

#ctgov: intervention_model
nordic_all <- left_join(nordic_all, select(ctgov, nct_id, intervention_model),
                        by = c("main_id" = "nct_id"))

nordic_all <- mutate(nordic_all, is_controlled = case_when(
  registry == "euctr" & trial_controlled == "t" ~ "Yes",
  registry == "euctr" & trial_controlled == "f" ~ "No",
  registry == "ctgov" & !intervention_model == "Single Group Assignment" ~ "Yes",
  registry == "ctgov" & intervention_model == "Single Group Assignment" ~ "No"
))

#Levels: No, Yes


#is_randomised
#euctr: trial_randomised

euctr_all <- group_by(euctr_all, eudract_number)
euctr_all <- mutate(euctr_all, trial_randomised = 
                      paste0(unique(trial_randomised), collapse = ";"))
euctr_all <- ungroup(euctr_all)

nordic_all <- 
  left_join(nordic_all, select(filter(euctr_all, !duplicated(eudract_number)), 
                               eudract_number, trial_randomised), 
            by = c("main_id" = "eudract_number"))
#We code trials as randomized if at least 1 country protocol defined it as such. Only affected 1 trial

nordic_all <- mutate(nordic_all, trial_randomised = ifelse(grepl(";", trial_randomised), "t", trial_randomised))


#ctgov: allocation
nordic_all <- left_join(nordic_all, select(ctgov, nct_id, allocation),
                        by = c("main_id" = "nct_id"))
#For trials with the value "N/A" ("not applicable", not missing) in ctgov, we recode as "non-randomized"
#These were all single group assignment trials.
#There are 4 missing values on randomization from CTgov.

nordic_all <- mutate(nordic_all, is_randomised = case_when(
  registry == "euctr" & trial_randomised == "t" ~ "Yes",
  registry == "euctr" & trial_randomised == "f" ~ "No",
  registry == "ctgov" & allocation == "Randomized" ~ "Yes",
  registry == "ctgov" & !allocation == "Randomized" ~ "No"
))


#masking
#euctr: trial_open

euctr_all <- group_by(euctr_all, eudract_number)
euctr_all <- mutate(euctr_all, trial_open = 
                      paste0(unique(trial_open), collapse = ";"))
euctr_all <- ungroup(euctr_all)

nordic_all <- 
  left_join(nordic_all, select(filter(euctr_all, !duplicated(eudract_number)), 
                               eudract_number, trial_open), 
            by = c("main_id" = "eudract_number"))
#We code trials as open-label if at least 1 country protocol defined it as such. Only affected 3 trials

nordic_all <- mutate(nordic_all, trial_open = ifelse(grepl(";", trial_open), "t", trial_open))


#ctgov: masking
nordic_all <- left_join(nordic_all, select(ctgov, nct_id, masking),
                        by = c("main_id" = "nct_id"))
#There are 9 missing values on randomization from CTgov.

nordic_all <- mutate(nordic_all, masking = case_when(
  registry == "euctr" & trial_open == "f" ~ "Yes",
  registry == "euctr" & trial_open == "t" ~ "No",
  registry == "ctgov" & masking == "None (Open Label)" ~ "No",
  registry == "ctgov" & !masking == "None (Open Label)" ~ "Yes"
))


#intervention_type
#euctr: all are categorized as medicinal product trials per definition
#ctgov: intervention_types_combined
nordic_all <- mutate(nordic_all, intervention_type = case_when(
  registry == "euctr" ~ "Medicinal product (EUCTR)",
  grepl("biological|combination|drug|genetic", 
        intervention_types_combined, ignore.case = T) ~ "Medicinal product",
  !is.na(intervention_types_combined) ~ "Not medicinal product",
))


#phase
#euctr: phase, Levels: 0 (discordant), 1-4
#ctgov: phase, Levels: "Not Applicable", "Early Phase 1", "Phase 4", "Phase 1"        
# "Phase 3", "Phase 2", "Phase 2/Phase 3", "Phase 1/Phase 2"
#1636/2456 "Not Applicable"


#center_size
#threshold defined as median of the total number of registered trials per institution
eligible <- filter(nordic_all, eligibility == "Eligible")
institutions_trial_num <- table(unlist(str_split(eligible$lead_institution, ";")))
median_trial_num <- institutions_trial_num %>% median()
large_centers <- which(institutions_trial_num > median_trial_num) %>% names()

nordic_all <- nordic_all %>%
  mutate(center_size = lead_institution %>% purrr::map_chr(function(x) 
    ifelse(x %>% str_detect(large_centers) %>% any(), "large", "small")))
nordic_all <- mutate(nordic_all, center_size = ifelse(!eligibility == "Eligible", NA, center_size))
#Levels: Large, Small


#single_sponsor
nordic_all <- mutate(nordic_all, single_sponsor = str_replace(lead_institution, ";.*$", ""))



# Enter manually verified variables ----

# Manually imputed information from cross-registrations
#Note. For 8 records, manually verified information is handled separately from the rest:
#proj_id 2474, 2514, 2522, 2549, 2571, 2577, 2582, 2586.


crossreg_manually_verified$publication_pmid <- as.numeric(crossreg_manually_verified$publication_pmid)

nordic_all <- left_join(nordic_all, select(crossreg_manually_verified, -c(primary_id, 
                         lead_institution, nct_id, eudract_id, eligibility_final,
                         crossreg_final, crossreg_manual_extractor, is_multicentric,
                         main_sponsor, completion_year, primary_completion_year, 
                         is_prospective)), by = "proj_id")

nordic_all$enrollment.y <- as.integer(nordic_all$enrollment.y)

nordic_all <- mutate(nordic_all,
    phase = if_else((grepl("identified", crossregistration_check, ignore.case = TRUE) & !proj_id=="2474"), phase.y, phase.x),
    start_date = if_else((grepl("identified", crossregistration_check, ignore.case = TRUE) & !proj_id=="2474"), start_date.y, start_date.x),
    completion_date = if_else((grepl("identified", crossregistration_check, ignore.case = TRUE) & !proj_id=="2474"), completion_date.y, completion_date.x),
    enrollment = if_else((grepl("identified", crossregistration_check, ignore.case = TRUE) & !proj_id=="2474"), enrollment.y, enrollment.x),
    has_publication = if_else((grepl("identified", crossregistration_check, ignore.case = TRUE) & !proj_id=="2474"), has_publication.y, has_publication.x),
    publication_doi = if_else((grepl("identified", crossregistration_check, ignore.case = TRUE) & !proj_id=="2474"), publication_doi.y, publication_doi.x),
    publication_pmid = if_else((grepl("identified", crossregistration_check, ignore.case = TRUE) & !proj_id=="2474"), publication_pmid.y, publication_pmid.x),
    publication_url = if_else((grepl("identified", crossregistration_check, ignore.case = TRUE) & !proj_id=="2474"), publication_url.y, publication_url.x),
    publication_date = if_else((grepl("identified", crossregistration_check, ignore.case = TRUE) & !proj_id=="2474"), publication_date.y, publication_date.x),
    identification_step = if_else((grepl("identified", crossregistration_check, ignore.case = TRUE) & !proj_id=="2474"), identification_step.y, identification_step.x),
    publication_type = if_else((grepl("identified", crossregistration_check, ignore.case = TRUE) & !proj_id=="2474"), publication_type.y, publication_type.x),
    has_summary_results = if_else((grepl("identified", crossregistration_check, ignore.case = TRUE) & !proj_id=="2474"), has_summary_results.y, has_summary_results.x),
    summary_results_date = if_else((grepl("identified", crossregistration_check, ignore.case = TRUE) & !proj_id=="2474"), summary_results_date.y, summary_results_date.x),
    registration_date = if_else((grepl("identified", crossregistration_check, ignore.case = TRUE) & !proj_id=="2474"), registration_date.y, registration_date.x),
    primary_completion_date = if_else((grepl("identified", crossregistration_check, ignore.case = TRUE) & !proj_id=="2474"), primary_completion_date.y, primary_completion_date.x),
    recruitment_status = if_else((grepl("identified", crossregistration_check, ignore.case = TRUE) & !proj_id=="2474"), recruitment_status.y, recruitment_status.x),
    is_multinational = if_else((grepl("identified", crossregistration_check, ignore.case = TRUE) & !proj_id=="2474"), is_multinational.y, is_multinational.x),
    is_controlled = if_else((grepl("identified", crossregistration_check, ignore.case = TRUE) & !proj_id=="2474"), is_controlled.y, is_controlled.x),
    is_randomised = if_else((grepl("identified", crossregistration_check, ignore.case = TRUE) & !proj_id=="2474"), is_randomised.y, is_randomised.x),
    masking = if_else((grepl("identified", crossregistration_check, ignore.case = TRUE) & !proj_id=="2474"), masking.y, masking.x),
    center_size = if_else((grepl("identified", crossregistration_check, ignore.case = TRUE) & !proj_id=="2474"), center_size.y, center_size.x)
  )

# Manually verified presence of summary results and trial dates (start, completion) from that section

nordic_all <- mutate(nordic_all,
    start_date = if_else((has_summary_results.x == "Yes" & !grepl("2474|2514|2522|2549|2571|2577|2582|2586", proj_id)), start_date.y, start_date.x),
    completion_date = if_else((has_summary_results.x == "Yes" & !grepl("2474|2514|2522|2549|2571|2577|2582|2586", proj_id)), completion_date.y, completion_date.x),
    has_summary_results = if_else((has_summary_results.x == "Yes" & !grepl("2474|2514|2522|2549|2571|2577|2582|2586", proj_id)), has_summary_results.y, has_summary_results.x),
    summary_results_date = if_else((has_summary_results.x == "Yes" & !grepl("2474|2514|2522|2549|2571|2577|2582|2586", proj_id)), summary_results_date.y, summary_results_date.x),
    primary_completion_date = if_else((has_summary_results.x == "Yes" & !grepl("2474|2514|2522|2549|2571|2577|2582|2586", proj_id)), primary_completion_date.y, primary_completion_date.x)
)


nordic_all <- select(nordic_all, !matches("\\.x|\\.y"))

# Separate imputation of manually verified variables for 1 record

nordic_all <- mutate(nordic_all, 
    has_summary_results = ifelse(proj_id == "2474", "No", has_summary_results),
    summary_results_date = ifelse(proj_id == "2474", NA, summary_results_date))


# Calculate time-to variables ----


#days_cd_to_publication
nordic_all <- mutate(nordic_all, days_cd_to_publication = 
                       as.integer(ymd(publication_date)-ymd(completion_date)))
summary(nordic_all$days_cd_to_publication)
#see <- filter(nordic_all, eligibility == "Eligible")
#see <- filter(see, days_cd_to_publication<0) #184 publications before completion date



#days_pcd_to_publication
nordic_all <- mutate(nordic_all, days_pcd_to_publication = 
                       as.integer(ymd(publication_date)-ymd(primary_completion_date)))
summary(nordic_all$days_pcd_to_publication)


#days_cd_to_summary
nordic_all <- mutate(nordic_all, days_cd_to_summary = 
                       as.integer(ymd(summary_results_date)-ymd(completion_date)))
summary(nordic_all$days_cd_to_summary)


#days_pcd_to_summary
nordic_all <- mutate(nordic_all, days_pcd_to_summary = 
                       as.integer(ymd(summary_results_date)-ymd(primary_completion_date)))
summary(nordic_all$days_pcd_to_summary)


#days_reg_to_start
nordic_all <- mutate(nordic_all, days_reg_to_start = 
                       as.integer(ymd(start_date)-ymd(registration_date)))
summary(nordic_all$days_reg_to_start)


#days_reg_to_cd
nordic_all <- mutate(nordic_all, days_reg_to_cd = 
                       as.integer(ymd(completion_date)-ymd(registration_date)))
summary(nordic_all$days_reg_to_cd)


#days_reg_to_pcd
nordic_all <- mutate(nordic_all, days_reg_to_pcd = 
                       as.integer(ymd(primary_completion_date)-ymd(registration_date)))
summary(nordic_all$days_reg_to_pcd)


#days_reg_to_publication
nordic_all <- mutate(nordic_all, days_reg_to_publication = 
                       as.integer(ymd(publication_date)-ymd(registration_date)))
summary(nordic_all$days_reg_to_publication)


#completion_year
nordic_all$completion_year <- as.character(year(as_date(nordic_all$completion_date)))
#Character variable denoting year, 20 missing


#primary_completion_year
nordic_all$primary_completion_year <- as.character(year(as_date(nordic_all$primary_completion_date)))
#Character variable denoting year


#is_prospective
#Whether trial was prospectively registered. Derived from `registration_date` and `start_date`. Trial is considered prospectively registered if registered in the same month or previous month to start date.
nordic_all <- mutate(nordic_all, is_prospective = case_when(
  (floor_date(as_date(registration_date), unit = "month") <=
     floor_date(as_date(start_date), unit = "month")) == T ~ "Yes",
  (floor_date(as_date(registration_date), unit = "month") <=
     floor_date(as_date(start_date), unit = "month")) == F ~ "No"
))
#Levels: No, Yes


# Save analysis dataset ---
colnames(nordic_all)

vars <- c("main_id", "proj_id", "eligibility", "eudract_id", "nct_id", "crossregistration_check", "registry", "trial_title", "trial_name", "lead_institution", "lead_country", "has_publication", "publication_doi", "publication_pmid", "publication_url", "publication_date", "identification_step", "publication_type", "is_prospective", "has_summary_results", "summary_results_date", "registration_date", "start_date", "completion_date", "completion_year", "primary_completion_date", "primary_completion_year", "days_cd_to_publication", "days_pcd_to_publication", "days_cd_to_summary", "days_pcd_to_summary", "days_reg_to_start", "days_reg_to_cd", "days_reg_to_pcd", "days_reg_to_publication", "recruitment_status", "phase", "enrollment", "is_multicentric", "is_multinational", "main_sponsor", "is_controlled", "is_randomised", "masking", "intervention_type", "center_size", "single_sponsor")

analysis <- select(nordic_all, any_of(vars))
analysis <- filter(analysis, eligibility == "Eligible")

save(analysis, file = paste0(folder_path, "data/2-data-processing/output-data/", "analysis-", today, ".rda"))
write.csv(analysis, file = paste0(folder_path, "data/2-data-processing/output-data/", "analysis-", today, ".csv"))
writexl::write_xlsx(analysis, paste0(folder_path, "data/2-data-processing/output-data/", "analysis-", today, ".xlsx"))
