
# Systematic evaluation of clinical trial reporting at medical universities and university 
# hospitals in the Nordic countries 
# Cathrine Axfors
# OSF protocol: https://osf.io/uckyt


# README Script 1b ----

# Description: Creates the sample of trials from EUCTR and combines with ClinicalTrials.gov
# sample generated by script 1. Also performs automatic cross-registration identification
# based on registry information (additional trial IDs mentioned in registrations).
# Trials are selected based on institution, completion year, and status.
# Also adds EUCTR variables to the dataset for later descriptive purposes.

# We use the trial cohort from the EU Trials Tracker: https://eu.trialstracker.net/
# downloaded from https://github.com/ebmdatalab/euctr-tracker-data ("trials.csv")
# as well as EUCTR web scrapers for protocols and results sections shared
# by the EU Trials Tracker team on request.

# OBS! To replicate the Nordic trials project, you must use the same folder structure as
# shown here on our GitHub. Download the EUCTR data folders from Zenodo 
# (https://zenodo.org/records/10091147) and save in the 'raw data' folder. Unzip the files.

# Output: creates xlsx file of trials to be manually verified for institution eligibility.


# Load packages ----

library(dplyr)
library(stringr)
library(lubridate)


# Hard code preparations ----

#Note: The script "1a-sample-ctgov.R" should be run before this script.

today <- "" #Enter today's date YYYY-MM-DD (our date: 2023-11-09)

## Folder path ----
#Enter here the path to the folder where you saved the "data" and "code" folders (see our GitHub)
folder_path <- "C:/Users/catax386/Documents/Forskning/Egna studier/Postdoc METRICS/clinical trials dashboard/final-share/"

## Read in files ----

euctr_all <- read.csv(paste0(folder_path, "data/1-sample-generation/raw-data/", "euctr_euctr_dump-2022-11-05-011854.csv"), encoding = "UTF-8") #protocols dataset
euctr_results <- read.csv(paste0(folder_path, "data/1-sample-generation/raw-data/", "euctr_data_quality_results_scrape_nov_2022.csv"), encoding = "UTF-8") #results section dataset
euctr_spons <- read.csv(paste0(folder_path, "data/1-sample-generation/raw-data/", "nov2022_spon_info.csv"), encoding = "UTF-8") #sponsor dataset
eutt <- read.csv(paste0(folder_path, "data/1-sample-generation/raw-data/", "trials-2022-11-07.csv"), encoding = "UTF-8") #EUTT trials dataset, data from 2022-11-07

#Run script 1a-sample-ctgov.R to generate CTgov_sample_save
load(paste0(folder_path, "data/1-sample-generation/output-data/", "/clintrialsnord_CTgov_sample-2023-11-09.rda"))
ctgov <- CTgov_sample_save

#Other input files
city_search_terms <- readxl::read_xlsx(paste0(folder_path, "data/1-sample-generation/", "city_search_terms_nordic-2022-11-29.xlsx"))
load(paste0(folder_path, "data/1-sample-generation/", "complemented.rda"))

# Clean search terms ----
city_search_terms <- filter(city_search_terms, is.na(duplicated))
city_search_terms <- city_search_terms$search_terms_combined
city_search_terms <- str_split(city_search_terms, ";")

city_search_terms <- unlist(city_search_terms)
city_search_terms <- unique(city_search_terms)
city_search_terms <- city_search_terms[! city_search_terms %in% NA]
city_search_terms <- str_c(city_search_terms, collapse = "|")


# Identify trials in EUTT file ----

# Filter for eligible institutions
eutt_nordic <- filter(eutt, grepl(city_search_terms, name_of_sponsor, ignore.case = T))
# n=2159


# Add eligibility variables from EUCTR protocol scraper ----

# Rename trial_id to eudract_id
euctr_all <- rename(euctr_all, eudract_id = eudract_number)
eutt_nordic <- rename(eutt_nordic, eudract_id = trial_id)
euctr_results <- rename(euctr_results, eudract_id = trial_id)

# In EUTT, merge records with several sponsors to get 1 entry per trial
eutt_nordic <- group_by(eutt_nordic, eudract_id)
eutt_nordic <- mutate(eutt_nordic, sponsors = paste0(name_of_sponsor, collapse=";"))
eutt_nordic <- ungroup(eutt_nordic)
eutt_nordic <- filter(eutt_nordic, !duplicated(eudract_id))

# Select protocols for the identified trials
euctr_protocols <- left_join(eutt_nordic, euctr_all, by = "eudract_id")

# Select only Nordic protocols for the identified trials
euctr_nordic_protocols <- filter(euctr_protocols, grepl("Denmark|Finland|Iceland|Norway|Sweden", 
                                                        member_state_concerned, ignore.case = T))

## Trial status based on Nordic protocols ----

# Merge Nordic protocols (note that status can be discrepant)
euctr_nordic_protocols <- group_by(euctr_nordic_protocols, eudract_id)
euctr_nordic_protocols <- mutate(euctr_nordic_protocols, trial_status = 
                            paste0(unique(trial_status.y), collapse = ";"))
euctr_nordic_protocols <- ungroup(euctr_nordic_protocols)

euctr_nordic_protocols$trial_status <- str_replace(euctr_nordic_protocols$trial_status, "^;", "")
euctr_nordic_protocols$trial_status <- str_replace(euctr_nordic_protocols$trial_status, ";$", "")
euctr_nordic_protocols$trial_status <- str_replace(euctr_nordic_protocols$trial_status, ";;", ";")


# Add to main table
eutt_nordic <- 
  left_join(eutt_nordic, select(filter(euctr_nordic_protocols, !duplicated(eudract_id)), 
                                eudract_id, trial_status), by = "eudract_id")


# Recode with order of preference: Prematurely ended, Completed, Ongoing

eutt_nordic <- mutate(eutt_nordic, trial_status_recoded = case_when(
  grepl("Prematurely Ended", trial_status.y) ~ "Any Prematurely Ended",
  grepl("Completed", trial_status.y) ~ "Any Completed, none Prematurely Ended",
  str_length(trial_status.y)<1 ~ NA_character_,
  TRUE ~ trial_status.y
))


# Add global completion date from EUCTR results scraper ----

eutt_nordic <- left_join(eutt_nordic, select(euctr_results, eudract_id, global_end_of_trial_date),
                         by = "eudract_id")


# Filter for status and completion year ----

eutt_nordic <- filter(eutt_nordic, grepl("Completed|Prematurely", trial_status_recoded, 
  ignore.case = T) | is.na(trial_status_recoded))

eutt_nordic <- mutate(eutt_nordic, completion_date_merged = case_when(
  str_length(global_end_of_trial_date)>1 ~ global_end_of_trial_date,
  TRUE ~ max_end_date
))

eutt_nordic <- filter(eutt_nordic, grepl("2016|2017|2018|2019", 
                      year(as_date(completion_date_merged))))


# Add de-cross-registration variables from EUCTR protocol scraper and results scraper ----

## NCT ID ----

euctr_protocols <- euctr_protocols %>%
  rename(nct_id = us_nct_clinicaltrials_gov_registry_number)

euctr_protocols <- group_by(euctr_protocols, eudract_id)
euctr_protocols <- mutate(euctr_protocols, nct_id = 
                        paste0(unique(nct_id), collapse = ";"))
euctr_protocols <- ungroup(euctr_protocols)
euctr_protocols$nct_id <- str_replace(euctr_protocols$nct_id, "^;", "")
euctr_protocols$nct_id <- str_replace(euctr_protocols$nct_id, ";$", "")
summary(as.factor(grepl(";", euctr_protocols$nct_id)))

eutt_nordic <- 
  left_join(eutt_nordic, select(filter(euctr_protocols, !duplicated(eudract_id)), 
                                eudract_id, nct_id), by = "eudract_id")

### Results scraper
euctr_results <- euctr_results %>%
  rename(nct_id_results = nct_number)

eutt_nordic <- 
  left_join(eutt_nordic, select(euctr_results, eudract_id, nct_id_results), by = "eudract_id")

eutt_nordic <- mutate(eutt_nordic, nct_id = case_when(
  str_length(nct_id_results)>1 ~ nct_id_results,
  str_length(nct_id)>1 ~ nct_id,
  TRUE ~ NA_character_
))


## Titles ----

eutt_nordic <- 
  left_join(eutt_nordic, select(filter(euctr_protocols, !duplicated(eudract_id)),
                                eudract_id, full_title_of_the_trial,
                                name_or_abbreviated_title_of_the_trial_where_available),
            by = "eudract_id")

eutt_nordic <- rename(eutt_nordic,
                      short_title = trial_title,
                      long_title = full_title_of_the_trial,
                      trial_name = name_or_abbreviated_title_of_the_trial_where_available)


#In results scraper, isrctn_number and who_utn_number only have missing values, so
#only adding spon_prot_number
eutt_nordic <- left_join(eutt_nordic, select(euctr_results, eudract_id,
                                             spon_prot_number), by = "eudract_id")



# Automatic de-cross-registration based on registry information ----

#ctgov_merge <- select(ctgov, nct_id, eudract_ids, brief_title, official_title)
ctgov_merge <- ctgov

#euctr_merge <- select(eutt_nordic, nct_id, eudract_id, short_title, long_title, trial_name)
euctr_merge <- eutt_nordic

euctr_merge$registry <- "euctr"
ctgov_merge$registry <- "ctgov"

ctgov_merge$trial_name <- NA_character_
ctgov_merge <- rename(ctgov_merge, eudract_id = eudract_ids,
                      short_title = brief_title,
                      long_title = official_title)

# Add project ID per row
#Note: for this project, we made one correction of the code after the original assignment of project IDs.
#That is why project IDs are here assigned for two batches. 
#For new projects, delete the 5 indicated lines within the next 10 lines.

euctr_merge_2 <- inner_join(euctr_merge, select(complemented, -registry), by = "eudract_id") #Delete line for new projects
euctr_merge <- anti_join(euctr_merge, select(complemented, -registry), by = "eudract_id") #Delete line for new projects
euctr_merge_2 <- euctr_merge_2[order(euctr_merge_2$eudract_id),] #Delete line for new projects

euctr_merge$proj_id <- seq.int(nrow(euctr_merge))
ctgov_merge$proj_id <- seq.int(nrow(ctgov_merge))+nrow(euctr_merge)

euctr_merge_2$proj_id <- seq.int(nrow(euctr_merge_2))+nrow(ctgov_merge)+nrow(euctr_merge) #Delete line for new projects
euctr_merge <- rbind(euctr_merge, euctr_merge_2) #Delete line for new projects

# Recode "phase" into character
euctr_merge$phase <- as.character(euctr_merge$phase)

nordic_all <- full_join(euctr_merge, ctgov_merge, by = c("proj_id",
  "eudract_id", "phase", "short_title", "sponsors", "nct_id",
  "long_title", "trial_name", "registry"))
#Note that trial_status.x is from eutt and .y is from euctr scraper
nordic_all <- rename(nordic_all, trial_status_eutt = trial_status.x, trial_status_euctr = trial_status.y)


nordic_all <- mutate(nordic_all, crossreg = case_when(
  str_length(eudract_id)>0 & (duplicated(eudract_id)|duplicated(eudract_id, fromLast=TRUE)) ~ "eudract_id",
  str_length(nct_id)>0 & (duplicated(nct_id)|duplicated(nct_id, fromLast=TRUE)) ~ "nct_id",
  str_length(long_title)>0 & (duplicated(long_title)|duplicated(long_title, fromLast=TRUE)) ~ "long_title",
  str_length(short_title)>0 & (duplicated(short_title)|duplicated(short_title, fromLast=TRUE)) ~ "short_title"
))


#order randomly before manual checking
set.seed(42)
rows <- sample(nrow(nordic_all))
nordic_all <- nordic_all[rows, ]

#Save dataset

writexl::write_xlsx(nordic_all, paste0(folder_path, "data/1-sample-generation/output-data/", "nordic-all-randomorder-", today, ".xlsx"))
save(nordic_all, file=paste0(folder_path, "data/1-sample-generation/output-data/", "nordic_all-randomorder-", today, ".rda"))


### END ###
