
# Systematic evaluation of clinical trial reporting at medical universities and university 
# hospitals in the Nordic countries 
# Cathrine Axfors
# OSF protocol: https://osf.io/uckyt


# This code is based extensively on the IntoValue 1-2 project. 
# See e.g.: https://github.com/quest-bih/IntoValue2, https://github.com/maia-sh/intovalue-data
# Any errors in this adapted version are mine.


# README Script 1a ----

# Description: Creates the sample of trials from ClinicalTrials.gov.
# Note that you first need to download the AACT dataset (size several GB),
# from https://aact.ctti-clinicaltrials.org/pipe_files (for new projects)
# Note also that you need to use the same folder structure as in the Nordic trials project (as on GitHub)
# OBS! To replicate the Nordic trials project, download the AACT data folder from Zenodo 
# (https://zenodo.org/records/10091147) and save in the 'raw data' folder.

# Output: creates R data file to be combined with EUCTR trials using script 1b,
# containing relevant trials from AACT (institution, years, status, completion date, descriptive data).
# Note that the final assignment of trials to institutions is verified manually.
# Also creates dataset of CTgov variables to be used in script 2.



# Hard code preparations ----

#Enter here the path to the folder where you saved the "data" and "code" folders (see our GitHub)
folder_path <- ""

#Search terms for institutions
city_search_terms <- readxl::read_xlsx(paste0(folder_path, "data/1-sample-generation/", "city_search_terms_nordic-2022-11-29.xlsx"))

today <- "" #Enter today's date YYYY-MM-DD (our date: 2023-11-09)


# Load packages ----

library(tidyverse)


# Load AACT dataset ----

#The AACT dataset was downloaded on 11-27, reflecting data from 2022-11-09
#AACT filenames that we need to load
AACT_dataset_names <- c("studies", "overall_officials", "sponsors", "responsible_parties",
                        "facilities", "interventions", "calculated_values", "id_information",
                        "designs")

AACT_dataset_files <- paste0(folder_path, "data/1-sample-generation/raw-data/AACT-2022-11-09/", AACT_dataset_names, ".txt")
AACT_datasets <- AACT_dataset_files %>%
  map(read_delim, delim = "|")
names(AACT_datasets) <- AACT_dataset_names


# Load search terms for the institutions ----

city_search_terms <- filter(city_search_terms, is.na(duplicated))
city_search_terms <- city_search_terms$search_terms_combined
city_search_terms <- str_split(city_search_terms, ";")

cities <- city_search_terms %>% map_chr(1)
city_search_terms <- city_search_terms %>%
  map(function(x) paste0("\\b", x, "\\b", collapse = "|"))
names(city_search_terms) <- cities


# Search for trials led by eligible institutions and get NCTs of those trials ----

#Credit for these functions to the IntoValue project authors:

#we want to find the names of the different universities in the
#sponsor/responsible_party/facilities columns
grep_fast <- function(pattern, x)
{
  return(which(str_detect(x, pattern)))
}

get_nct <- function(affil_indices, dataset)
{
  ncts <- dataset %>%
    slice(affil_indices) %>%
    select(nct_id)
  return(ncts[[1]])
}

city_grep <- function(dataset, colname, grep_terms)
{
  indices <- map(grep_terms, grep_fast, x=dataset[[colname]])
  city_ncts <- map(indices, get_nct, dataset=dataset)
  return(city_ncts)
}

city_grep_indices <- function(dataset, colname, grep_terms)
{
  indices <- map(grep_terms, grep_fast, x=dataset[[colname]])
  return(indices)
}

#search the different affiliation datasets for the city search terms
grep_sponsor <- city_grep(AACT_datasets$sponsors %>% filter(lead_or_collaborator == "lead"), "name", city_search_terms)
grep_resp_party_org <- city_grep(AACT_datasets$responsible_parties, "organization", city_search_terms)
grep_resp_party_affil <- city_grep(AACT_datasets$responsible_parties, "affiliation", city_search_terms)


#joining of the different grep results
affil_join <- function(affil_nct_list)
{
  affil_indices_joined <- affil_nct_list %>%
    pmap(c) %>%
    map(unique) %>%
    map(sort)
}

#combine the results for the different columns to get the studies with a match for
#a lead (sponsor/responsible_party) or facility affiliation
grep_results_lead <- list(grep_sponsor, grep_resp_party_org, grep_resp_party_affil)

#for each study we want to know which institution has
#a lead (sponsor/responsible_party)
affil_ncts_lead <- affil_join(grep_results_lead)

#get the unique study IDs
unique_ncts_lead <- unique(unlist(affil_ncts_lead))


# Filter CTgov dataset for eligible studies (institution lead, completion year, study status) ----

CTgov_sample <- AACT_datasets$studies

completion_years <- c("2016", "2017", "2018", "2019") %>%
  paste(collapse="|")
study_status <- c("Completed" , "Terminated" , "Unknown status") %>%
  paste(collapse="|")

#filter cases for years, status, and affiliation
CTgov_sample <- CTgov_sample %>%
  filter(nct_id %in% unique_ncts_lead) %>%
  filter(grepl(completion_years, completion_date)) %>%
  filter(study_type == "Interventional")

#filter cases for study status
CTgov_sample <- CTgov_sample %>%
  filter(grepl(study_status, overall_status))


# Create for each study a list of affiliated institutions and add to main table ----


get_city_per_NCT <- function(cities_nct_list, unique_ncts)
{
  cities_col <- vector("list", length(unique_ncts))
  names(cities_col) <- unique_ncts
  for (city in names(cities_nct_list)) {
    cities_col[cities_nct_list[[city]]] <-
      paste(cities_col[cities_nct_list[[city]]], city, sep = ";")
  }
  cities_col <- substring(cities_col, first = 6)
  names(cities_col) <- unique_ncts
  return(cities_col)
}

#create columns that list which institutions are affiliated with the studies
nct_cities_lead <- get_city_per_NCT(affil_ncts_lead, unique_ncts_lead)

#prepare for joining with main table
nct_cities_lead_tbl <- as_tibble(cbind(unique_ncts_lead, nct_cities_lead))
names(nct_cities_lead_tbl) <- c("nct_id", "cities_lead")

#add columns to main table
CTgov_sample <- CTgov_sample %>%
  left_join(nct_cities_lead_tbl, by = "nct_id")


# Add descriptive variables to the table ----

## add PI affil info to main table (for information, not eligibility) ----
#first get only affils of relevant PIs from full table
grep_PI_indices <- city_grep_indices(AACT_datasets$overall_officials, "affiliation", city_search_terms) %>% 
  unlist() %>% unique() %>% sort() 
PI_affils_table_filtered <- AACT_datasets$overall_officials[grep_PI_indices,] %>%
  distinct(nct_id, .keep_all = TRUE) #only take first relevant PI for each study to allow a clean join
PI_affils_table_filtered <- PI_affils_table_filtered %>%
  select(nct_id, name, affiliation) %>%
  rename(PI_name = name,
         PI_affiliation = affiliation)

CTgov_sample <- CTgov_sample %>%
  left_join(PI_affils_table_filtered, by = "nct_id")

## add intervention name ----
interventions_combined <- AACT_datasets$interventions %>%
  group_by(nct_id) %>%
  summarise(intervention_names_comb = paste(name, collapse=" | "))

CTgov_sample <- CTgov_sample %>%
  left_join(interventions_combined, by = "nct_id")


## Intervention type ----
intervention_types_combined <- AACT_datasets$interventions %>%
  group_by(nct_id) %>%
  summarise(intervention_types_combined = paste(unique(intervention_type), collapse=" | "))

CTgov_sample <- CTgov_sample %>%
  left_join(intervention_types_combined, by = "nct_id")


## add "calculated values" variables ----
CTgov_sample <- CTgov_sample %>%
  left_join(AACT_datasets$calculated_values, by = "nct_id")


## Add lead sponsors to main table ----
sponsor_table_filtered <- AACT_datasets$sponsors
sponsor_table_filtered_lead <- filter(sponsor_table_filtered, lead_or_collaborator == "lead")
sponsor_table_filtered_lead <- group_by(sponsor_table_filtered_lead, nct_id)
sponsor_table_filtered_lead <- sponsor_table_filtered_lead %>%
  mutate(sponsors = paste0(name, collapse=";"))
sponsor_table_filtered_lead <- ungroup(sponsor_table_filtered_lead)
sponsor_table_filtered_lead <- sponsor_table_filtered_lead %>%
  filter(!duplicated(nct_id)) %>%
  select(nct_id, sponsors)

CTgov_sample <- CTgov_sample %>%
  left_join(sponsor_table_filtered_lead, by = "nct_id")


## Add agency_classes (for all sponsors, also collaborators) to main table ----
sponsor_table_filtered <- group_by(sponsor_table_filtered, nct_id)
#sponsor_table_filtered <- sponsor_table_filtered %>% #unmute to add ALL sponsors, also collaborators
#  mutate(all_sponsors = paste0(name, collapse=";")) #unmute to add ALL sponsors, also collaborators
sponsor_table_filtered <- sponsor_table_filtered %>%
  mutate(agency_classes = paste0(agency_class, collapse=";"))
sponsor_table_filtered <- ungroup(sponsor_table_filtered)
sponsor_table_filtered <- sponsor_table_filtered %>%
  filter(!duplicated(nct_id)) %>%
  select(nct_id, agency_classes
#,all_sponsors #unmute to add ALL sponsors, also collaborators         
         )

CTgov_sample <- CTgov_sample %>%
  left_join(sponsor_table_filtered, by = "nct_id")


## Add responsible parties to main table ----
responsible_parties_filtered <- AACT_datasets$responsible_parties
responsible_parties_filtered <- group_by(responsible_parties_filtered, nct_id)
responsible_parties_filtered <- responsible_parties_filtered %>%
  mutate(responsible_parties_orgs = paste0(organization, collapse=";"))
responsible_parties_filtered <- responsible_parties_filtered %>%
  mutate(responsible_parties_affils = paste0(affiliation, collapse=";"))
responsible_parties_filtered <- responsible_parties_filtered %>%
  mutate(responsible_parties_types = paste0(responsible_party_type, collapse=";"))
responsible_parties_filtered <- ungroup(responsible_parties_filtered)
responsible_parties_filtered <- responsible_parties_filtered %>%
  filter(!duplicated(nct_id)) %>%
  select(nct_id, responsible_parties_types, responsible_parties_orgs, responsible_parties_affils)

CTgov_sample <- CTgov_sample %>%
  left_join(responsible_parties_filtered, by = "nct_id")


## Add design variables ----

designs <- AACT_datasets$designs

CTgov_sample <- CTgov_sample %>%
  left_join(select(designs, nct_id, allocation, intervention_model, masking), by = "nct_id")


## EudraCT ID ----
ids_filtered <- AACT_datasets$id_information
ids_eudract <- ids_filtered %>%
  filter(id_type == "EudraCT Number") %>%
  rename(eudract_id = id_value)
ids_eudract <- group_by(ids_eudract, nct_id)
ids_eudract <- mutate(ids_eudract, eudract_ids = 
                        paste0(unique(eudract_id), collapse = ";"))
ids_eudract <- ungroup(ids_eudract)

ids_eudract <- ids_eudract %>%
  filter(!duplicated(nct_id)) %>%
  select(nct_id, eudract_ids)

CTgov_sample <- CTgov_sample %>%
  left_join(ids_eudract, by = "nct_id")
summary(as.factor(grepl(";", CTgov_sample$eudract_ids)))
#1 with many

## Other IDs ----
ids_other <- ids_filtered %>%
  filter(!id_type == "EudraCT Number") %>%
  rename(other_id = id_value)
ids_other <- group_by(ids_other, nct_id)
ids_other <- mutate(ids_other, other_ids = 
                        paste0(unique(other_id), collapse = ";"))
ids_other <- ungroup(ids_other)

ids_other <- ids_other %>%
  filter(!duplicated(nct_id)) %>%
  select(nct_id, other_ids)

CTgov_sample <- CTgov_sample %>%
  left_join(ids_other, by = "nct_id")


# Select columns to save ----
CTgov_sample_save <- CTgov_sample %>%
  rename(interventions = intervention_names_comb) %>% 
  select(nct_id, cities_lead, brief_title, official_title,
         study_first_submitted_date, start_date, start_date_type,
         completion_date, completion_date_type, PI_name,
         PI_affiliation, interventions, intervention_types_combined, overall_status,
         phase, enrollment, enrollment_type, were_results_reported,
         sponsors, agency_classes, responsible_parties_types, responsible_parties_orgs, 
         responsible_parties_affils, eudract_ids, other_ids)


# Save CT.gov trial sample ----
#Note: not all associations of the trials to the cites are correct, and institutions have to be
#checked manually.
#To be used in script 1b:
save(CTgov_sample_save, file=paste0(folder_path, "data/1-sample-generation/output-data/", "clintrialsnord_CTgov_sample-", today, ".rda"))

#Full output (to be used in script 2):
save(CTgov_sample, file=paste0(folder_path, "data/1-sample-generation/output-data/", "clintrialsnord_CTgov_full-", today, ".rda"))

### END ###
