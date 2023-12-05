# nordic-trial-reporting
Data processing and analysis: Research project on trial reporting in the Nordic countries (protocol: https://osf.io/wua3r)
- Raw data on Zenodo that complement the current repository: https://zenodo.org/records/10091147
- Preprint: TBD
- Publication: TBD

DESCRIPTION UNDER CONSTRUCTION

In this repository, we share the code and data used for the project "Systematic evaluation of clinical trial reporting at medical universities and university hospitals in the Nordic countries" by Cathrine Axfors, Gustav Nilsonne, and team. Preregistered protocol: https://osf.io/97qkv/

OVERVIEW OF SCRIPTS

- Script 1a creates the sample of trials from ClinicalTrials.gov. It creates an R data file to be combined with EUCTR trials using script 1b, containing relevant trials from AACT (institution, years, status, completion date, descriptive data). Note that the final assignment of trials to institutions was verified manually. It also creates a dataset of CTgov variables to be used in script 2.

- Script 1b creates the sample of trials from EUCTR and combines with ClinicalTrials.gov sample generated by script 1. Also performs automatic cross-registration identification based on registry information (additional trial IDs mentioned in registrations). Trials are selected based on institution, completion year, and status. Also adds EUCTR variables to the dataset for later descriptive purposes.

- Script 2 performs data cleaning of variables from ClinicalTrials.gov and EUCTR to prepare the analysis dataset.
OBS! Between the automatic output from scripts 1a-1b and this one, we manually assessed trials for eligibility; performed extensive manual searches for publications; manually searched and verified cross-registrations; and verified summary results posted in the EUCTR.

- Script 3 performs the data analysis to produce in-text results (in console only), tables, and figures. We also share manually curated tables and figures.

CREATED DATABASE

Our database of results publications for the trial sample, to be used for further curation and research, can be found in the folder 4-created-database together with a variable description. For a full methods description, see our protocol (https://osf.io/97qkv/) together with the amendments to the protocol in the appendix of our preprint (TBD).  
NOTE: 
- Two independent reviewers completed publications searches for each trial.
- The file contains the results of the search forms that the reviewers filled out (form_id starting with F followed by a number, e.g., F1, F5977). Since some search forms were deleted as ineligible, the numbering is not consecutive. It also contains the final decisions for inclusion in the original study, i.e., the earliest results publication after solving discrepancies between reviewers (form_id starting with Final followed by a number, i.e., Final1 to Final2113, with consecutive numbering).
- Only for the earliest results publications for each article have we solved discrepancies between the two independent reviewers. All other articles remain POTENTIALLY eligible as results publications.
- We did not assess whether publications reported on the trial registrations' primary outcomes.

OTHER REPLICATION MATERIAL
For the benefit of others who want to create similar projects, we upload the instruction material developed to coordinate the manual publications searches. 
- Search manual
- Google Form used to document the search results
- Discrepancy corrections manual. The spreadsheet Cathrine_A_discrepancies accompanies this manual as an example.
- Deduplication of cross-registrations manual
