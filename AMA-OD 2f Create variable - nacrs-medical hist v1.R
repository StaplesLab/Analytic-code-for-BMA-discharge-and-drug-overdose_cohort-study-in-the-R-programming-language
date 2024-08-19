#####################################################
# This work is licensed under CC BY-NC-SA 4.0. To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/4.0/
# Suggested citation: Hu X, Yu Y, Daly-Grafstein D, Erdelyi S, Staples JA. AMA-OD_coh create variable - NACRS medical history. 2023 Jun 21. Retrieved from: ***LINK***
# Variable Creation for Table 1 - NACRS PMH (5 year)
# Author: Xiao (Nicole) Hu 
# Date: 2023-06-21
# Updated: 2023-06-21
#####################################################

# libraries
source("R:/working/AMA-OD_coh/NH/code/AMA-OD - 0 Packages and working directory.R")

# set working dir
setwd("R:/working/AMA-OD_coh/NH/results/Table A")

################### NACRS pmh (ED visits for substance use)#####################
nacrs <- read_delim("R:/DATA/2023-05-09_190103/POC2023-04-05/vw_nacrs.csv.gz")
cohort <- readRDS("dad_cohort.rds")
icd10_ca <- readxl::read_xlsx("R:/working/XW files/Staples Lab - PMH XW ICD-10-CA - 2023-06-23.xlsx")
# For the LHS, duplicate the variables used for the join; rename other variables (eg DIAGX_all, TDAYS) to make clear their provenance
nacrs <- nacrs %>% 
  filter(ed_visit == 1|is.na(ed_visit)) %>% 
  unite("ed_diag_all", contains("ed_diag_"), na.rm = TRUE, sep = "_, _") %>% 
  mutate(ed_diag_all = paste0("_", ed_diag_all, "_")) %>% 
  select(moh_study_id, pmh_regdate = reg_date, pmh_regdate_merge = reg_date, pmh_diagx_all = ed_diag_all) %>% 
  mutate(pmh_diagx_all = ifelse(pmh_diagx_all == "__",NA,pmh_diagx_all)) %>% 
  filter(!is.na(pmh_diagx_all)) %>% 
  setDT()

# For the RHS, create the dates used for the join and duplicate these variables
coh_ids <- cohort %>%
  #start from ad_date [06-15]
  mutate(addate_m1d = ad_date,
         addate_m1y = ad_date - years(5)) %>% 
  select(moh_study_id, dad_id, ad_date,sep_date, addate_m1y, addate_m1d, start = addate_m1y, end = addate_m1d) %>%
  setDT()

# Complete a right_join (ie. keep all rows from coh_ids)
pmh_nacrs <- nacrs[coh_ids,
                   on = .(moh_study_id == moh_study_id,
                          pmh_regdate_merge >= start,
                          pmh_regdate_merge <= end)] %>%
  select(moh_study_id, dad_id, sep_date, ad_date, pmh_regdate, pmh_diagx_all)

# On reg_date is one visit, concatenate all diagnosis in one pmh_regdate
pmh_nacrs <- pmh_nacrs %>% 
  group_by(moh_study_id, dad_id, ad_date,pmh_regdate) %>%
  summarize(pmh_diagx_all = paste(pmh_diagx_all, collapse=", "))


# Map through the list of selected comorbidities
selected_coms = c('drugs_opioid','drugs_all', 'drugs_nonopioid', 'alcohol')
pmh_nacrs <- map(selected_coms, ~ if_else(str_detect(pmh_nacrs$pmh_diagx_all, 
                                                     icd10_ca %>% 
                                                       filter(pmh_name == .x) %>% 
                                                       pull(icd10_code) %>% 
                                                       unique() %>% 
                                                       paste0(., collapse = "|")), 
                                          1, 
                                          0)) %>% 
  set_names(paste0(selected_coms, ".nacrs")) %>% 
  bind_cols(pmh_nacrs, .)

pmh_nacrs <-   
  pmh_nacrs %>%
  mutate(any_sbstcs.nacrs = case_when(alcohol.nacrs == 1 | drugs_all.nacrs == 1 | drugs_opioid.nacrs == 1 | drugs_nonopioid.nacrs == 1 ~ 1, TRUE ~ 0)) %>% 
  # sum each comorbidity for each patient
  group_by(moh_study_id,dad_id,ad_date) %>%
  # You want to capture the number of visits for a given comorbidity because our criteria require >=1 visits to qualify as 'having' that diagnosis.
  summarise(  pmh_diagx_all = paste(pmh_diagx_all, collapse=", "),
              num_ed_prev_5year=n(),
              across(.cols = ends_with("nacrs"),
                     .fns = ~sum(.x, na.rm = TRUE)) ) %>%
  ungroup() %>%
  mutate(num_ed_prev_5year = if_else(pmh_diagx_all == "NA", as.double(0), as.double(num_ed_prev_5year))) %>% 
  replace(is.na(.), 0) %>% 
  select(-pmh_diagx_all)

saveRDS(pmh_nacrs, "pmh_nacrs.rds")



