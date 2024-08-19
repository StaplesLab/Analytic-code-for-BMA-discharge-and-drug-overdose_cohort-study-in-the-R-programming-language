##########################################################################
# This work is licensed under CC BY-NC-SA 4.0. To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/4.0/
#AMA-OD_coh Variable Creation for Table 1 - demographic variables (pop density, income)
#Author: Xiao (Nicole) Hu
#Date: 2023-06-05
#Update: 2023-07-26
# Suggested citation: Hu X, Yu Y, Daly-Grafstein D, Khan M, Staples JA. AMA-OD_coh variable creation for Table 1. 2024 July 26. Retrieved from: ***LINK***

##########################################################################

# edit file paths for source, wd, data dictionary, data files

# libraries
source("R:/working/AMA-OD_coh/NH/code/AMA-OD - 0 Packages and working directory.R")

# set working dir
setwd("R:/working/AMA-OD_coh/NH/results/Table A")

# table A cohort
dad_demo_cohort <- readRDS("dad_cohort.rds")

#####################################################################
# Get population density classification
# fas xw file
fsa_xw <- read_delim("R:/working/XW files/fsa_2016_urbanRural.csv")
pop_density <- dad_demo_cohort %>% 
  left_join(fsa_xw, by = c("clnt_fsa" = "fsa")) %>% 
  select(moh_study_id, dad_id, fsaType) %>% 
  mutate(pop_density_class = case_when(
    fsaType %in% c("popCentreMedium", "popCentreLarge") ~ "Urban",
    fsaType %in% c("ruralArea","popCentreSmall") ~ "Rural",
    TRUE ~ "Missing"
  ))


#  Get client residential health authority
lu_pnet_ha_hsda_lha <- read_delim("R:/DATA/2023-05-09_190103/POC2023-04-05/vw_lu_pnet_ha_hsda_lha.csv.gz")
health_region <- dad_demo_cohort %>% 
  left_join(lu_pnet_ha_hsda_lha, by = c("clnt_lha" = "lha_id")) %>% 
  select(dad_id,ha_area) %>% 
  mutate(health_region = ifelse(ha_area == "09 Unknown HA" | is.na(ha_area), "Unknown/Out of Province HA",ha_area))


# Get MSP premium assistance received in the year of index
#msp_xw <- read_excel("R:/working/XW files/MSP Premium Codes_xw_PopData MDCentral_v2.xlsx",sheet = "Crosswalk")
clr <- read_delim("R:/DATA/2023-05-09_190103/POC2023-04-05/vw_clr.csv.gz")
msp_prem <- dad_demo_cohort %>% 
  mutate(sep_year = year(sep_date)) %>% 
  left_join(clr, by = c("sep_year" = "year", "moh_study_id")) %>% 
  select(dad_id, prem_asst_label) %>% 
  mutate(msp_prem = case_when(prem_asst_label %in% c("UNKNOWN","C") ~ "No",
                              TRUE ~ "Yes"))

# Get SDPR income assistance amount in the prior year to the index hosp
sdpr <- read_csv("R:/DATA/2023-08-15_230952/vw_sdpr.csv")
sdpr_pay <- dad_demo_cohort %>% 
  left_join(sdpr %>% select(moh_study_id, sdpr_date, pay), relationship = "many-to-many") %>% 
  filter(as.Date(ad_date) - as.Date(sdpr_date) <= 365 
         & as.Date(ad_date) - as.Date(sdpr_date) >= 1) %>% 
  group_by(dad_id) %>% 
  summarise(sdpr_pay = sum(pay)) %>% 
  ungroup() %>% 
  right_join(dad_demo_cohort) %>% 
  mutate(sdpr_pay = replace_na(sdpr_pay,0)) %>% 
  select(dad_id,sdpr_pay)

# Combine all demo covariates
pop_density %>%
  full_join(health_region) %>%
  full_join(msp_prem) %>% 
  full_join(sdpr_pay) %>% 
  saveRDS("demo_covariates.rds")
