##########################################################################
# This work is licensed under CC BY-NC-SA 4.0. To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/4.0/
# Suggested citation: Hu x, Yu Y, Daly-Grafstein D, Khan M, Erdelyi S, Staples JA. AMA-OD_coh create variables - medications. 2023 Jun 8. Retrieved from: ***LINK***
#Create medication variables
#Author: Xiao Hu
#Date: 2023-06-08
#Update: 2023-06-08
##########################################################################

# libraries
source("R:/working/AMA-OD_coh/NH/code/AMA-OD - 0 Packages and working directory.R")

# set working dir
setwd("R:/working/AMA-OD_coh/NH/results/Table A")

############ read in ###################3
cohort <- readRDS("dad_cohort.rds")

f <- function(x,pos){
  subset(x, 
         select = c(
           'moh_study_id',
           'din_pin',
           "dispensing_claims_srv_date",
           "dispensing_claims_qty",
           "dispensing_claims_days_sply",
           "dispensing_claims_sply_end_date"),
         moh_study_id %in% cohort$moh_study_id
  )
}

read_chunk <- function(chunk) {
  df <- read_csv_chunked(chunk,
                         callback =  DataFrameCallback$new(f),
                         chunk_size = 1000000) 
  return(df)
}

path_rx <- "R:/DATA/2023-05-09_190103/POC2023-04-05/vw_pnet_rx_group.csv.gz"
pnet_rx_group <- read_chunk(path_rx) #11min
saveRDS(pnet_rx_group,"cohort_pnet_rx.rds") #update: 06-26

path_other <- "R:/DATA/2023-05-09_190103/POC2023-04-05/vw_pnet_other.csv.gz"
pnet_other <- read_chunk(path_other)
saveRDS(pnet_other, "cohort_pnet_other.rds") #update: 06-26

pnet <- pnet_rx_group %>% bind_rows(pnet_other) #update: 06-26
saveRDS(pnet, "cohort_pnet.rds")

rm(pnet_other)


cohort <- readRDS("dad_cohort.rds")
pnet <- readRDS("cohort_pnet.rds")
lu_pnet_dinpin_rx_group <- read_delim("R:/DATA/2023-05-09_190103/POC2023-04-05/vw_lu_pnet_dinpin_rx_group.csv.gz")
############# Number of total medications active at baseline (t0 = hospital admission date) #################

# filter data such that index dates fall between prescription service dates and
# prescription service dates + the number of expected days of medication supply
# prescription service date must come before event index date

pharm_active <- pnet %>%   
  inner_join(cohort %>% select(moh_study_id, ad_date),
             by = "moh_study_id",
             relationship = "many-to-many") %>% 
  filter((ad_date > dispensing_claims_srv_date) & 
           (ad_date <= (dispensing_claims_srv_date + dispensing_claims_days_sply))) %>% 
  select(moh_study_id, ad_date, din_pin) %>%
  distinct() %>%
  group_by(moh_study_id,ad_date) %>%
  summarise(num_active = n()) %>%
  ungroup() %>%
  right_join(cohort %>% select(moh_study_id, ad_date),
             by = c("moh_study_id","ad_date")) %>%
  mutate(num_active = replace_na(num_active, 0)) %>%
  select(moh_study_id, ad_date, num_active)

saveRDS(pharm_active,"ttl_meds_ad.rds") #update: 0626

############# Active OAT prescription at admission date ######################

oat_active <- pnet %>% 
  inner_join(lu_pnet_dinpin_rx_group,
             by = "din_pin") %>% 
  #get all oat prescriptions
  filter(oat == 1) %>%
  #join with the cohort to get everyone's oat prescriptions
  inner_join(cohort %>% select(moh_study_id, ad_date),
             by = "moh_study_id",
             relationship = "many-to-many") %>% 
  #include active prescriptions at admission date
  filter((ad_date >= dispensing_claims_srv_date) & 
           (ad_date <= (dispensing_claims_srv_date + dispensing_claims_days_sply + 1))) %>%
  #only care if there is a oat prescription active at ad_date
  select(moh_study_id, ad_date, oat) %>%
  distinct() %>% 
  right_join(cohort %>% select(moh_study_id, ad_date),
             by = c("moh_study_id","ad_date")) %>% 
  mutate(oat_active_ad = as.numeric(!is.na(oat))) %>% 
  select(moh_study_id, ad_date, oat_active_ad)

saveRDS(oat_active, "oat_active_ad.rds") #update: 0626

############# Number of distinct medications filled in the 90 days prior to hospital admission #######
ttl_meds_90ad <- pnet %>%   
  inner_join(cohort %>% select(moh_study_id, ad_date),
             by = "moh_study_id",
             relationship = "many-to-many") %>% 
  filter((ad_date > dispensing_claims_srv_date) & 
           (ad_date <= (dispensing_claims_srv_date + 90))) %>% 
  select(moh_study_id, ad_date, din_pin) %>% 
  distinct() %>% 
  group_by(moh_study_id, ad_date) %>% 
  summarise(ttl_meds_90ad = n()) %>%
  ungroup() %>%
  right_join(cohort %>% select(moh_study_id, ad_date),
             by = c("moh_study_id","ad_date")) %>%
  mutate(ttl_meds_90ad = replace_na(ttl_meds_90ad, 0)) %>%
  select(moh_study_id, ad_date, ttl_meds_90ad)

saveRDS(ttl_meds_90ad, "ttl_meds_90ad.rds") #update: 0624

############# Selected prescription medications filled in the 90d prior to index date ############

#selected: OAT, Benzodiazepine, Opioids, Antipsychotics, Other
#sum of these five columns = number of distinct medications filled in the 90 days prior to hospital admission
lu_pnet_dinpin_rx_group <- lu_pnet_dinpin_rx_group %>% 
  mutate(antipsyc = as.numeric(grepl("antipsychotic", ahfs_3, ignore.case = T))) %>%
  mutate(opioid = as.numeric(!is.na(opioid_category))) %>% 
  rowwise() %>% 
  mutate(other = as.numeric(sum(c(oat, benzo, opioid, antipsyc, z_drug),na.rm = T) == 0))

sub_meds_90ad <- pnet %>%   
  inner_join(cohort %>% select(moh_study_id, ad_date),
             by = "moh_study_id",
             relationship = "many-to-many") %>% 
  filter((ad_date > dispensing_claims_srv_date) & 
           (ad_date <= (dispensing_claims_srv_date + 90))) %>% 
  select(moh_study_id, ad_date, din_pin) %>% 
  distinct() %>% 
  mutate(oat = as.numeric(din_pin %in% as.numeric(lu_pnet_dinpin_rx_group %>% filter(oat==1) %>% pull(din_pin))),
         benzo = as.numeric(din_pin %in% as.numeric(lu_pnet_dinpin_rx_group %>% filter(benzo==1 | z_drug==1) %>% pull(din_pin))),
         opioid = as.numeric(din_pin %in% as.numeric(lu_pnet_dinpin_rx_group %>% filter(opioid == 1) %>% filter(oat == 0 | is.na(oat)) %>% pull(din_pin))),
         antipsyc = as.numeric(din_pin %in% as.numeric(lu_pnet_dinpin_rx_group %>% filter(antipsyc==1) %>% pull(din_pin))),
         other = as.numeric(din_pin %in% as.numeric(lu_pnet_dinpin_rx_group %>% filter(other==1) %>% pull(din_pin))|
                              !(din_pin %in% as.numeric(lu_pnet_dinpin_rx_group %>% pull(din_pin))))) %>% 
  select(-din_pin) %>% 
  group_by(moh_study_id, ad_date) %>% 
  summarise(across(everything(), sum)) %>% 
  ungroup() %>% 
  right_join(cohort %>% select(moh_study_id,ad_date),
             by = c("moh_study_id","ad_date")) %>% 
  replace(is.na(.), 0)

saveRDS(sub_meds_90ad, "sub_meds_90ad.rds") #update: 0624

############# If OAT is dispensed in 5 years prior to ad_date ##################
oat_5y <- pnet %>%   
  inner_join(cohort %>% select(moh_study_id, ad_date),
             by = "moh_study_id",
             relationship = "many-to-many") %>% 
  filter((ad_date > dispensing_claims_srv_date) & 
           (ad_date <= (dispensing_claims_srv_date + years(5)))) %>% 
  select(moh_study_id, ad_date, din_pin) %>% 
  distinct() %>% 
  mutate(oat_5y = as.numeric(din_pin %in% as.numeric(lu_pnet_dinpin_rx_group %>% filter(oat==1) %>% pull(din_pin)))) %>% 
  select(-din_pin) %>% 
  group_by(moh_study_id, ad_date) %>% 
  summarise(across(everything(), sum)) %>% 
  ungroup() %>% 
  right_join(cohort %>% select(moh_study_id,ad_date),
             by = c("moh_study_id","ad_date")) %>% 
  replace(is.na(.), 0) %>% 
  mutate(geq1_oat_5y = as.numeric(oat_5y > 0))

saveRDS(oat_5y,"oat_5y.rds")

############## If OAT is dispensed in 1 years prior to ad_date #################

oat_1y <- pnet %>%   
  inner_join(cohort %>% select(moh_study_id, ad_date, sep_date),
             by = "moh_study_id",
             relationship = "many-to-many") %>% 
  filter(dispensing_claims_srv_date < ad_date & 
           dispensing_claims_srv_date >= ad_date - years(1)) %>% 
  # filter((ad_date > dispensing_claims_srv_date) & 
  #          (ad_date <= (dispensing_claims_srv_date + years(1)))) %>% 
  select(moh_study_id, ad_date, din_pin) %>% 
  distinct() %>% 
  mutate(oat_1y = as.numeric(din_pin %in% as.numeric(lu_pnet_dinpin_rx_group %>% filter(oat==1) %>% pull(din_pin)))) %>% 
  select(-din_pin) %>% 
  group_by(moh_study_id, ad_date) %>% 
  summarise(across(everything(), sum)) %>% 
  ungroup() %>% 
  right_join(cohort %>% select(moh_study_id,ad_date),
             by = c("moh_study_id","ad_date")) %>% 
  replace(is.na(.), 0) %>% 
  mutate(geq1_oat_1y = as.numeric(oat_1y > 0))

saveRDS(oat_1y,"oat_1y.rds")

############## If OAT is dispensed in 3 years prior to ad_date #################

oat_3y <- pnet %>%   
  inner_join(cohort %>% select(moh_study_id, ad_date, sep_date),
             by = "moh_study_id",
             relationship = "many-to-many") %>% 
  filter(dispensing_claims_srv_date < ad_date & 
           dispensing_claims_srv_date >= ad_date - years(3)) %>% 
  select(moh_study_id, ad_date, din_pin) %>% 
  distinct() %>% 
  mutate(oat_3y = as.numeric(din_pin %in% as.numeric(lu_pnet_dinpin_rx_group %>% filter(oat==1) %>% pull(din_pin)))) %>% 
  select(-din_pin) %>% 
  group_by(moh_study_id, ad_date) %>% 
  summarise(across(everything(), sum)) %>% 
  ungroup() %>% 
  right_join(cohort %>% select(moh_study_id,ad_date),
             by = c("moh_study_id","ad_date")) %>% 
  replace(is.na(.), 0) %>% 
  mutate(geq1_oat_3y = as.numeric(oat_3y > 0))

saveRDS(oat_3y,"oat_3y.rds")

################## OAT dispensed <= 3 days after discharge ###################

early_oat <- pnet %>%   
  inner_join(cohort %>% select(moh_study_id, sep_date),
             by = "moh_study_id",
             relationship = "many-to-many") %>% 
  filter((dispensing_claims_srv_date > sep_date) & 
           (dispensing_claims_srv_date <= sep_date + days(3))) %>% 
  select(moh_study_id, sep_date, din_pin) %>% 
  distinct() %>% 
  mutate(early_oat = as.numeric(din_pin %in% as.numeric(lu_pnet_dinpin_rx_group %>% filter(oat==1) %>% pull(din_pin)))) %>% 
  select(-din_pin) %>% 
  group_by(moh_study_id, sep_date) %>% 
  summarise(across(everything(), sum)) %>% 
  ungroup() %>% 
  right_join(cohort %>% select(moh_study_id,sep_date),
             by = c("moh_study_id","sep_date")) %>% 
  replace(is.na(.), 0) %>% 
  mutate(early_oat = as.numeric(early_oat > 0))

saveRDS(early_oat,"early_oat.rds")

##### combine all together ######
medication_covariates <- 
  readRDS("ttl_meds_ad.rds") %>% 
    full_join(readRDS("oat_active_ad.rds")) %>% 
    full_join(readRDS("ttl_meds_90ad.rds")) %>% 
    full_join(readRDS("sub_meds_90ad.rds")) %>% 
    full_join(readRDS("oat_5y.rds")) %>% 
    distinct()

saveRDS(medication_covariates, "medication_covariates.rds")

