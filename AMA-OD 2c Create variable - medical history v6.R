#####################################################
# This work is licensed under CC BY-NC-SA 4.0. To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/4.0/
# Suggested citation: Hu X, Yu Y, Daly-Grafstein D, Khan M, Erdelyi S, Staples JA. AMA-OD_coh - medical history variables. 2023 June 14. Retrieved from: ***LINK***
# Variable Creation for Table 1 - medical history
# Author: Xiao (Nicole) Hu 
# Date: 2023-06-05
# Updated: 2023-06-14
#####################################################
# libraries
source("R:/working/AMA-OD_coh/NH/code/AMA-OD - 0 Packages and working directory.R")

# set working dir
setwd("R:/working/AMA-OD_coh/NH/results/Table A")
cohort <- readRDS("dad_cohort.rds")

############################# Read in ################################
# table A cohort
dad <- read_delim("R:/DATA/2023-05-09_190103/POC2023-04-05/vw_dad.csv.gz")

# get all hospitalizations of cohort
hosp <- dad %>% 
  filter(moh_study_id %in% cohort$moh_study_id) %>% 
  select(moh_study_id, dad_id, ad_date, starts_with("sep"), t_days, starts_with("diagx"),
          # Also select hospto, hospfrom, admit variables
         hosp_to,hosp_from, admit, hosp)
cbind(nrow(hosp), n_distinct(hosp$moh_study_id)) 
#v5: 582831 104652; v6:585535 105289

# creat los
hosp <- hosp %>%
  mutate(sep_date = ymd(sep_date), 
         ad_date = ymd(ad_date), 
         tlos = as.period(ad_date %--% sep_date),
         los = as.duration(tlos) / ddays(1)) %>% 
  select(-tlos)

# rename columns (avoid duplicate names when joining with dad)
colnames(hosp) <- paste0(colnames(hosp), ".hosp")
#saveRDS(hosp,"cohort_hosp.rds") #update 06-26

# read in cohort msp
f <- function(x,pos){
  subset(x, 
         select = c(moh_study_id, msp_id, serv_date,serv_loc, 
                    fitm,clm_spec,diag_cd_1,diag_cd_2,diag_cd_3),
         moh_study_id %in% cohort$moh_study_id)
}

read_chunk <- function(chunk) {
  df <- read_csv_chunked(chunk,
                         callback =  DataFrameCallback$new(f),
                         chunk_size = 1000000) 
  return(df)
}

path <- "R:/DATA/2023-05-09_190103/POC2023-04-05/vw_msp.csv.gz"
cohort_msp <- read_chunk(path) #15min using fast mahine

#saveRDS(cohort_msp, "cohort_msp.rds") #update: 06-26

################### Hospitalizations in prior year ###########################
hosp <- readRDS("cohort_hosp.rds")
# discharge date within 365 days, and at least 1 day before event index date
prior_hosp <- hosp %>% 
  # only keep the last hospitalization (no transfer to) 
  filter(hosp_to.hosp == 6 | hosp_to.hosp == 7 | is.na(hosp_to.hosp))  %>%
  inner_join(., cohort, by = c("moh_study_id.hosp" = "moh_study_id"),relationship = "many-to-many") %>%
  # if the event happened in the interval of [ad_date - 365, ad_date - 1]
  filter(as.Date(ad_date) - as.Date(sep_date.hosp) <= 365 
         & as.Date(ad_date) - as.Date(sep_date.hosp) >= 1) %>%
  group_by(dad_id) %>% 
  summarise(num_hosp_prev_year=n(), tdays_prev_year=sum(los.hosp)) %>% 
  right_join(cohort, by = "dad_id") %>%
  mutate(num_hosp_prev_year = replace_na(num_hosp_prev_year, 0),
         tdays_prev_year = replace_na(tdays_prev_year, 0),
         geq1_hosp_prev_year = ifelse(num_hosp_prev_year > 0, 1, 0)) %>%
  select(dad_id,moh_study_id, sep_date, num_hosp_prev_year, geq1_hosp_prev_year, tdays_prev_year)

saveRDS(prior_hosp,"prior_hosp.rds") #update 06-26  
no_pr_hosp_id <- prior_hosp %>% filter(num_hosp_prev_year == 0) %>% pull(dad_id)
pr_hosp_id <- prior_hosp %>% filter(num_hosp_prev_year > 0 ) %>% pull(dad_id)

################## Physician clinic visits in prior year #######################
cohort_msp <- readRDS("cohort_msp_servloc.rds")
MD_spec_XW <- readxl::read_xlsx("R:/working/XW files/Staples Lab - MD specialties included for 'prior clinic visits' - 2023-08-08.xlsx",sheet = "Sheet1")
MD_spec_XW$Code <- as.numeric(MD_spec_XW$Code)
prior_clinic <- cohort_msp  %>%
  # msp table rows represent billing items, so only count unique dates as one visit
  #filter(!serv_loc %in% c('D','E','F','G','H','I')) %>%  #median = 14, prop = 0.75
  filter(serv_loc == "A") %>%
  left_join(MD_spec_XW, by = c("clm_spec" = "Code")) %>% 
  filter(incl_for_outpt_visits == 1) %>% 
  select(moh_study_id, serv_date) %>% 
  distinct() %>% 
  inner_join(cohort, by = "moh_study_id",relationship = "many-to-many") %>%
  filter(as.Date(serv_date) >= as.Date(ad_date)-years(1)  & 
           as.Date(serv_date) <= as.Date(ad_date)-days(1) ) %>% 
  group_by(moh_study_id, sep_date) %>%
  summarise(num_clinic_prev_year = n()) %>%
  ungroup() %>%
  right_join(cohort, by = c("moh_study_id","sep_date"),) %>%
  mutate(num_clinic_prev_year = replace_na(num_clinic_prev_year, 0),
        geq7_clinic_prev_year = ifelse(num_clinic_prev_year >= 7, 1,0)) %>%
  select(moh_study_id, sep_date,num_clinic_prev_year,geq7_clinic_prev_year)

#median(prior_clinic$num_clinic_prev_year)
#prior_clinic %>% group_by(geq7_clinic_prev_year) %>% summarize(p = n()/nrow(cohort))
# Save file
saveRDS(prior_clinic, "prior_clinic.rds") #update 08-08


################# Psychiatric hospitalizations in the prior year ##############
prior_psyc_hosp <- cohort %>% 
  #pychiatric hospitalizations are diagx_1 starts with "F"
  left_join(hosp %>% filter((str_detect(diagx_1.hosp, 'F'))), 
            by = c("moh_study_id" = "moh_study_id.hosp"), relationship = "many-to-many") %>%
  filter(ad_date - as.Date(sep_date.hosp) >= days(1) & 
           ad_date - as.Date(sep_date.hosp) <= days(365)) %>%
  group_by(dad_id) %>% 
  summarise(num_psyc_hosp_prev_year=n(), 
            tdays_psych_hosp_prev_year=sum(los.hosp)) %>%
  right_join(cohort, by = "dad_id") %>%
  mutate(num_psyc_hosp_prev_year = replace_na(num_psyc_hosp_prev_year, 0),
         tdays_psych_hosp_prev_year = replace_na(tdays_psych_hosp_prev_year, 0)) %>%
  select(dad_id,moh_study_id, sep_date, num_psyc_hosp_prev_year, tdays_psych_hosp_prev_year) %>% 
  ungroup()

# Save file
saveRDS(prior_psyc_hosp, "prior_psyc_hosp.rds") #update 06-26
  
################# Overdose episodes in the past ##########################
od_date <- read_delim("R:/DATA/2023-05-09_190103/POC2023-04-05/vw_od_date.csv.gz")

# in prior year
prior_od <- cohort %>% select(-od_date) %>%  
  left_join(od_date, by = "moh_study_id",relationship = "many-to-many") %>% 
  filter(ad_date - as.Date(od_date) >= days(1) & 
           ad_date - as.Date(od_date) <= days(365)) %>%
  group_by(dad_id) %>% 
  summarise(num_od_prev_year=n()) %>%
  right_join(cohort, by = "dad_id") %>%
  mutate(num_od_prev_year = replace_na(num_od_prev_year, 0)) %>%
  select(dad_id,moh_study_id, sep_date, num_od_prev_year) %>% 
  ungroup()
# Save file
saveRDS(prior_od, "prior_od.rds") #update 06-26

# in the past 5 years
prior_5_od <- cohort %>% select(-od_date) %>%  
  left_join(od_date, by = "moh_study_id",relationship = "many-to-many") %>% 
  filter(ad_date - as.Date(od_date) >= days(1) & 
           ad_date - as.Date(od_date) <= years(5)) %>%
  group_by(dad_id) %>% 
  summarise(num_od_5_year=n()) %>%
  right_join(cohort, by = "dad_id") %>%
  mutate(num_od_5_year = replace_na(num_od_5_year, 0)) %>%
  select(dad_id, num_od_5_year) %>% 
  ungroup()
saveRDS(prior_5_od, "prior_5_od.rds")

# overdose during index hosp
index_od <- cohort %>% select(-od_date) %>% 
  left_join(od_date, by = "moh_study_id",relationship = "many-to-many") %>% 
  filter(ad_date <= as.Date(od_date)  & 
           sep_date >= as.Date(od_date) ) %>%
  group_by(dad_id) %>% 
  summarise(num_od_index = n()) %>%
  right_join(cohort, by = "dad_id") %>%
  mutate(num_od_index = replace_na(num_od_index, 0)) %>%
  select(dad_id,moh_study_id, sep_date, num_od_index) %>% 
  ungroup()

index_od %>% count(num_od_index)

################# Age of first drug use ######################################
mha_client <- read_delim("R:/DATA/2023-05-09_190103/POC2023-04-05/vw_mha_client.csv.gz")

frst_sbstc_age <- 
  mha_client %>% 
    select(moh_study_id,frst_alcl_use_age_in_yrs,frst_mrjn_use_age_in_yrs,frst_oth_use_age_in_yrs) %>% #n = 84226
    filter(moh_study_id %in% cohort$moh_study_id) %>%  #nrow = 20397
    mutate(across(-1,function(x) as.numeric(gsub("year\\w*",'',x)))) %>%
    pivot_longer(cols = c('frst_alcl_use_age_in_yrs','frst_mrjn_use_age_in_yrs','frst_oth_use_age_in_yrs'),
                names_to = 'sbstc_source',
                values_to = 'frst_sbstc_age') %>%
    filter(!is.na(frst_sbstc_age)) %>% 
    group_by(moh_study_id) %>%
    #the earliest age among three substance use columns and among multiple records of a person
    summarise(min_frst_sbstc_age = min(frst_sbstc_age, na.rm = T)) %>%  #nrow = 4606 
    right_join(cohort %>% select(moh_study_id,dad_id),
              by = "moh_study_id") %>% 
    ungroup()

saveRDS(frst_sbstc_age, "frst_sbstc_age.rds") #update 06-08


################# Last substance use before index sep #########################
mha_substance <- read_delim("R:/DATA/2023-05-09_190103/POC2023-04-05/vw_mha_substance.csv.gz")
 
last_sbsct <- 
  mha_substance %>% 
    select(moh_study_id,sbstc_assmt_date,mha_sbstc) %>% 
    filter(moh_study_id %in% cohort$moh_study_id) %>% #nrow = 23,833
    left_join(cohort %>% select(dad_id,moh_study_id,sep_date),
              by = "moh_study_id",
              relationship = "many-to-many") %>%
    filter(sep_date - as.Date(sbstc_assmt_date) >= days(1)) %>% 
    group_by(moh_study_id,sep_date) %>% 
    slice_max(sbstc_assmt_date) %>% 
    ungroup() #total: 18,743, distinct study_id and sep_date: 12,070

saveRDS(last_sbsct,"last_sbstc_before_sep.rds") #update 06-26

############### prior DC-AMA ##################################################
hosp <- readRDS("cohort_hosp.rds")
sep_xw <- readxl::read_excel("R:/working/XW files/Staples Lab - DAD sep_disp xw_v4.xlsx",sheet = "StaplesLab_xw")
hosp <- 
  hosp %>% 
  left_join(sep_xw %>% select(sep_disp,SL_dc_disp), by =c("sep_disp.hosp"="sep_disp"))

prior_ama <- hosp %>% 
  # only keep the last hospitalization (no transfer to) 
  filter(hosp_to.hosp == 6 | hosp_to.hosp == 7 | is.na(hosp_to.hosp))  %>%
  filter(SL_dc_disp == "discharged BMA") %>% 
  inner_join(.,cohort, by = c("moh_study_id.hosp" = "moh_study_id"),relationship = "many-to-many") %>%
  # if the event happened in the interval of [ad_date - 5 year, ad_date - 1]
  filter(as.Date(ad_date) - as.Date(sep_date.hosp) <= years(5)
         & as.Date(ad_date) - as.Date(sep_date.hosp) >= days(1)) %>%
  group_by(dad_id) %>% 
  summarize(num_ama_5_year = n()) %>% 
  right_join(cohort, by = "dad_id") %>%
  mutate(num_ama_5_year = replace_na(num_ama_5_year,0),
         prior_ama = as.numeric(num_ama_5_year > 0)) %>% 
  select(dad_id, num_ama_5_year, prior_ama)

saveRDS(prior_ama,"prior_ama.rds") #update 07-26 

################# MSP visits for comorbidity #################################
msp <- readRDS("cohort_msp.rds")
# read in XW files
icd9_cm <- readxl::read_xlsx("R:/working/XW files/Staples Lab - PMH XW ICD-9-CM - 2023-07-25.xlsx")
selected_coms <- icd9_cm %>% filter(!pmh_numb %in% c(18.0,20.0,21.0,22.0)) %>% distinct(pmh_name) %>% pull()
#saveRDS(selected_coms,"selected_coms.rds")

# For the LHS, duplicate the variables used for the join; rename other variables (eg DIAGX_all, TDAYS) to make clear their provenance
msp <- msp %>% 
  unite("icd_all", contains("diag_cd"), na.rm = TRUE, sep = "_, _") %>% 
  mutate(icd_all = paste0("_", icd_all, "_")) %>% 
  select(moh_study_id, pmh_servdate = serv_date, pmh_servdate_merge = serv_date, pmh_icd_all = icd_all) %>% 
  setDT()
#saveRDS(msp,"msp_icd_all.rds")

# For the RHS, create the dates used for the join and duplicate these variables
#msp <- readRDS("msp_icd_all.rds")
coh_ids <- cohort %>%
  #start from ad_date [06-15]
  mutate(#addate_m1d = ad_date,
         addate_m1y = ad_date - years(5)) %>% 
  select(moh_study_id, dad_id, ad_date,sep_date, addate_m1y, start = addate_m1y, end = ad_date) %>%
  setDT()

# Complete a right_join (ie. keep all rows from coh_ids)
pmh_msp <- msp[coh_ids,
               on = .(moh_study_id == moh_study_id,
                      pmh_servdate_merge >= start,
                      pmh_servdate_merge <= end)] %>%
  select(moh_study_id, ad_date,sep_date, pmh_servdate, pmh_icd_all)

# For multiple visits on the same serv_date, combine all icd-codes
pmh_msp <- pmh_msp %>% 
  distinct() %>% 
  group_by(moh_study_id,ad_date,pmh_servdate) %>% 
  summarise(pmh_icd_all = str_c(pmh_icd_all, collapse = ", ")) %>% 
  ungroup()

#saveRDS(pmh_msp,"msp_icd_combined.rds")


# Map through the list of selected comorbidities
pmh_msp <- readRDS("msp_icd_combined.rds")
pmh_msp <- map(selected_coms, ~ if_else(str_detect(pmh_msp$pmh_icd_all, 
                                                   icd9_cm %>% 
                                                     filter(pmh_name == .x) %>% 
                                                     pull(icd9_code) %>% 
                                                     unique() %>% 
                                                     paste0("_",.,"_", collapse = "|")), #when icd9_code = 3040(opioid), it wouldn't be detected as 304(non-opioid)
                                        1, 
                                        0)) %>% 
  set_names(paste0(selected_coms, ".msp")) %>% 
  bind_cols(pmh_msp, .)


pmh_msp <-   
  pmh_msp %>%
    mutate(dm_nc_or_compl.msp = case_when(dm_nc.msp == 1 | dm_compl.msp == 1 ~ 1, TRUE ~ 0),
          mild_mod_sev_liver.msp = case_when(liver_mild.msp == 1 | liver_modsev.msp == 1 ~ 1, TRUE ~ 0),
          cancer_or_mc.msp = case_when(cancer.msp == 1 | mets.msp == 1 ~ 1, TRUE ~ 0),
          any_sbstcs.msp = case_when(alcohol.msp == 1 | drugs_all.msp == 1 | drugs_opioid.msp == 1 | drugs_nonopioid.msp == 1 ~ 1, TRUE ~ 0)) %>% 
    # sum each comorbidity for each patient
    group_by(moh_study_id,ad_date) %>%
    # capture the number of visits for a given comorbidity
    summarise(across(.cols = ends_with("msp"),
                     .fns = ~sum(.x, na.rm = TRUE))) %>%
    ungroup() %>%
    replace(is.na(.), 0)


saveRDS(pmh_msp,"pmh_msp.rds") #0727

################ HOSP admitions for comorbidity #############################
hosp <- readRDS("cohort_hosp.rds")
icd10_ca <- readxl::read_xlsx("R:/working/XW files/Staples Lab - PMH XW ICD-10-CA - 2023-07-25.xlsx")
icd10_ca <- icd10_ca %>% mutate(pmh_name = ifelse(pmh_name == "psychosis","psychotic",pmh_name ))
selected_coms <- readRDS("selected_coms.rds")

# For the LHS, duplicate the variables used for the join; rename other variables (eg DIAGX_all, TDAYS) to make clear their provenance
hosp <- hosp %>% 
  unite("diagx_all", contains("diagx"), na.rm = TRUE, sep = "_, _") %>% 
  mutate(diagx_all = paste0("_", diagx_all, "_")) %>% 
  dplyr::select(moh_study_id.hosp, pmh_sepdate = sep_date.hosp, pmh_sepdate_merge = sep_date.hosp, pmh_diagx_all = diagx_all, pmh_tdays = los.hosp) %>% 
  setDT()

# For the RHS, create the dates used for the join and duplicate these variables
coh_ids <- cohort %>%
  #start from ad_date [06-15]
  mutate(#addate_m1d = ad_date,
         addate_m1y = ad_date - years(5)) %>% 
  dplyr::select(moh_study_id, dad_id, ad_date,sep_date, addate_m1y, start = addate_m1y, end = ad_date) %>%
  setDT()

# Complete a right_join (ie. keep all rows from coh_ids)
pmh_dad <- hosp[coh_ids,
                on = .(moh_study_id.hosp == moh_study_id,
                       pmh_sepdate_merge >= start,
                       pmh_sepdate_merge <= end)] %>%
  dplyr::select(moh_study_id.hosp, dad_id, ad_date, pmh_sepdate, pmh_diagx_all, pmh_tdays)


# Map through the list of selected comorbidities
pmh_dad <- map(selected_coms, ~ if_else(str_detect(pmh_dad$pmh_diagx_all, 
                                                   icd10_ca %>% 
                                                        filter(pmh_name == .x) %>% 
                                                        pull(icd10_code) %>% 
                                                        unique() %>% 
                                                        paste0(., collapse = "|")), 
                                          1, 
                                          0)) %>% 
  set_names(paste0(selected_coms, ".dad")) %>% 
  bind_cols(pmh_dad, .)


pmh_dad <-   
  pmh_dad %>%
    mutate(dm_nc_or_compl.dad = case_when(dm_nc.dad == 1 | dm_compl.dad == 1 ~ 1, TRUE ~ 0),#diag = NA when there is no prior hosp
            mild_mod_sev_liver.dad = case_when(liver_mild.dad == 1 | liver_modsev.dad == 1 ~ 1, TRUE ~ 0),
            cancer_or_mc.dad = case_when(cancer.dad == 1 | mets.dad == 1 ~ 1, TRUE ~ 0),
            any_sbstcs.dad = case_when(alcohol.dad == 1 | drugs_all.dad == 1 | drugs_opioid.dad == 1 | drugs_nonopioid.dad == 1 ~ 1, TRUE ~ 0)) %>% 
    #if multiple comorbidities at one hosp, just count as 1
    group_by(moh_study_id.hosp,dad_id,ad_date,pmh_sepdate) %>% summarise(across(.cols = ends_with("dad"),
                                                                                function(x) {ifelse(sum(x) >= 1, 1, 0)})) %>%
    ungroup() %>%
    # sum each comorbidity for each patient
    group_by(moh_study_id.hosp,dad_id,ad_date) %>%
    # capture the number of visits for a given comorbidity
    summarise(across(.cols = ends_with("dad"),
                     .fns = ~sum(.x, na.rm = TRUE))) %>%
    ungroup() %>%
    replace(is.na(.), 0) %>%
    rename(moh_study_id = moh_study_id.hosp)
  
saveRDS(pmh_dad, "pmh_dad.rds") #0626

################# Charlson's score ##############################################
pmh_dad <- readRDS("pmh_dad.rds")
pmh_msp <- readRDS("pmh_msp.rds")
pmh_index <- readRDS("index_com.rds")

pmh_all <- pmh_dad %>% 
  full_join(pmh_msp) %>% 
  full_join(pmh_index)

# Create function to identify comorbidities in the year prior to admission (1 hosp or 2 physician services)
get_comorbidity_core <- function(com_list, df) {
  for (i in 1:length(com_list)) {
    name <- com_list[i]
    df[name] <- as.numeric(df[paste0(name,'.dad')] >= 1 | df[paste0(name,'.msp')] >= 2 | df[paste0(name,'.index')] == 1 )} 
  return(df)}

pmh_all <- pmh_all %>% 
  get_comorbidity_core(com_list = gsub(".dad","",colnames(pmh_dad)[-c(1:3)]), .) %>%
  mutate(
    # Charlson comorbidity score
    cci = (mi*1 + chf*1 + pvd*1 + cvd*1 + dem*1 + copd*1 + rheum*1 + pud*1 + liver_mild*1 + dm_nc*1 + 
             dm_compl*2 + paraplegia*2 + renal*2 + cancer*2 + liver_modsev*3 + mets*6 + hiv*6),
    cci_cat = if_else(cci>=2, 1, 0) %>% factor(., levels = c(0, 1)),
    cci_cat5 = if_else(cci>=5, 1, 0) %>% factor(., levels = c(0, 1))) 

saveRDS(pmh_all, "pmh_all.rds") #06-27

###################  Homeless history in past 5 years  #########################
cohort <- readRDS("dad_cohort v2.rds")
# homeless hist mentioned in mha
mha_dsm_5 <- read_delim("R:/DATA/2023-05-09_190103/POC2023-04-05/vw_mha_dsm_5.csv.gz")
homeless_mha <- 
  cohort %>% 
  select(dad_id,moh_study_id,ad_date) %>% 
  left_join(mha_dsm_5,
            by = "moh_study_id",
            relationship = "many-to-many") %>% 
  filter(as.Date(ad_date) - as.Date(enrl_assmt_date) <= years(5) & as.Date(ad_date) - as.Date(enrl_assmt_date) >= 0) %>%
  mutate(homeless = as.numeric(if_any(paste0("dsm_5_enrl_dx_cd_", 1:3), ~ . %in% c("Z59.0","Z59.1")))) %>%
  group_by(dad_id,moh_study_id,ad_date) %>% 
  summarise(homeless.mha = sum(homeless)) %>% 
  right_join(cohort %>% select(dad_id,moh_study_id, ad_date), by = c("dad_id","moh_study_id","ad_date"))  %>% 
  mutate(homeless.mha = replace_na(homeless.mha, 0)) %>% 
  ungroup()

# homeless history: if >= 1 hospitalizations or >= 1 mha
homeless_hist <- 
  homeless_mha %>% 
  full_join(readRDS("pmh_dad.rds") %>% select(dad_id,homeless.dad)) %>% 
  full_join(readRDS("index_com.rds") %>% select(dad_id,homeless.index)) %>% 
  mutate(homeless_hist = as.numeric(homeless.mha >= 1 | homeless.dad >= 1 | homeless.index == 1))

saveRDS(homeless_hist,"homeless_hist.rds") 

################### Intravenous drug use in past 3 year and in past 6 year ########################
source("R:/working/AMA-OD_coh/NH/code/Table A/AMA-OD Get n year pmh.R")
pmh_ivdu_dad <- get_n_year_dad_pmh(n = 3, selected_coms = "ivdu")
pmh_ivdu_msp <- get_n_year_msp_pmh(n = 3, selected_coms = "ivdu")

ivdu_hist <- pmh_ivdu_dad %>% 
  full_join(pmh_ivdu_msp) %>% 
  full_join(readRDS("oat_3y.rds")) %>% 
  full_join(readRDS("index_com.rds")) %>% 
  mutate(ivdu_hist = as.numeric(ivdu.dad >= 1 | ivdu.msp >= 2 | geq1_oat_3y == 1 | ivdu.index == 1)) %>% 
  distinct() %>% 
  select(dad_id,ivdu_hist)
saveRDS(ivdu_hist,"ivdu_hist.rds")

ivdu_hist_5y <- readRDS("pmh_dad.rds") %>% select(dad_id,moh_study_id,ad_date,ivdu.dad) %>% 
  full_join(readRDS("pmh_msp.rds") %>% select(moh_study_id,ad_date,ivdu.msp)) %>% 
  full_join(readRDS("oat_5y.rds"),relationship = "many-to-many") %>% 
  full_join(readRDS("index_com.rds")) %>% 
  mutate(ivdu_hist = as.numeric(ivdu.dad >= 1 | ivdu.msp >= 2 | geq1_oat_5y == 1 | ivdu.index == 1)) %>% 
  distinct() %>% 
  select(dad_id,ivdu_hist)
saveRDS(ivdu_hist_5y,"ivdu_hist_5y.rds")

################ Combine all ##################################################

#last_sbstc_before_sep <- readRDS("last_sbstc_before_sep.rds")

medical_hist_covariates <- readRDS("prior_hosp.rds") %>%
  full_join(readRDS("prior_psyc_hosp.rds")) %>% 
  full_join(readRDS("prior_od.rds")) %>% 
  full_join(readRDS("homeless_hist.rds") %>% dplyr:: select(-homeless.mha,-homeless.dad,-homeless.index))%>% 
  full_join(readRDS("prior_clinic.rds")) %>%
  left_join(readRDS("frst_sbstc_age.rds")) %>% 
  #left_join(last_sbstc_before_sep) %>% 
  full_join(readRDS("pmh_all.rds") %>% dplyr:: select(- ends_with("dad"),- ends_with("msp"))) %>% 
  full_join(readRDS("pmh_nacrs.rds"))

saveRDS(medical_hist_covariates, "medical_hist_covariates.rds")  
  


