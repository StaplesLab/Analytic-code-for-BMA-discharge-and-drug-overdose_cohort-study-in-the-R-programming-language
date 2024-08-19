##########################################################################
# This work is licensed under CC BY-NC-SA 4.0. To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/4.0/
# Suggested citation: Hu X, Yu Y, Daly-Grafstein D, Khan M, Erdelyi S, Staples JA. AMA-OD_coh create variable - details of hospitalization. 2023 Jul 31. Retrieved from: ***LINK***
# Create details of hosp variables
# Author: Xiao (Nicole) Hu
# Date: 2023-06-08
# Update: 2023-07-31
##########################################################################
# libraries
source("R:/working/AMA-OD_coh/NH/code/AMA-OD - 0 Packages and working directory.R")

# set working dir
setwd("R:/working/AMA-OD_coh/NH/results/Table A")

# Table A cohort
cohort <- readRDS("dad_cohort.rds")

######## Most Responsible Diagnosis for index hospitalization ##################

ddgrp_xw <- readxl::read_xlsx("R:/working/XW files/AMA-OD - DIAGX1 to ddgrp XW - 2023-06-15_NH.xlsx")
selected_ddgrp <- ddgrp_xw %>% select(ddgrp3) %>% distinct() %>% filter(ddgrp3 != "Missing") %>% pull()

mrd <- map(selected_ddgrp, ~ if_else(str_detect(cohort$diagx_1, 
                                                    ddgrp_xw %>% 
                                                     filter(ddgrp3 == .x) %>% 
                                                     pull(diagx1.dad) %>% 
                                                     unique() %>% 
                                                     paste0(., collapse = "|")), 
                                        1, 
                                        0)) %>% 
  set_names(paste0(selected_ddgrp)) %>% 
  bind_cols(cohort %>% select(dad_id,diagx_1),.) %>% 
  mutate(other_sbstc = as.numeric(`Alcohol misuse` == 1 | `Non-alcohol, non-opioid substance misuse` == 1),
         ed_or_om = as.numeric(Endocarditis == 1 | Osteomyelitis == 1),
         other_MRD = as.numeric(other_sbstc == 0 & ed_or_om == 0 & `Opioid misuse` == 0))
  

saveRDS(mrd, "mrd.rds")



###### index comorbidity ######
selected_coms <- readRDS("selected_coms.rds")
icd10_ca <- readxl::read_xlsx("R:/working/XW files/Staples Lab - PMH XW ICD-10-CA - 2023-07-25.xlsx")
icd10_ca <- icd10_ca %>% mutate(pmh_name = ifelse(pmh_name == "psychosis","psychotic",pmh_name ))
cohort <- cohort %>% 
  unite("diagx_all", contains("diagx"), na.rm = TRUE, sep = "_, _") %>% 
  mutate(diagx_all = paste0("_", diagx_all, "_"))

pmh_index <- map(selected_coms, ~ if_else(str_detect(cohort$diagx_all, 
                                                     icd10_ca %>% 
                                                       filter(pmh_name == .x) %>% 
                                                       pull(icd10_code) %>% 
                                                       unique() %>% 
                                                       paste0(., collapse = "|")), 
                                          1, 
                                          0)) %>% 
  set_names(paste0(selected_coms, ".index")) %>% 
  bind_cols(cohort %>%
              select(dad_id,moh_study_id,ad_date), .) %>% 
  mutate(dm_nc_or_compl.index = case_when(dm_nc.index == 1 | dm_compl.index == 1 ~ 1, TRUE ~ 0),#diag = NA when there is no prior hosp
         mild_mod_sev_liver.index= case_when(liver_mild.index == 1 | liver_modsev.index == 1 ~ 1, TRUE ~ 0),
         cancer_or_mc.index = case_when(cancer.index == 1 | mets.index == 1 ~ 1, TRUE ~ 0),
         any_sbstcs.index = case_when(alcohol.index== 1 | drugs_all.index== 1 | drugs_opioid.index == 1 | drugs_nonopioid.index == 1 ~ 1, TRUE ~ 0)) %>% 
  saveRDS("index_com.rds")


############ procedures & complication ##########################

#procedure and complication indicator
dad <- read_delim("R:/DATA/2023-05-09_190103/POC2023-04-05/vw_dad.csv.gz")
dad <- dad %>% filter(dad_id %in% cohort$dad_id)
pro_com <- dad %>% 
  #mutate(procedures = as.numeric(if_any(paste0("icode_", 2:20), str_detect(.,"^1")))) %>% 
  mutate(complication = as.numeric(if_any(paste0("dtypx_", 2:25), ~ . %in% c("2")))) %>% 
  mutate(procedure = as.numeric(if_any(paste0("icode_", 1:20),~str_detect(.,"^1")))) %>% 
  mutate(addiction = as.numeric(if_any(paste0("icode_", 1:20),~str_detect(.,"^6")))) %>% 
  mutate(procedure = replace_na(procedure,0)) %>% 
  select(dad_id,procedure,complication,addiction,starts_with("icode")) 


pro_com %>% count(addiction)
pro_com %>% filter(addiction == 1)


#procedure duration 
dad_icode <- read_delim("R:/DATA/2023-05-09_190103/POC2023-04-05/vw_drv_dad_icode_long.csv.gz")
dad_icode <- dad_icode %>% filter(dad_id %in% cohort$dad_id)

#test if results are the same with original dataset
# dad_icode %>% 
#   mutate(procedure = as.numeric(str_detect(icode,"^1"))) %>%
#   filter(procedure == 1) %>% 
#   select(dad_id) %>% n_distinct()
# sum(pro_com$procedure)

ttl_duration <- 
dad_icode %>% 
  mutate(procedure = as.numeric(str_detect(icode,"^1"))) %>% 
  mutate(duration = ifelse(procedure == 1,(iend_dt_tm - istart_dt_tm)/60,0)) %>% 
  group_by(dad_id) %>% 
  summarize(ttl_duration = sum(duration,na.rm = T)) %>% 
  ungroup() %>% 
  mutate(ttl_duration = replace_na(ttl_duration,0)) %>% 
  select(dad_id, ttl_duration)

#note: 
# sum up all duration of experiencing procedures
# unit = minutes

#procedure == 1, ttl_duration == 0: n = 17,408
#1. some icodes have start time but no end time
#2. some icodes have same start time and end time
sub <- dad_icode %>%
  filter(dad_id %in% (ttl_duration %>% filter(ttl_duration == 0) %>% pull(dad_id))) %>%
  filter(str_detect(icode,"^1")) %>%
  filter(!is.na(istart_dt_tm))

#combine all
pro_com <- 
pro_com %>% 
  left_join(ttl_duration) %>% 
  mutate(ttl_duration = replace_na(ttl_duration,0))

saveRDS(pro_com,"pro_com.rds")


########## drug toxicity #########################
episode <- read_delim("R:/DATA/2023-05-09_190103/POC2023-04-05/vw_episode_master.csv.gz")
episode <- episode %>% filter(od_flag == 1)
drug_toxicity <- episode %>% mutate(ym = substr(start_dt_tm,1,7)) %>% count(ym)
write.csv(drug_toxicity,"R:/working/AMA-OD_coh/NH/results/Table A/drug_toxicity.csv",row.names = F)

drug_toxicity <- cohort %>% 
  mutate(ym = substr(sep_date,1,7)) %>% 
  left_join(drug_toxicity, by = "ym") %>% 
  select(dad_id, drug_toxicity = n)
saveRDS(drug_toxicity,"drug_toxicity.rds")
