#####################################################
# This work is licensed under CC BY-NC-SA 4.0. To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/4.0/
# Suggested citation: Hu X, Yu Y, Khan M, Daly-Grafstein D, Erdelyi S, Staples JA. AMA-OD_coh primary analysis create outcome variables. 2023 Nov 7. Retrieved from: ***LINK*** 
# Filename: "AMA-OD 4a Primary Analysis - get response variables v1.R"
# Create Cox PH primary outcome and secondary outcome
# Author: Xiao (Nicole) Hu 
# Date: 2023-07-04
# Updated: 2023-11-07
#####################################################
# libraries
source("R:/working/AMA-OD_coh/NH/code/AMA-OD - 0 Packages and working directory.R")

# set working dir
setwd("R:/working/AMA-OD_coh/NH/results/Table B")

# read cohort
cohort = readRDS("R:/working/AMA-OD_coh/NH/results/Table A/coh_all_v2.rds")

######## 1. od events (fatal/non_fatal) ###########################################
# event type: 0.censoring, 1.od, 2.non od death, 3.non-od readmission 
# censoring date: readmission happens before od -> readmission date
#                 readmission happens when no od -> readmission date
#                 if there is no readmission and od, and died -> death date

cohort <- cohort %>% mutate(od_event.type = case_when(frst_readmit_adddate < od_date ~ 3, #readmission before od
                                                      od_status == 1 ~ 1, #od 
                                                      readmit_30 == 1 ~ 3, #readmission when no od
                                                      death_30 == 1 & od_fatal_30 == 0 ~ 2, #death due to non-od reason
                                                      TRUE ~ 0),
                            od_survival.time = case_when(od_event.type == 2 ~ as.numeric(death_date - sep_date),
                                                         od_event.type == 3 ~ as.numeric(frst_readmit_adddate - sep_date),
                                                         TRUE ~ od_stime),
                            od_event = as.numeric(od_event.type == 1))

# cohort %>% count(od_event.type,od_status)
# cohort %>% count(od_status)

######### 2. fatal od #############################################################
# event type: 0.censoring, 1.od death, 2.non-od death  
# censoring date: death due to reasons other than fatal od -> death date

cohort <- cohort %>% mutate(fod_event.type = case_when(od_fatal_30 == 1 ~ 1,
                                                       death_30 == 1 ~ 2,
                                                       TRUE ~ 0),
                            fod_survival.time = case_when(fod_event.type == 2 ~ as.numeric(death_date - sep_date),
                                                          TRUE ~ od_stime),
                            fod_event = as.numeric(fod_event.type == 1))



# cohort %>% count(fod_event.type)
# cohort %>% count(od_fatal_30)

#############3. nonfatal od ########################################################
# event type: 0.censoring, 1.non-fatal od, 2.all-cause death 3.non-od readmission
# censoring date: readmission happens before od -> readmission date
#                 readmission happens when no od -> readmission date
#                 if there is no readmission and od, and died -> death date

cohort <- cohort %>% mutate(nfod_event.type = case_when( frst_readmit_adddate < od_non_fatal_30_date ~ 3, #readmission before od 
                                                         od_non_fatal_30 == 1 ~ 1, #if there is a non_fatal od
                                                         readmit_30 == 1 ~ 3, #readmission when no od
                                                         death_30 == 1 ~ 2, #non-od death
                                                         TRUE ~ 0 ),
                            nfod_survival.time = case_when(nfod_event.type == 2 ~ as.numeric(death_date - sep_date),
                                                           nfod_event.type == 3 ~ as.numeric(frst_readmit_adddate - sep_date),
                                                           TRUE ~ od_stime),
                            nfod_event = as.numeric(nfod_event.type == 1))

# cohort %>% count(nfod_event.type,od_non_fatal_30)
# cohort %>% count(od_non_fatal_30)

#########  4. unplanned readmission ###############################################
# event type: 0.censoring, 1.readmission, 2.all cause death

cohort <- cohort %>% mutate(uread_event.type = case_when(readmit_U_30 == 1 ~ 1,
                                                         death_30 == 1 ~ 2,
                                                         TRUE ~ 0),
                            uread_survival.time = case_when(uread_event.type == 1 ~ as.numeric(readmit_U_addate - sep_date),
                                                            uread_event.type == 2 ~ as.numeric(death_date - sep_date),
                                                            TRUE ~ pmin(30,as.Date("2020-01-01") - as.Date(sep_date))),
                            uread_event = as.numeric(uread_event.type == 1))

#cohort %>% count(uread_event.type)
#cohort %>% count(readmit_U_30)

########## 5. ed ##################################################################
# event type: 0.censoring, 1.ed visit, 2.all cause death, 3. all cause readmission
# censoring date: readmission happens before ed -> readmission date
#                 readmission happens when no ed -> readmission date
#                 if there is no readmission and no ed, and died -> death date

cohort <- cohort %>% mutate(ed_event.type = case_when( frst_readmit_adddate < ed_date ~ 3,
                                                       ed_30 == 1 ~ 1,
                                                       readmit_30 == 1 ~ 3,
                                                       death_30 == 1 ~ 2,
                                                       TRUE ~ 0),
                            ed_survival.time = case_when(ed_event.type == 1 ~ as.numeric(ed_date - sep_date),
                                                         ed_event.type == 2 ~ as.numeric(death_date - sep_date),
                                                         ed_event.type == 3 ~ as.numeric(frst_readmit_adddate - sep_date),
                                                         TRUE ~ pmin(30,as.Date("2020-01-01") - as.Date(sep_date))),
                            ed_event = as.numeric(ed_event.type == 1))

# cohort %>% count(ed_event.type,ed_30)
# cohort %>% count(ed_30)

########## 6. all-cause mortality ##############################################
cohort <- cohort %>% mutate(death_survival.time = ifelse(death_30 == 1,
                                                         death_date - sep_date,
                                                         pmin(30,as.Date("2020-01-01") - as.Date(sep_date))))


######### Events on the index day ##############################################
# Assumption o f Cox PH: 100% survival at t0
# We coded events happened on the index day as they happened on day 0.5
# The choice of day 0.5 or day 0.1 or day 0.01 does not matter
# Cox model considers all event times in order, starting with very frist event time and proceeding up through each subsequent event time

cohort <- cohort %>% mutate(death_survival.time = ifelse(death_survival.time == 0, 0.5, death_survival.time),
                            uread_survival.time = ifelse(uread_survival.time == 0, 0.5, uread_survival.time),
                            ed_survival.time = ifelse(ed_survival.time == 0, 0.5, ed_survival.time))



######### categorical variales ###############################################
cohort <- cohort %>% 
  mutate_at(vars(age_group,pop_density_class,male_sex,homeless,msp_prem,
                 alcohol, drugs_opioid,drugs_nonopioid,num_od_5_year_cat,
                 psych,cci_cat,oat_active_ad,geq1_oat, geq1_benzo, geq1_opioid, geq1_antipsyc,
                 if_icu,`Opioid misuse`,other_sbstc,ed_or_om, procedure, complication,
                 ivdu_hist,OUD,prior_ama, amb_arrive,ed_entry,
                 Gastroenteritis,HIV,Cancer,Diabetes,Psychiatric, `Cardiac disease`,Stroke,
                 Pneumonia,`Obstructive lung disease`,`Biliary tract disease`,Cellulitis,`Kidney injury`,
                 Urolithiasis), as.factor) 

cohort$dc <- relevel(as.factor(cohort$dc), ref = "wa")
cohort$pop_density_class <- relevel(cohort$pop_density_class, ref = "Rural")
cohort$age_group <- relevel(cohort$age_group, ref = "50+")

#SAVE
saveRDS(cohort,"R:/working/AMA-OD_coh/NH/results/Table B/cohort_cox.rds")
