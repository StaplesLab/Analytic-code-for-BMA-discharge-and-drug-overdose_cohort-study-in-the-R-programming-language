##########################################################################
# This work is licensed under CC BY-NC-SA 4.0. To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/4.0/
# AMA-OD_coh Variable Creation - outcomes
# Author: Nicole Hu
# Date: 2023-06-01
# Update: 2023-11-07
# Suggested citation: Hu X, Yu Y, Daly-Grafstein D, Khan M, Staples JA. AMA-OD_coh variable creation - outcomes. 2023 Nov 7. Retrieved from: ***LINK***
###########################################################################

# Create outcome variables:

## Non-fatal or fatal drug overdose (primary outcome)
## Non-fatal drug overdose (secondary outcome)
## Fatal drug overdose (secondary outcome)
## Hospital readmission 
## Unplanned/urgent  (secondary outcome)
## Elective (censoring event)
## ED visits (all cause, overdose & non-overdose) (secondary outcome)
## Death from any cause  (secondary outcome)

##########################################################################

##### edit file paths for source, wd, data dictionary, data files


# libraries
source("R:/working/AMA-OD_coh/NH/code/AMA-OD - 0 Packages and working directory.R")
# Results start from Line 42

####################### read in overdose episode file ##########################
# set working dir
setwd("R:/working/AMA-OD_coh/NH/results/Table A")
#ee_xwalk <- read_delim("R:/DATA/2023-05-09_190103/POC2023-04-05/vw_episode_encounter_xwalk.csv.gz")
od_date <- read_delim("R:/DATA/2023-05-09_190103/POC2023-04-05/vw_od_date.csv.gz")
dad_demo_cohort <- readRDS("dad_demo_cohort.rds")

################### primary outcome for survival analysis ######################
######### Non-fatal or fatal drug overdose (primary outcome) ###################

#dad_demo_cohort <- dad_demo_cohort %>% dplyr :: select(-c(od_stime,od_status,od_date,od_fatal))

dad_demo_cohort<- dad_demo_cohort %>% 
  dplyr :: select(moh_study_id,dad_id,sep_date) %>% 
  #get od events for each individual
  left_join(od_date %>% dplyr :: select(moh_study_id,od_date,od_fatal), 
            #od_date: date first od after dc within first 30 days
            #od_fatal: if the first od is a fatal od
                              by = "moh_study_id", 
                              relationship = "many-to-many") %>% 
  #only keep od events with first 30 days after dc
  filter(od_date - as.Date(sep_date) >= days(1) & 
           od_date - as.Date(sep_date) <= days(30)) %>%  
  group_by(dad_id) %>% 
  #get first od event for each hospitalization
  slice_min(od_date) %>% 
  ungroup() %>% 
  select(dad_id,od_date,sep_date,frst_od_fatal = od_fatal) %>% 
  distinct() %>% 
  #od-free survival time
  mutate(od_stime = od_date - sep_date) %>%
  #if observed = 1, censored = 0
  mutate(od_status = 1) %>%
  right_join(dad_demo_cohort) %>%
  #if there is no od event happened till the end of follow-up period,
  #use time difference btw sep_date and the end of study or 30 days as od-free survival time, whichever is shorter
  #and record it as censored
  mutate(od_stime = ifelse(is.na(od_stime),pmin(30,as.Date("2020-01-01") - as.Date(sep_date)),od_stime),
         od_status = replace_na(od_status,0)) 


# total number of od events within 30 days after dc
# If there is od event happened within 30 days after dc, then how many times does that happen?
dad_demo_cohort <-  
  left_join(dad_demo_cohort %>%  select(moh_study_id,dad_id,sep_date),
            od_date,
            by = "moh_study_id",
            relationship = "many-to-many") %>% 
  filter(od_date - as.Date(sep_date) >= days(1) & 
           od_date - as.Date(sep_date) <= days(30)) %>%
  group_by(dad_id) %>% 
  summarise(sep30_total_od = n()) %>% 
  ungroup() %>% 
  right_join(dad_demo_cohort, by = "dad_id")  %>% 
  mutate(sep30_total_od  = replace_na(sep30_total_od, 0))



################### secondary outcome for survival analysis ####################


  ############### Fatal drug overdose (secondary outcome) ######################

# if there is a fatal od within 30 days after dc
dad_demo_cohort <- 
  left_join(dad_demo_cohort %>%  select(moh_study_id,dad_id,sep_date),
            od_date,
            by = "moh_study_id",
            relationship = "many-to-many") %>% 
  filter(od_date - as.Date(sep_date) >= days(1) & 
           od_date - as.Date(sep_date) <= days(30)) %>%
  #get all fatal od within 30 days
  filter(od_fatal == 1) %>%
  mutate(od_fatal_30 = od_fatal) %>% 
  #get the date of fatal od
  select(dad_id, od_fatal_30, od_fatal_30_date = od_date) %>% 
  distinct() %>% 
  right_join(dad_demo_cohort, by = "dad_id") %>% 
  mutate(od_fatal_30 = ifelse(is.na(od_fatal_30),0,1))


  ############## Non-fatal drug overdose (secondary outcome) ###################

# if there is a non-fatal od within 30 days after dc
dad_demo_cohort <- dad_demo_cohort %>% 
  #if the first overdose is not fatal, then the date is the first non-fatal overdose date
  #if the first overdose is fatal, no possible od any more
  mutate(od_non_fatal_30 = as.numeric(od_status == 1 & frst_od_fatal == 0),
         od_non_fatal_30_date = ifelse(od_non_fatal_30 == 1, od_date,NA))



  ######################### Hospital readmission ###############################
dad <- read_delim("R:/DATA/2023-05-09_190103/POC2023-04-05/vw_dad.csv.gz")

# get all hospitalizations of cohort (last hosp)
hosp <- dad %>% 
  select(moh_study_id,readmit_addate = ad_dt_tm, hosp_to,hosp_from, readmit_admit = admit) %>% 
  #filter(hosp_to == 6 | hosp_to == 7 | is.na(hosp_to))  %>% 
  setDT()

# Unplanned/urgent readmission within 30 days (secondary outcome)
dad_demo_cohort <- dad_demo_cohort %>% 
    select(dad_id, moh_study_id, sep_date) %>% 
    inner_join(
      hosp %>% 
        filter(readmit_admit == "U") %>%
        dplyr::select(moh_study_id, readmit_U_addate = readmit_addate),
      by = "moh_study_id",
      relationship = "many-to-many") %>% 
    filter(as.Date(readmit_U_addate) >= as.Date(sep_date) & 
             as.Date(readmit_U_addate) <= as.Date(sep_date) + days(30)) %>%
    group_by(dad_id) %>% 
    slice_min(readmit_U_addate) %>% 
    ungroup() %>%
    distinct() %>% 
    select(-sep_date, - moh_study_id) %>% 
    right_join(dad_demo_cohort, by="dad_id") %>%
    mutate(readmit_U_30 = as.numeric(!is.na(readmit_U_addate))) %>% 
    mutate(readmit_U_addate  = as.Date(readmit_U_addate)) 

# Elective readmission within 30 days
dad_demo_cohort <-  dad_demo_cohort %>% 
  select(dad_id, moh_study_id, sep_date) %>% 
  inner_join(
    hosp %>% 
      filter(readmit_admit == "L") %>%
      dplyr::select(moh_study_id, readmit_L_addate = readmit_addate),
    by = "moh_study_id",
    relationship = "many-to-many") %>% 
  filter(as.Date(readmit_L_addate) >= as.Date(sep_date) & 
           as.Date(readmit_L_addate) <= as.Date(sep_date) + days(30)) %>%
  group_by(dad_id) %>% 
  slice_min(readmit_L_addate) %>% 
  ungroup() %>% 
  distinct() %>% 
  select(dad_id,readmit_L_addate) %>% 
  right_join(dad_demo_cohort, by="dad_id") %>%
  mutate(readmit_L_30 = as.numeric(!is.na(readmit_L_addate))) %>% 
  mutate(readmit_L_addate = as.Date(readmit_L_addate))

#first re-admission (competing event) [earliest readmission date for each hospitalization]
dad_demo_cohort <-  dad_demo_cohort %>% 
  select(dad_id, moh_study_id, sep_date) %>% 
  inner_join(
    hosp %>% 
      dplyr::select(moh_study_id, readmit_addate,readmit_admit),
    by = "moh_study_id",
    relationship = "many-to-many") %>% 
  filter(as.Date(readmit_addate) >= as.Date(sep_date)) %>%
  group_by(dad_id) %>% 
  #first readmission date [no follow-up period restriction which is easier for further sensitivity analyses using follow-up period with different lengths]
  slice_min(readmit_addate) %>% 
  ungroup() %>% 
  distinct() %>% 
  select(dad_id,frst_readmit_adddate =  readmit_addate,frst_readmit_admit = readmit_admit) %>% 
  right_join(dad_demo_cohort, by="dad_id") %>%
  # indicator of whether readmission within 30 days
  mutate(readmit_30 = as.numeric(as.Date(frst_readmit_adddate) <= as.Date(sep_date) + days(30))) %>% 
  mutate(frst_readmit_adddate = as.Date(frst_readmit_adddate)) %>% 
  mutate(frst_readmit_admit = replace_na(frst_readmit_admit,"No Readmition")) %>% 
  mutate(readmit_30 = replace_na(readmit_30,0))


dad_demo_cohort %>% summarise(prop.readmitted = mean(!is.na(readmit_L_addate)))
dad_demo_cohort %>% summarise(prop.readmitted = mean(!is.na(readmit_U_addate)))
dad_demo_cohort %>% summarise(prop.readmitted = mean(!is.na(frst_readmit_adddate)))
dad_demo_cohort %>% summarise(prop.readmitted = mean(readmit_30 == 1))


#hist
dad_demo_cohort %>% 
  filter(!is.na(readmit_U_addate)) %>% 
  mutate(dur = interval(sep_date, readmit_U_addate) %/% days(1)) %>% 
  ggplot() + geom_histogram(aes(x = dur)) + coord_cartesian(xlim=c(0,45)) + theme_stapleslab

  ####### ED visits (all cause) (secondary outcome) ############################

# if there is a ed-visit within 30 days after dc
nacrs <- read_delim("R:/DATA/2023-05-09_190103/POC2023-04-05/vw_nacrs.csv.gz")

dad_demo_cohort <- dad_demo_cohort %>%
  select(dad_id,moh_study_id,sep_date) %>% 
  left_join(nacrs %>% filter(ed_visit == 1 | is.na(ed_visit)) %>% 
              select(moh_study_id,reg_date) %>% 
              mutate(ed_date = reg_date) %>% 
              select(-reg_date), 
            by = "moh_study_id",
            relationship = "many-to-many") %>% 
  filter(ed_date >= as.Date(sep_date)  & 
           ed_date - as.Date(sep_date) <= days(30)) %>% 
  group_by(dad_id) %>% 
  slice_min(ed_date) %>% 
  ungroup() %>% 
  distinct() %>% 
  select(-sep_date, - moh_study_id) %>% 
  right_join(dad_demo_cohort, by="dad_id") %>%
  mutate(ed_30 = as.numeric(!is.na(ed_date)))

######################### Death from any cause  (secondary outcome) ############

# if there is a death within 30 days after dc
dad_demo_cohort <- 
  dad_demo_cohort %>% 
    mutate(death_30 = as.numeric(death_date  >=  as.Date(sep_date) & 
                                 death_date - as.Date(sep_date) <= days(30))) %>% 
    mutate(death_30 = replace_na(death_30,0)) 


#done
# select columns (remove unused columns for now)
dad_demo_cohort <- 
  dad_demo_cohort %>% select(-starts_with("icode"),
                             -starts_with("istart"), -starts_with("iend"),
                             -starts_with("dtypx"), -starts_with("to_keep"),
                             -matching_id, -case_control_flag)


saveRDS(dad_demo_cohort,"dad_cohort.rds")
####################### Results ################################################

dad_demo_cohort <- readRDS("dad_cohort.rds")

# overdose in the first 30 days after dc-ama vs. dc-wa
exposure_od <- 
dad_demo_cohort %>% 
  group_by(dc,od_status) %>% 
  summarise(n = n_distinct(moh_study_id),N = n())
exposure_od

# contingency table based on hospitalizations
con = table(dad_demo_cohort$dc,dad_demo_cohort$od_status)
con
prop.table(con,margin = 1) #DA-AMA is 9.2 times more likely to have od events in the first 30 days after dc
chisq.test(con)


# average of od events within 30 days after dc
dad_demo_cohort %>% 
  filter(sep30_total_od > 0) %>% 
  group_by(dc) %>% 
  summarise(mean = mean(sep30_total_od))

# fatal od in dc-ama vs. dc-wa
con_fatal <- table(dad_demo_cohort$dc,dad_demo_cohort$od_fatal_30)
con_fatal
prop.table(con_fatal,margin = 1) #DC-AMA is 6 times more likely to have fatal od in the first 30 days after dc


### save results #####
wb <- loadWorkbook("AMA-OD_coh - cohort creation_v2.xlsx")
#addWorksheet(wb, "exposure-od_30")
writeData(wb,"exposure-od_30", exposure_od)
saveWorkbook(wb, "AMA-OD_coh - cohort creation_v2.xlsx",overwrite = T)
