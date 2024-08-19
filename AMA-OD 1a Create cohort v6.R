##########################################################################
# This work is licensed under CC BY-NC-SA 4.0. To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/4.0/
# Suggested citation: Hu X, Khan M, Yu Y, Daly-Grafstein D, Staples JA. AMA-OD_coh cohort creation. 2023 June 26. Retrieved from: ***LINK***
# Cohort Creation for Table 1 - filtered on: U; 18<age<110; los>0; sepdisp: 4,5,6,12; no transfer to; not preg; admitted btw 2015-2019
# Author: Xiao Nicole Hu
# Date: 2023-05-31
# Update: 2023-06-26
# v6: use sep_date to filter eligible hosps, use sep_xw to solve the issue with sep_disp

##########################################################################

##### edit file paths for source, wd, data dictionary, data files

# libraries 
source("R:/working/AMA-OD_coh/NH/code/AMA-OD - 0 Packages and working directory.R")

## Results start from Line 162

#####################################################################
# set working dir
setwd("R:/working/AMA-OD_coh/NH/results/Table A")
# read in dad data
dad <- read_delim("R:/DATA/2023-05-09_190103/POC2023-04-05/vw_dad.csv.gz")
# read in demographic data
person_xwalk <- read_delim("R:/DATA/2023-05-09_190103/POC2023-04-05/vw_person_xwalk.csv.gz")

################ create length of stay variable #####################

# t_days = 1 when sep_date = ad_date
dad %>% filter(t_days == 1, sep_date == ad_date) %>% select(t_days,sep_date, ad_date) %>% sample_n(5)

# create variable of length of stay 
dad <- dad %>%
  mutate(sep_date = ymd(sep_date), 
         ad_date = ymd(ad_date), 
         tlos = as.period(ad_date %--% sep_date),
         los = as.duration(tlos) / ddays(1))
# check
dad %>% filter(los == 1, sep_date == ad_date)

################ apply exclusion criteria ###########################

# link with demo and create variable of age at index discharge
dad_demo <- dad %>% 
  left_join(person_xwalk, by = "moh_study_id") %>% 
  mutate(sep_age = (as.period(interval(birth_date, sep_date)))@year)

# filter out the hospitalizations of reference cohort
dad_demo <- dad_demo %>% filter(reference== 1)
#saveRDS(dad_demo,"reference_dad.rds")

cbind(nrow(dad_demo), dad_demo$moh_study_id %>% n_distinct()) #1548898 507118

#sep_disp xw
sep_xw <- readxl::read_excel("R:/working/XW files/Staples Lab - DAD sep_disp xw_v4.xlsx",sheet = "StaplesLab_xw")
dad_demo <- 
  dad_demo %>% 
  left_join(sep_xw %>% select(sep_disp,SL_dc_disp), by = "sep_disp")

# create a dataframe for filter criteria (indicators: 1 for exclusion and 0 for inclusion)
dad_sub <- 
  dad_demo %>% 
    mutate(
          #elective hosp (admit != "U")
          elective = ifelse(admit == "U", 0 ,1),
                              
          #length of stay zero days
          los0 = ifelse(los > 0, 0, 1),
          
          #not acute visits
          #issue: all levels are "L" (LTC holding)
          #not_acute = ifelse(care_level == "A", 0 ,1),
                              
          #urgent hosp related to pregnancy,childbirth, peripartum
          preg = ifelse(str_detect(diagx_1, 'O|P'),1,0),
                              
          #hosp ending in death or discharge to other facilities
          #we keep SL_dc_disp = "discharged BMA","discharged home"
          sep_disp = as.numeric(!SL_dc_disp %in% c("discharged BMA","discharged home")),
                              
          #hosp beginning or ending in transfer
          #We keep: blank (not a transfer), 006 = home care; 007 = private nursing home/intermediate personal care.
          trans_to = ifelse(hosp_to == 6 | hosp_to == 7 | is.na(hosp_to),0,1), #we keep the last hospitalization
                              
          #hospitalizations not in study interval
          sep_not_15_19 = ifelse(year(sep_date) >= 2015 & year(sep_date) <= 2019, 0, 1),
          
          #patient age is <18 or >110 at index discharge
          age_under_18 = ifelse(sep_age < 18 | sep_age > 110, 1,0)
          ) %>%
    select(moh_study_id,dad_id,elective,los0,preg,sep_disp,trans_to, sep_not_15_19,age_under_18)
#saveRDS(dad_sub,"exclusion_dad.rds")

# number of excluded hospitalizations for each exclusion criteria
result_table <- 
  bind_rows(                          
    as_tibble(t(apply(dad_sub[,c(-1,-2)],2, sum))),
    as_tibble(t(apply(dad_sub[,c(-1,-2)],2, function(x) round(sum(x)/nrow(dad_demo),2))))
  )

result_table <- t(result_table)
colnames(result_table) <- c("N","prop")

# the excluded hospitalizations come from ## individuals 
# (which is not the number of individuals excluded by the criteria)
ind_result_table <- 
  bind_rows(
    as_tibble(t(apply(dad_sub[,c(-1,-2)],2, 
                      function(x) nrow(unique(dad_sub[which(x == 1),1]))))),
    as_tibble(t(apply(dad_sub[,c(-1,-2)],2, 
                      function(x) round(nrow(unique(dad_sub[which(x == 1),1]))/nrow(dad_demo),2))))
  )
ind_result_table <- t(ind_result_table)
colnames(ind_result_table) <- c("n","prop")

# combine two tables together
result_table <- cbind(result_table,ind_result_table)
result_table

## check if the function works correctly:
dad_demo %>% filter(admit != "U") %>% select(moh_study_id) %>% n_distinct()
dad_demo %>% filter(sep_age < 18 | sep_age > 110) %>% select(moh_study_id) %>% n_distinct()

#write.csv(result_table, "exclusion_result_v2.csv", row.names = T) #update: 06-26

# eligible hospitalization id
dad_cohort_id <- dad_sub %>% 
                    filter(elective == 0,los0 == 0,
                           preg == 0, sep_disp == 0,
                           trans_to == 0,sep_not_15_19 == 0,age_under_18 == 0
                           ) %>%
                    select(moh_study_id,dad_id)

##################### exposure and control group ####################
# create exposure variable
dad_demo_cohort <- 
  dad_demo %>% filter(dad_id %in% dad_cohort_id$dad_id) %>% 
  mutate(dc = case_when( SL_dc_disp == "discharged BMA" ~ 'ama',
                         SL_dc_disp == "discharged home" ~ 'wa'))

################ remove duplicate  ###########################################
# check duplicates
dad_demo_cohort %>%
  group_by(moh_study_id,sep_date) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  select(moh_study_id, sep_date) %>%
  inner_join(dad_demo_cohort, by = c("moh_study_id","sep_date")) #%>%
  select(moh_study_id, dad_id,sep_date, ad_date,los)

# numbers before removing duplicates N = 189813, n = 105289
cbind(nrow(dad_demo_cohort), dad_demo_cohort$moh_study_id %>% n_distinct())

# remove duplicate studyids with different los, same sepdate (removed 3 hospitalization)
dad_demo_cohort <- dad_demo_cohort %>%
  group_by(moh_study_id, sep_date)  %>% slice_max(los) %>% ungroup()
cbind(nrow(dad_demo_cohort), dad_demo_cohort$moh_study_id %>% n_distinct())

# removes the duplicate studyid with same los (removed 2 hospitalizations)
dad_demo_cohort <- dad_demo_cohort %>% group_by(moh_study_id, sep_date) %>%
  slice_head(n=1) %>% ungroup()
cbind(nrow(dad_demo_cohort), dad_demo_cohort$moh_study_id %>% n_distinct())

# Removing duplicates affects 5 hospitalizations)

# update cohort id
dad_cohort_id <- dad_demo_cohort %>% select(moh_study_id,dad_id,sep_date)


#saveRDS(dad_cohort_id,"dad_cohort_id.rds") #update: 06-26
#saveRDS(dad_demo_cohort,"dad_demo_cohort.rds") #update: 06-26

##################### results #######################################
# exclusion results
flow_chart <- read.csv("exclusion_result_v2.csv", col.names = c("Exclusion","N","prop","n","prop"))
flow_chart

# cohort size: N = 189808 n = 105289; [v.s v5: N+2663,n+639]
# estimates in proposal: ~220000
dad_cohort_id <- readRDS("dad_cohort_id.rds")

coh_size <- 
rbind(
  #reference
  cbind(1548898,507118),
  #cohort
  cbind(nrow(dad_cohort_id),dad_cohort_id$moh_study_id %>% n_distinct()),
  # excluded size (from ODCr)
  cbind(1548898 - nrow(dad_cohort_id), 507118 - dad_cohort_id$moh_study_id %>% n_distinct()),
  # kept percentage (of ODCr)
  cbind(nrow(dad_cohort_id)/1548898, dad_cohort_id$moh_study_id %>% n_distinct()/507118)
)
rownames(coh_size) <- c("ODCr size","cohort size", "excluded size","pct of ODCr")
colnames(coh_size) <- c("N","n")


# number of hosp and individuals end with DC-AMA and DC-WA, 
# n = ## individuals, N = ## hospitalizations
dad_demo_cohort <- readRDS("dad_demo_cohort.rds")

exposure_freq <- 
dad_demo_cohort %>% group_by(dc) %>% summarise(n = n_distinct(moh_study_id), N = n(), 
                                               #n_p = round(n/n_distinct(dad_demo_cohort$moh_study_id),2), 
                                               N_p = round(N/nrow(dad_demo_cohort),4))

#AMA_N = 3841, WA_N = 183304 ; AMA_n = 2860   WA_n = 103829  
#AMA_N ~ 5600, WA_N ~ 214400

#### save results #####

wb <- createWorkbook()
addWorksheet(wb, "exclusion flow chart")
addWorksheet(wb, "cohort size")
addWorksheet(wb, "exposure freq")

writeData(wb,"exclusion flow chart" ,result_table,rowNames = T)
writeData(wb,"cohort size" ,coh_size,rowNames = T)
writeData(wb,"exposure freq" ,exposure_freq,rowNames = F)
saveWorkbook(wb, "AMA-OD_coh - cohort creation_v2.xlsx",overwrite = T)
