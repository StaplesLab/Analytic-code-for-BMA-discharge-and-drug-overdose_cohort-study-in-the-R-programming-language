#####################################################
# This work is licensed under CC BY-NC-SA 4.0. To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/4.0/
# Suggested citation: Hu X, Yu Y, Khan M, Daly-Grafstein D, Erdelyi S, Staples JA. AMA-OD_coh create table of cohort characteristics. 2023 Jun 19. Retrieved from: ***LINK***
# Table 1 creation
# Plot of trend of ama proportion across study month
# Author: Xiao (Nicole) Hu 
# Date: 2023-06-12
# Updated: 2023-06-19
# v2 updates: use new XW
# v3 updates: update OUD definition
# v4 updates: fix OUD, collapse some categories
#####################################################
# libraries
source("R:/working/AMA-OD_coh/NH/code/AMA-OD - 0 Packages and working directory.R")

# set working dir
setwd("R:/working/AMA-OD_coh/NH/results/Table A")

################## Combine all variables ############
cohort <- readRDS("dad_cohort.rds")
cohort <- cohort %>%
  left_join(readRDS("demo_covariates.rds")) %>% 
  left_join(readRDS("medical_hist_covariates.rds")) %>% 
  left_join(readRDS("medication_covariates.rds")) %>% 
  left_join(readRDS("mrd.rds") %>% dplyr::select(-diagx_1,)) %>% 
  left_join(readRDS("prior_5_od.rds")) %>% 
  left_join(readRDS("num_sbstc_prev_year.rds")) %>% 
  left_join(readRDS("pro_com.rds")) %>% 
  left_join(readRDS("prior_ama.rds")) %>% 
  #left_join(readRDS("ivdu_hist.rds")) %>% 
  left_join(readRDS("ivdu_hist_5y.rds")) %>% 
  left_join(readRDS("drug_toxicity.rds"))

################# Expand index hosp variables ##################################
cohort <- cohort %>% 
  mutate(male_sex = ifelse(gender == "M",1,0)) %>% 
  mutate(age_group = case_when(sep_age < 30 ~ '18-29',
                               sep_age < 50 ~ '30-49',
                               TRUE ~ '50+')) %>% 
  mutate(homeless = homeless.index) %>% 
  #OUD = 1 if  a) is present in any of the 25 diagnostic fields of the index hospitalization; or b) is present in
  #diagnostic fields for ???1 ED visit, ???1 hospitalization, or ???2 physician services during a 5-year lookback
  #interval; or c) if PharmaNet records indicate that OAT was dispensed during a 5-year lookback interval 
  mutate(OUD =  as.numeric(  drugs_opioid == 1| drugs_opioid.nacrs > 0 |
                             oat_5y > 0 ) ) %>% 
  mutate(if_icu = as.numeric(icu_days > 0)) %>% 
  mutate(num_od_prev_year_cat = case_when(num_od_prev_year == 0 ~ "0",
                                          num_od_prev_year == 1 ~ "1",
                                          TRUE ~ "2+")) %>% 
  mutate(geq1_num_psyc_hosp_prev_year = as.numeric(num_psyc_hosp_prev_year > 0)) %>%
  mutate(geq1_oat = as.numeric(oat > 0)) %>% 
  mutate(geq1_opioid = as.numeric(opioid > 0)) %>% 
  mutate(geq1_benzo = as.numeric(benzo > 0)) %>% 
  mutate(geq1_antipsyc = as.numeric(antipsyc > 0)) %>% 
  mutate(study_year = (year(sep_date)  - year((ymd("2015-01-01"))) + 1)) %>% 
  mutate(study_month = (study_year - 1)*12 + month(sep_date) - month((ymd("2015-01-01"))) + 1) %>% 
  mutate(min_frst_sbstc_age_grp = case_when(min_frst_sbstc_age <= 14 ~ "frst_sbstc_age <= 14",
                                            min_frst_sbstc_age > 14 ~ "frst_sbstc_age > 14",
                                            is.na(min_frst_sbstc_age) ~ "frst_sbstc_age > 14")) %>% 
  mutate(min_frst_sbstc_age_grp_ms = case_when(min_frst_sbstc_age <= 14 ~ "frst_sbstc_age <= 14",
                                            min_frst_sbstc_age > 14 ~ "frst_sbstc_age > 14",
                                            is.na(min_frst_sbstc_age) ~ "Missing")) %>% 
  mutate(sep30_total_od_cat = case_when(sep30_total_od == 0 ~ "0",
                                        sep30_total_od == 1 ~ "1",
                                        TRUE ~ "2+")) %>% 
  mutate(other_medication = ifelse(other < 20, other, "20+")) %>% 
  mutate(num_od_5_year_cat = case_when(num_od_5_year == 0 ~ "0",
                                       num_od_5_year == 1 ~ "1",
                                       TRUE ~ "2+")) %>% 
  mutate(num_active_cat = case_when(num_active == 0 ~ "0 or 1",
                                    num_active == 1 ~ "0 or 1",
                                    TRUE ~ "2+")) %>% 
  mutate(mrd = case_when(`Opioid misuse` == 1 ~ "Admission directly related to opioid use",
                         other_sbstc == 1 ~ "Admission directly related to other substance use",
                         ed_or_om == 1 ~ "Endocarditis or osteomyelitis",
                         Gastroenteritis == 1 ~ "Gastroenteritis",
                         HIV == 1 ~ "HIV",
                         Cancer == 1 ~ "Cancer",
                         Diabetes == 1 ~ "Diabetes",
                         Psychiatric == 1 ~ "Psychiatric", 
                         `Cardiac disease` == 1 ~ "Cardiac disease",
                         Stroke == 1 ~ "Stroke",
                         Pneumonia == 1 ~ "Pneumonia",
                         `Obstructive lung disease` == 1 ~ "Obstructive lung disease",
                         `Biliary tract disease` == 1 ~ "Biliary tract disease",
                         Cellulitis == 1 ~ "Cellulitis",
                         `Kidney injury` == 1 ~ "Kidney injury",
                         Urolithiasis == 1 ~ "Urolithiasis",
                         TRUE ~ "Other diagnoses")) %>% 
  mutate(amb_arrive = as.numeric(ambulanc != "N")) %>% 
  mutate(ed_entry = as.numeric(entry == "E")) %>% 
  mutate(frst_od_age = (as.period(interval(birth_date, first_od_date)))@year) %>% 
  mutate(num_sbstc_prev_year_cat = as.numeric(num_sbstc_prev_year > 0)) %>% 
  mutate(sdpr_pay_cat = as.numeric(sdpr_pay > 0))
  
saveRDS(cohort, "coh_all_v2.rds") #

################## Create Table 1 #############################################

# Vector of variables to summarize
table1_vars <- c(# demographic
                 "sep_age", "age_group","gender","male_sex", "pop_density_class","homeless","sdpr_pay_cat",
                 
                 # medical hist
                 "num_hosp_prev_year","geq1_hosp_prev_year","tdays_prev_year",
                 "geq1_num_psyc_hosp_prev_year", "num_clinic_prev_year", "geq7_clinic_prev_year", 
                
                 
                 # comorbidities
                 "homeless_hist",
                 "alcohol", "drugs_all", "drugs_opioid","drugs_nonopioid","any_sbstcs",
                 "mood","anxiety","dm_nc_or_compl","mi","chf","cvd","copd","dem","renal",
                 "cancer","afib","osa", "psych","cihd", "htn","endocarditis","om","hiv",  
                 "cci", "cci_cat","cci_cat5",
                 
                 # medication
                 "num_active","num_active_cat", "oat_active_ad" , "ttl_meds_90ad", 
                 "geq1_oat", "geq1_benzo", "geq1_opioid", "geq1_antipsyc", "other_medication",
                 
                 #substance use
                 "min_frst_sbstc_age_grp", "min_frst_sbstc_age_grp_ms", "num_od_prev_year_cat","num_od_5_year_cat",
                 "num_sbstc_prev_year_cat", "ivdu_hist", 
                 
                 # index hosp
                 "los","if_icu","mrd",
                 "study_year","health_region",
                 
                 # outcome
                 "sep30_total_od_cat","od_fatal_30","od_status","od_stime"
                 
                 
)


## Vector of non-normal variables 
non_norm_vars <- c("sep_age","sdpr_pay", "num_clinic_prev_year", "num_hosp_prev_year", "tdays_prev_year",
                   "cci", "ttl_meds_90ad", "los","num_active")


## Vector of categorical variables
cat_vars <- c("age_group", "male_sex","gender", "pop_density_class","homeless","sdpr_pay_cat",
              "geq1_hosp_prev_year","geq7_clinic_prev_year",
              "geq1_num_psyc_hosp_prev_year",
              
              "homeless_hist",
              "alcohol", "drugs_all", "drugs_opioid","drugs_nonopioid","any_sbstcs",
              "mood","anxiety","dm_nc_or_compl","mi","chf","cvd","copd","dem","renal","hiv", 
              "cancer","afib","osa", "psych","cihd", "htn","endocarditis","om", 
              "cci", "cci_cat","cci_cat5",
              
              "oat_active_ad","num_active_cat",
              "geq1_oat", "geq1_benzo", "geq1_opioid", "geq1_antipsyc", "other_medication",
              
              "min_frst_sbstc_age_grp", "min_frst_sbstc_age_grp_ms", "num_od_prev_year_cat","num_od_5_year_cat",
              "num_sbstc_prev_year_cat", "ivdu_hist", 
              
              "if_icu","mrd","study_year","health_region",
              "sep30_total_od_cat","od_fatal_30","od_status","od_stime"
             
)

tab1a <- CreateTableOne(data = cohort, 
                        vars = table1_vars, factorVars = cat_vars)
tab1a_csv <- print(tab1a, nonnormal = non_norm_vars, showAllLevels = F, formatOptions = list(big.mark=','), printToggle = F)
write.csv(tab1a_csv, file = "AMA-OD_coh - Table 1a v9 raw.csv")
#writeData(wb,"Table_1a" ,tab1a_csv,rowNames = T)

# Group by exposure category
tab1b <- CreateTableOne(data = cohort, 
                        strata = 'dc', 
                        vars = table1_vars, factorVars = cat_vars)
tab1b_csv <- print(tab1b, nonnormal = non_norm_vars, showAllLevels = F, formatOptions = list(big.mark=','), smd=TRUE, printToggle = F)
write.csv(tab1b_csv, file = "AMA-OD_coh - Table 1b v9 raw.csv")
#writeData(wb,"Table_1b" ,tab1b_csv,rowNames = T)

tab1c <- CreateTableOne(data = cohort %>% filter(OUD == 1), 
                        strata = 'dc', 
                        vars = table1_vars, factorVars = cat_vars)
tab1c_csv <- print(tab1c, nonnormal = non_norm_vars, showAllLevels = F, formatOptions = list(big.mark=','), smd=TRUE, printToggle = F)
write.csv(tab1c_csv, file = "AMA-OD_coh - Table 1c v9 raw.csv")
#writeData(wb,"Table_1c" ,tab1c_csv,rowNames = T)

#saveWorkbook(wb, "AMA-OD_coh - Table 1_abc v3.xlsx",overwrite = T)



############################### exploratory plots #############################
cohort <- readRDS("coh_all_v2.rds")

#################### trend of ama/wa across study month ######################
ratio_plot <- 
  cohort %>% 
  group_by(study_month,dc) %>% 
  count() %>% 
  pivot_wider(values_from = n, names_from = dc) %>%
  mutate(ama_wa_ratio = ama*100/(wa+ama)) %>% select(study_month,ama_wa_ratio) %>% 
  ggplot(aes(x = study_month, y = ama_wa_ratio)) +
  geom_line()+
  geom_point()+
  geom_smooth(method = "lm",color = "red4",se = T) +
  xlab("Study months") +
  ylab("BMA discharge (%)")+
  coord_cartesian(ylim=c(0,7.5))+
  scale_x_continuous(
    labels = c(0,12,24,36,48,60),
    breaks = c(0,12,24,36,48,60))+
  theme_stapleslab
ratio_plot
ggsave(filename = "R:/working/AMA-OD_coh/NH/results/fig/bma_pct_all_CI.tiff",plot = ratio_plot,
       width=14, height=8, units="in", dpi=600, compression = "lzw")
ggsave(filename = "R:/working/AMA-OD_coh/NH/results/fig/bma_pct_all_CI.pdf",plot = ratio_plot,
       width=14, height=8, units="in", dpi=600)

ratio_table <- 
  cohort %>% 
  group_by(study_month,dc) %>% 
  count() %>% 
  pivot_wider(values_from = n, names_from = dc) %>%
  mutate(ama_pct = round(ama*100/(wa+ama),2)) %>% select(study_month,ama_pct)
colnames(ratio_table) <- c("Study months","BMA discharge (%)")
write.xlsx(ratio_table, "R:/working/AMA-OD_coh/NH/results/Table A/BMA_pct_months.xlsx")

ratio_oud <- 
  cohort %>% 
  filter(OUD == 1) %>% 
  group_by(study_month,dc) %>% 
  count() %>% 
  pivot_wider(values_from = n, names_from = dc) %>%
  mutate(ama_wa_ratio = ama/(wa+ama)) %>% select(study_month,ama_wa_ratio) %>% 
  ggplot(aes(x = study_month, y = ama_wa_ratio)) +
  geom_line()+
  geom_point()+
  coord_cartesian(ylim=c(0,0.6))+
  geom_smooth(method = "lm",color = "green",se = F) +
  xlab("Study Months") +
  ylab("Percentage of DC-BMA")+
  theme_stapleslab

ratio_oud
ggsave(filename = "R:/working/AMA-OD_coh/NH/results/Table A/fig/bma_pct_oud.tiff",plot = ratio_oud,
       width=14, height=8, units="in", dpi=300, compression = "lzw", type='cairo')

############################ other plots ######################################

#fsa
missing_fsa <- cohort %>% filter(is.na(fsaType)) %>% select(clnt_fsa,fsaType,pop_density_class)
missing_fsa_freq <- cohort %>% filter(is.na(fsaType)) %>% select(clnt_fsa,fsaType,pop_density_class) %>% count(clnt_fsa)
write.csv(missing_fsa_freq, file = "missing_fsa_freq.csv")

#first substance age (only have data from 4606 individuals for 11577 hosps)
cohort %>% filter(!is.na(min_frst_sbstc_age)) %>% summarise(N = n(), n = n_distinct(moh_study_id))
sum(is.na(cohort$min_frst_sbstc_age))

cohort %>%
  ggplot() + 
  geom_histogram(aes(x = min_frst_sbstc_age), binwidth = 1) + 
  geom_vline(xintercept = median(cohort$min_frst_sbstc_age,na.rm = T), color = "red") #median = 14

last_sbstc_before_sep <- readRDS("last_sbstc_before_sep.rds")
last_sbstc_before_sep %>% ungroup() %>% count(mha_sbstc) %>% arrange(desc(n)) %>% write.xlsx (file = "last_sbstc_before_sep.xlsx")

#study month and year
study_y <- cohort %>% distinct(year(ad_date),study_year)
study_m <- cohort %>% distinct(year(ad_date),month(ad_date),study_month) %>% arrange(study_month)

#OUD
cohort %>% group_by(dc,OUD) %>% summarise(n = n(), p = n()/nrow(cohort))
2252/859
27353/5527
(2252+27353)/(859+5527)

#clinic visits
cohort %>%
  ggplot() + 
  geom_histogram(aes(x = num_clinic_prev_year), binwidth = 1) + 
  coord_cartesian(xlim=c(0,50)) + 
  geom_vline(xintercept = 7, color = "red") +
  theme_stapleslab

cohort %>%
  ggplot() + 
  geom_histogram(aes(x = num_clinic_prev_year), binwidth = 1) + 
  coord_cartesian(xlim=c(0,50)) + 
  geom_vline(xintercept = 7, color = "red") +
  facet_grid(.~study_year) + 
  theme_stapleslab

#hosp visits
cohort %>%
  ggplot() + 
  geom_histogram(aes(x = num_hosp_prev_year), binwidth = 1) + 
  coord_cartesian(xlim=c(0,7)) + 
  geom_vline(xintercept = 1, color = "red") +
  theme_stapleslab

#comorbidities
avg_dad <- pmh_all %>%
  summarize(across(.cols = all_of(paste0(selected_coms,".dad")), .fns = ~mean(.x))) %>% 
  pivot_longer(cols = everything(), names_to = "row_names", values_to = "avg") %>% 
  arrange(desc(avg)) %>% 
  print(n = 100)

avg_msp <- pmh_all %>%
  summarize(across(.cols = all_of(paste0(selected_coms,".msp")), .fns = ~mean(.x))) %>% 
  pivot_longer(cols = everything(), names_to = "row_names", values_to = "avg") %>% 
  arrange(desc(avg)) %>% 
  print(n = 100)

#od_free survival time
cohort %>%
  ggplot() + 
  geom_histogram(aes(x = od_stime), binwidth = 30) + 
  #coord_cartesian(xlim=c(0,50)) + 
  theme_stapleslab

last_sbstc_before_sep <- readRDS("last_sbstc_before_sep.rds")


# ratio by years
cohort %>% 
  group_by(study_year,dc) %>% 
  count() %>% 
  pivot_wider(values_from = n, names_from = dc) %>%
  mutate(ama_wa_ratio = ama/wa) %>% select(study_year,ama_wa_ratio)

cohort %>% 
  filter(OUD == 1) %>% 
  group_by(study_year,dc) %>% 
  count() %>% 
  pivot_wider(values_from = n, names_from = dc) %>%
  mutate(ama_wa_ratio = ama/wa) %>% select(study_year,ama_wa_ratio)

# ama/wa in health regions
ratio_hr <- 
cohort %>% 
  #filter(health_region != "Unknown/Out of Province HA") %>% 
  group_by(health_region,dc) %>% 
  count() %>% 
  pivot_wider(values_from = n, names_from = dc) %>%
  mutate(ama_wa_ratio = ama/(wa+ama)) %>% 
  ggplot(aes(x = health_region, y = ama_wa_ratio)) +
  geom_bar(stat = "identity") +
  xlab("Health Region") +
  ylab("Percentage of DC-BMA")+
  #coord_cartesian(ylim=c(0,0.2))+
  theme_stapleslab
ratio_hr
ggsave(filename = "R:/working/AMA-OD_coh/NH/results/Table A/fig/bma_pct_region_unk.tiff",plot = ratio_hr,
       width=14, height=8, units="in", dpi=300, compression = "lzw", type='cairo')


# OUD
sub_coh <- cohort %>% filter(OUD == 1) #12587

sub_coh %>% count(opioid_dad) #4898 #1827
sub_coh %>% count(drugs_opioid) #8331
sub_coh %>% count(drugs_opioid.nacrs > 1) #1238 #180
sub_coh %>% count(oat_5y > 1) # 6118 #1820

cohort %>% filter(opioid_dad == 1) %>% count(drugs_nonopioid)
cohort %>% filter(drugs_opioid == 1) %>% count(drugs_nonopioid)
cohort %>% filter(drugs_opioid.nacrs > 1) %>% count(drugs_nonopioid)
cohort %>% filter(oat_5y > 1) %>% count(drugs_nonopioid)

sub_coh %>% count(drugs_opioid)#8331
sub_coh %>% filter(drugs_opioid == 0) %>% count(opioid_dad) #1827
sub_coh %>% filter(drugs_opioid == 0,opioid_dad == 0) %>% count(oat_5y>0) #1849
sub_coh %>% filter(drugs_opioid == 0,opioid_dad == 0,oat_5y == 0) %>% count(drugs_opioid.nacrs > 0) #580

#homeless
cohort %>% group_by(homeless_hist,homeless) %>% count()
cohort %>% group_by(homeless_hist) %>% summarize(n = n(), p = n/nrow(cohort))
cohort %>% mutate(homeless_ind_hist = as.numeric(homeless == 1 | homeless_hist == 1)) %>%  group_by(homeless_ind_hist) %>% summarize(n = n(), p = n/nrow(cohort))


# substance use in the prior year
cohort %>% count(num_sbstc_prev_year) %>% 
  ggplot(aes(x = num_sbstc_prev_year, y = n)) + 
  geom_bar(stat = identity)

cohort %>%
  ggplot() + 
  geom_histogram(aes(x = num_sbstc_prev_year), binwidth = 1) + 
  coord_cartesian(xlim=c(0,10)) + 
  theme_stapleslab

