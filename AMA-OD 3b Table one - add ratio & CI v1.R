####################################################
# This work is licensed under CC BY-NC-SA 4.0. To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/4.0/
# Suggested citation: Hu X, Yu Y, Khan M, Daly-Grafstein D, Erdelyi S, Staples JA. AMA-OD_coh create table of cohort characteristics - prevalence ratio. 2023 Jun 20. Retrieved from: ***LINK***
# Table 1 creation - Calculate crude prevalence ratio, CI and P-value
# Author: Xiao (Nicole) Hu
###################################################

full_cohort <- readRDS("R:/working/AMA-OD_coh/NH/results/Table A/coh_all_v2.rds")
OUD_cohort <- full_cohort %>% filter(OUD == 1)


get_ratio_CI <- function(cohort){
  selected_vars  <- c("dc", "age_group", "male_sex", "pop_density_class","homeless","sdpr_pay_cat",
                      "geq1_hosp_prev_year","geq7_clinic_prev_year",
                      "geq1_num_psyc_hosp_prev_year",
                      
                      "homeless_hist",
                      "alcohol", "drugs_opioid","drugs_nonopioid","any_sbstcs",
                      "mood","anxiety","dm_nc_or_compl","mi","chf","cvd","copd","dem","renal","hiv", 
                      "cancer","afib","osa", "psych","cihd", "htn","endocarditis","om", 
                      "cci_cat",
                      
                      "oat_active_ad","geq1_oat", "geq1_benzo", "geq1_opioid", "geq1_antipsyc", 
                      
                      "min_frst_sbstc_age_grp_ms", "num_od_prev_year_cat","num_od_5_year_cat",
                      "num_sbstc_prev_year_cat", "ivdu_hist", 
                      
                      "if_icu","mrd"
                      
                      
  )
  cohort_sub <- cohort[,selected_vars]
  cohort_sub[,-1] <- lapply(cohort_sub[,-1], as.factor)
  colnames(cohort_sub) <- c("dc",paste0(1:(length(selected_vars)-1)+10, colnames(cohort_sub)[-1]))
  
  wa <- cohort_sub %>% 
    filter(dc == "wa") %>% 
    select(-dc) %>% 
    pivot_longer(everything()) %>% 
    group_by(name,value) %>% 
    tally() %>% 
    mutate(p_wa = n/nrow(cohort_sub %>% filter(dc == "wa"))) %>% 
    rename(n_wa = n)
  
  bma <- cohort_sub %>% 
    filter(dc == "ama") %>%
    select(-dc) %>% 
    pivot_longer(everything()) %>% 
    group_by(name,value) %>% 
    tally() %>% 
    mutate(p_bma = n/nrow(cohort_sub %>% filter(dc == "ama"))) %>% 
    rename(n_bma = n)
  all <- wa %>% left_join(bma, by = c("name","value")) %>% ungroup() %>% filter(value != 0)
  
  
  all <- all %>% 
    mutate(total_wa = cohort %>% filter(dc=="wa") %>% nrow(),
           total_bma = cohort %>% filter(dc=="ama") %>% nrow(),
           RR = p_bma/p_wa,
           RR_se = sqrt((1-p_bma)/n_bma + (1-p_wa)/n_wa),
           RR_low_ci = exp(log(RR) - 1.96*RR_se),
           RR_up_ci = exp(log(RR) + 1.96*RR_se),
           z = log(RR)/RR_se,
           ratio_pvalue = ifelse((pmin(pnorm(z),1-pnorm(z))*2)<0.001,"p<0.001", paste0("p=",sprintf("%.3f",pmin(pnorm(z),1-pnorm(z))*2)))
    ) 
  
  return(all)
}



########### Full cohort ##########
all <- get_ratio_CI(full_cohort)
# write.xlsx(all,"R:/working/AMA-OD_coh/NH/results/Table A/crude ratio CI v4.xlsx")
# #add label column in excel, and then read in again
# all <- read.xlsx("R:/working/AMA-OD_coh/NH/results/Table A/crude ratio CI v4.xlsx")
#saveRDS(all$label,"label.rds")
all <- cbind(label = readRDS("label.rds"),all)

lable_name <- read.xlsx("R:/working/XW files/AMA-OD_coh table1_lab.xlsx")
table_1 <- read.csv("AMA-OD_coh - Table 1b v9.csv")

res_1b <- lable_name %>% 
  left_join(table_1, by = c("label" = "X")) %>% 
  left_join(all) %>% 
  mutate("ratio, CI, p" = ifelse(!is.na(RR), paste0(sprintf("%.1f",RR),", ",sprintf("%.1f",RR_low_ci),"-",sprintf("%.1f",RR_up_ci), ", ", ratio_pvalue), RR)) %>% 
  select(lab_show,ama,wa,SMD,`ratio, CI, p`) 

write.xlsx(res_1b, "R:/working/AMA-OD_coh/NH/results/Table A/AMA-OD_coh - Table 1b v9.xlsx")


####### OUD cohort #############
all <- get_ratio_CI(OUD_cohort)
# write.xlsx(all,"R:/working/AMA-OD_coh/NH/results/Table A/crude ratio CI OUD.xlsx")
# #add label column in excel, and then read in again
# all <- read.xlsx("R:/working/AMA-OD_coh/NH/results/Table A/crude ratio CI OUD.xlsx")
all <- cbind(label = readRDS("label.rds"),all)


lable_name <- read.xlsx("R:/working/XW files/AMA-OD_coh table1_lab.xlsx")
table_1 <- read.csv("AMA-OD_coh - Table 1c v9.csv")

res_1c <- lable_name %>% 
  left_join(table_1, by = c("label" = "X")) %>% 
  left_join(all) %>% 
  mutate("ratio, CI, p" = ifelse(!is.na(RR), paste0(sprintf("%.1f",RR),", ",sprintf("%.1f",RR_low_ci),"-",sprintf("%.1f",RR_up_ci), ", ", ratio_pvalue), RR)) %>% 
  select(lab_show,ama,wa,SMD,`ratio, CI, p`) 

write.xlsx(res_1c, "R:/working/AMA-OD_coh/NH/results/Table A/AMA-OD_coh - Table 1c v9.xlsx")




