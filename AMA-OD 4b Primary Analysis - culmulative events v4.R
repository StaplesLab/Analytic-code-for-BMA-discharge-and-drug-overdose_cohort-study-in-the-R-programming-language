################################################################################
# This work is licensed under CC BY-NC-SA 4.0. To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/4.0/
# Suggested citation: Hu X, Yu Y, Khan M, Daly-Grafstein D, Erdelyi S, Staples JA. AMA-OD_coh primary analysis. 2023 Nov 7. Retrieved from: ***LINK***
# Analysis - Cumulative events within 30 days of index discharge (NOT accounting for censoring)
# Author: Xiao (Nicole) Hu 
# Date: 2023-06-19
# Updated: 2023-11-07
################################################################################

# libraries
source("R:/working/AMA-OD_coh/NH/code/AMA-OD - 0 Packages and working directory.R")

# set working dir
setwd("R:/working/AMA-OD_coh/NH/results/Table B")

# read cohort
cohort = readRDS("R:/working/AMA-OD_coh/NH/results/Table B/cohort_cox.rds")

summGLM <- function(glmFit){
  cov_robust <- vcovHC(glmFit, type = "HC1")
  se_robust = sqrt(diag(cov_robust))
  res <- tidy(glmFit) %>% 
    mutate(conf.low = exp(estimate - qnorm(0.975)*se_robust),
           conf.high = exp(estimate + qnorm(0.975)*se_robust),
           estimate = exp(estimate),) %>% 
    select(estimate, conf.low, conf.high, p.value)
  return(res)
}

######################### Descriptive Covariates ###############################

table2_vars = c("od_status","od_fatal_30","od_non_fatal_30",
                "readmit_U_30","readmit_L_30","ed_30","death_30")
cat_vars = c("od_status","od_fatal_30","od_non_fatal_30",
             "readmit_U_30","readmit_L_30","ed_30","death_30")
  
tab2 <- CreateTableOne(data = cohort, 
                       strata = 'dc',
                       vars = table2_vars, factorVars = cat_vars)
tab2_csv <- print(tab2, showAllLevels = F, formatOptions = list(big.mark=','), printToggle = F,smd = F,test = F)

write.csv(tab2_csv, file = "Table 2_cumul_dscpt v2.csv")
tab2_csv <- read.csv("Table 2_cumul_dscpt v2.csv")
tab2_csv <- tab2_csv[-1,]
tab2_csv$label <- table2_vars
saveRDS(tab2_csv,"tab2_cumul_ds.rds")

################## Unadjusted Analysis ###########################################


outcomes = c("od_status","od_fatal_30","od_non_fatal_30","readmit_U_30","ed_30","death_30")
lg_models = list()
or = numeric(0)

for(i in 1: length(outcomes)){
  formula = as.formula(paste(outcomes[i], " ~ dc"))
  lg_models[[i]] <- glm(formula, data = cohort, family = binomial(link = "logit"))

  res <- summGLM(lg_models[[i]])[2,] 
  or <- rbind(or,res)
}

rownames(or) <- outcomes
or


###################### Adjusted Analysis #######################################

#data set with adjusted variables
cohort_adj <- cohort %>% dplyr::select(od_status, dc, 
                                       # demographics   
                                       age_group,male_sex, pop_density_class, homeless, msp_prem, sdpr_pay,
                                       
                                       # medical hist
                                       num_hosp_prev_year,
                                       #tdays_prev_year, 
                                       num_clinic_prev_year, prior_ama,
                                       #min_frst_sbstc_age, 
                                       
                                       # comorbidities
                                       #homeless_hist,drugs_all, any_sbstcs,
                                       alcohol, drugs_opioid, drugs_nonopioid,
                                       psych, cci_cat,
                                       
                                       #substance use
                                       num_sbstc_prev_year,num_od_5_year_cat,OUD,
                                       
                                       # OAT
                                       oat_active_ad,
                                       ttl_meds_90ad, #not in appendix
                                       geq1_oat, geq1_benzo, geq1_opioid, geq1_antipsyc, #not in appendix
                                       
                                       # index hosp
                                       los,if_icu,study_month,drug_toxicity,procedure, 
                                       complication, ttl_duration, amb_arrive,ed_entry,
                                       HIV,Cancer,Diabetes,Psychiatric, `Cardiac disease`,Stroke,
                                       Pneumonia,`Obstructive lung disease`,`Biliary tract disease`,Cellulitis,`Kidney injury`,
                                       Urolithiasis,`Opioid misuse`,other_sbstc,ed_or_om,
                                       #Gastroenteritis
                                       #secondary outcomes
                                       od_fatal_30,od_non_fatal_30,readmit_U_30,ed_30,death_30)


# global model
full_glm <- glm(od_status ~ ., 
                data = cohort_adj %>% 
                select(- c(od_fatal_30,od_non_fatal_30,readmit_U_30,ed_30,death_30)), 
                family = binomial(link = "logit")) 

# lower model with force-in covariates
lower_glm <- glm(od_status ~ ., 
                 data = cohort_adj %>% 
                 select(od_status, dc, male_sex, age_group, OUD, num_od_5_year_cat), 
                 family = binomial(link = "logit")) 

# BE
step_AIC <- step(full_glm,
                 scope = list(upper = formula(full_glm),
                              lower = formula(lower_glm)),
                 direction = "backward")

#best model from BE
reduce_glm <- glm(od_status ~ dc + age_group + male_sex + homeless + msp_prem + 
                    sdpr_pay + num_clinic_prev_year + drugs_opioid + drugs_nonopioid + 
                    psych + cci_cat + num_od_5_year_cat + OUD + ttl_meds_90ad + 
                    geq1_opioid + geq1_antipsyc + study_month + drug_toxicity + 
                    ttl_duration + amb_arrive + Cancer + Psychiatric + Stroke + 
                    `Biliary tract disease` + Cellulitis + `Opioid misuse` + 
                    other_sbstc + ed_or_om + oat_active_ad,
                  data = cohort_adj, 
                  family = binomial(link = "logit")) #make sure we have: oat_active_ad+study_month


saveRDS(reduce_glm,"lg_step_AIC.rds")

#AIC:  8363 -> 8334
glance(reduce_glm) %>% select(AIC)
glance(full_glm) %>% select(AIC)

#eliminated variables, 
names(coef(full_glm))[!names(coef(full_glm)) %in% names(coef(reduce_glm))]

#EPV: 20 -> 36
min(table(cohort_adj %>% pull(od_status)))/(length(coef(full_glm)) - 1) #18.83
min(table(cohort_adj %>% pull(od_status)))/(length(coef(reduce_glm))-1) #31.58

# check the fitness
# hosmer-Lemeshow goodness of fit test:
library(ResourceSelection)
hoslem.test(cohort$od_status, fitted(reduce_glm))

# residuals
resid <- residuals(reduce_glm, type = "deviance")
resid <- rstudent(reduce_glm)[1:80000]
resid <- rstandard(reduce_glm)
plot(resid)

pred <- predict(reduce_glm)
plot(resid,pred)

# AUC
ROC <- roc(cohort$od_status, fitted(reduce_glm))
auc(ROC)
coords(ROC,"best") #Youden's J statistics
predicted <- ifelse(predict(reduce_glm, type = "response") > coords(ROC,"best")[,1], 1, 0)
confusionMatrix(as.factor(predicted), as.factor(cohort$od_status))

#vif
vif(reduce_glm)[which(vif(reduce_glm) > 5),]


library(DHARMa)
simulationOutput <- simulateResiduals(fittedModel = reduce_glm, n = 250)
plotQQunif(simulationOutput)

############################# odds ratio #######################################
reduce_glm = readRDS("lg_step_AIC.rds")
cohort = readRDS("R:/working/AMA-OD_coh/NH/results/Table B/cohort_cox.rds")

outcomes <- c("od_status","od_fatal_30","od_non_fatal_30","readmit_U_30","ed_30","death_30")

alg_models = list()
aor = summGLM(reduce_glm)[2,]

for(i in 2: length(outcomes)){
  formula = as.formula(
    paste( outcomes[i], "~", sub(".*~ ","",reduce_glm$formula)[3])
  )
  alg_models[[i]] <- glm(formula, data = cohort,  family = binomial(link = "logit"))
  res <-  summGLM(alg_models[[i]])[2,]
  aor <- rbind(aor,res)
}

aor <- as.data.frame(aor)
rownames(aor) <- outcomes


# stratified by gender
cohort_adj_male <- cohort %>% filter(gender == "M")
cohort_adj_male$dc <- relevel(as.factor(cohort_adj_male$dc), ref = "wa")

alg_male_models = list()
aor_male = numeric(0)

for(i in 1: length(outcomes)){
  formula = as.formula(
    gsub("male_sex \\+", "", paste( outcomes[i], "~", sub(".*~ ","",reduce_glm$formula)[3]))
  )
  alg_male_models[[i]] <- glm(formula, data = cohort_adj_male,  family = binomial(link = "logit"))
  res <- summGLM(alg_male_models[[i]])[2,]
  aor_male <- rbind(aor_male,res)
}

aor_male <- as.data.frame(aor_male)
rownames(aor_male) <- outcomes


cohort_adj_notmale <- cohort %>% filter(gender == "F")
cohort_adj_notmale$dc <- relevel(as.factor(cohort_adj_notmale$dc), ref = "wa")

alg_notmale_models = list()
aor_notmale = numeric(0)

for(i in 1: length(outcomes)){
  formula = as.formula(
    gsub("male_sex \\+", "", paste( outcomes[i], "~", sub(".*~ ","",reduce_glm$formula)[3]))
  )
  alg_notmale_models[[i]] <- glm(formula, data = cohort_adj_notmale,  family = binomial(link = "logit"))
  res <-summGLM(alg_notmale_models[[i]])[2,]
  aor_notmale <- rbind(aor_notmale,res)
}

aor_notmale <- as.data.frame(aor_notmale)
rownames(aor_notmale) <- outcomes

############### results ########################################################
or_combined <- 
  cbind(
    or %>% 
      mutate(unadjusted = paste0(sprintf("%.2f",estimate),", ",sprintf("%.2f",conf.low), "-",sprintf("%.2f",conf.high), ", ", "p", ifelse(p.value < 0.001, "<0.001", paste0("=",sprintf("%.3f",p.value))))) %>% 
      select(unadjusted),
    aor %>%  
      mutate(adjusted = paste0(sprintf("%.2f",estimate),", ",sprintf("%.2f",conf.low), "-",sprintf("%.2f",conf.high), ", ", "p", ifelse(p.value < 0.001, "<0.001", paste0("=",sprintf("%.3f",p.value))))) %>% 
      select(adjusted),
    aor_male %>% 
      mutate(adjusted_male = paste0(sprintf("%.2f",estimate),", ",sprintf("%.2f",conf.low), "-",sprintf("%.2f",conf.high), ", ", "p", ifelse(p.value < 0.001, "<0.001", paste0("=",sprintf("%.3f",p.value)))))%>% 
      select(adjusted_male),
    aor_notmale %>% 
      mutate(adjusted_female = paste0(sprintf("%.2f",estimate),", ",sprintf("%.2f",conf.low), "-",sprintf("%.2f",conf.high), ", ", "p", ifelse(p.value < 0.001, "<0.001", paste0("=",sprintf("%.3f",p.value)))))%>% 
      select(adjusted_female)
  )
or_combined

or_combined$label <- c("od_status","od_fatal_30","od_non_fatal_30","readmit_U_30","ed_30","death_30")

names <- tibble(lab_name = c('Cumulative events within 30 days of index discharge (NOT accounting for censoring)',
                            'Non-fatal or fatal drug overdose (primary outcome)',
                            'Fatal drug overdose (secondary outcome)',
                            'Non-fatal drug overdose (secondary outcome)',
                            'Unplanned/urgent hospital readmission   (secondary outcome)',
                            'ED visits (all cause, overdose & non-overdose) (secondary outcome)',
                            'Death from any cause  (secondary outcome)'),
                 label = c("","od_status","od_fatal_30","od_non_fatal_30","readmit_U_30","ed_30","death_30"))
res_table_cul <- 
  names %>% left_join(or_combined) %>% left_join(readRDS("tab2_cumul_ds.rds")) %>% select(lab_name,wa,ama,unadjusted,adjusted,adjusted_male,adjusted_female)

saveRDS(res_table_cul,"cumulative_res_table.rds")

