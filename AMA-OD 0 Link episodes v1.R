#############################################
# This work is licensed under CC BY-NC-SA 4.0. To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/4.0/
# Suggested citation: Hu X, Yu Y, Daly-Grafstein D, Khan M, Staples JA. AMA-OD_coh Link episode of care. 2023 August 22. Retrieved from: ***LINK***
# Link episodes of care (ambulance service, ED visits, hospitalizations, transfers to another care facility)
# Author: Shannon Erdelyi, Daniel Daly-Grafstein, Ying (Daisy) Yu, John Staples, Xiao (Nicole) Hu
# Adapted for AMA-OD by Nicole Hu
# Date: 2022-12-08
# Updated: 2023-08-22

#############################################

# Mandatory inputs to read DAD function:
#   - DAD data table: this table should include columns moh_study_id, ad_date, sep_date,
#     sep_disp, hosp_to*, hosp_from*
#         - note sep_disp is only needed for DT_1day_transfer and DT_1day_transfer_hosp
#         - hosp_to* and hosp_from* only needed for DT_1day_transfer_hosp


# Optional inputs to read DAD function:
#   - filter_record_num: Flag, if TRUE, filter out duplicates using RECRDNUM flag in DAD data
#                       Note if set to TRUE but no RECRDNUM column in DAD data, will throw an error
#                       Default is FALSE. If FALSE, filters duplicates by removing duplicates of
#                       moh_study_id, ad_date, sep_date
#   - episodes_of_care: parameter to decide how to create episodes of care. If episodes_of_care = "DT_1day", 
#     an episode of care is created by combining
#     records for a individual if less than or equal to 1 day between admission and prior discharge date.
#     If episodes_of_care = "DT_1day_transfer, criteria as previous + need a transfer flag in the sep_disp
#     column (sep_disp == "01" | sep_disp == "10" | sep_disp == "20). 
#     If episodes_of_care = "DT_1day_transfer_hosp", criteria as previous +
#     need the hosp_to* and hosp_from* ID columns to match between transfers. If episodes_of_care = "all"
#     create episodes according to all 3 criteria.
#     Default is episodes_of_care = "DT_1day"

#dd_path <- ""                      # file path for data dictionary (excel spreadsheet)
#types <- c("G", "H", "N")
#dad_dir <- ""                      # file path for data folder

#date_start <- as.Date("2002-03-01")
#date_end <- as.Date("2017-01-31")
#study_ids <- c([study IDs here])
#study_ids <- NULL
#cols_select <- c("fileName", "moh_study_id", "ad_date", "sep_date", 
#                 "hosp_to*", "hosp_from*",
#                 "TDAYS", "sep_disp", paste0("DIAGX", 1:25))
#filter_record_num <- FALSE
#episodes_of_care <- "DT_1day"

create_episodes <- function(data, filter_record_num = F,
                          episodes_of_care = "DT_1day"){
  
  # filter out duplicates
  if(filter_record_num == TRUE){
    if(!"RECRDNUM" %in% names(data)){
      stop("Cannot filter by RECRDNUM because column isn't present in data
       Either include RECRDNUM in selected cols, or set 'filter_record_num = F'")
    }
    data <- data[!duplicated(data$RECRDNUM),]
  } else{
    # remove duplicates of moh_study_id, admission date, discharge date
    data <- data[!duplicated(data[,c("moh_study_id", "ad_date", "sep_date")]),]
  }
  
  # coerce ad_date, sep_date to DATE 
  tryCatch({
    data <- data %>% mutate(ad_date = as.Date(ad_date),
                            sep_date = as.Date(sep_date)) 
    }, error = function(x){ stop("ad_date or sep_date cannot be coerced to date values")})
  
  # create episodes of care
  if(!episodes_of_care %in% c("DT_1day", "DT_1day_transfer", "DT_1day_transfer_hosp", "all")) {
    stop("episodes_of_care parameter not set to one of DT_1day, DT_1day_transfer, DT_1day_transfer_hosp, all")
  } else if(episodes_of_care == "DT_1day") {
    # nested/overlapping hospital admissions by >1 day
    # OR up to 1-day diff between admit vs prior discharge date
    data <- data %>% 
      group_by(moh_study_id) %>% 
      arrange(moh_study_id, sep_date, .by_group=TRUE) %>%
      mutate(DT_1day = replace_na(as.numeric(as.Date(ad_date) > (lag(as.Date(sep_date)) + 1)), 1)) %>% 
      ungroup() %>% 
      mutate(episode_of_care = cumsum(DT_1day)) %>% 
      select(-DT_1day)
  } else if(episodes_of_care == "DT_1day_transfer"){
    # nested/overlapping hospital admissions by >1 day
    # OR up to 1-day diff between admit vs prior discharge date
    # AND transfer flag present
    data <- data %>% 
      group_by(moh_study_id) %>% 
      arrange(moh_study_id, sep_date, .by_group=TRUE) %>%
      mutate(DT_1day_transfer = replace_na(as.numeric((ad_date > (lag(sep_date) + 1)) |
                                                        (!lag(sep_disp) %in% c("1","10","20"))), 1)) %>% 
      ungroup() %>% 
      mutate(episode_of_care = cumsum(DT_1day_transfer)) %>% 
      select(-DT_1day_transfer)
  } else if(episodes_of_care == "DT_1day_transfer_hosp"){
    # nested/overlapping hospital admissions by >1 day
    # OR up to 1-day diff between admit vs prior discharge date
    # AND transfer flag present
    # AND hosp_to* ID in record transferring patient 
    # equals hosp_from* ID in record receiving transferred patient
    data <- data %>% 
      group_by(moh_study_id) %>% 
      arrange(moh_study_id, sep_date, .by_group=TRUE) %>%
      mutate(DT_1day_transfer_hosp = replace_na(as.numeric((ad_date > (lag(sep_date) + 1)) |
                                                             (!lag(sep_disp) %in% c("1","10","20")) |
                                                             (lag(`hosp_to*`) != `hosp_from*`)), 1)) %>% 
      ungroup() %>% 
      mutate(episode_of_care = cumsum(DT_1day_transfer_hosp)) %>% 
      select(-DT_1day_transfer_hosp)
  } else{
    #create using all three criteria
    data <- data %>% 
      group_by(moh_study_id) %>% 
      arrange(moh_study_id, sep_date, .by_group=TRUE) %>%
      mutate(DT_1day = replace_na(as.numeric(ad_date > (lag(sep_date) + 1)), 1),
             DT_1day_transfer = replace_na(as.numeric((ad_date > (lag(sep_date) + 1)) |
                                                        (!lag(sep_disp) %in% c("1","10","20"))), 1),
             DT_1day_transfer_hosp = replace_na(as.numeric((ad_date > (lag(sep_date) + 1)) |
                                                             (!lag(sep_disp) %in% c("1","10","20")) |
                                                             (lag(`hosp_to*`) != `hosp_from*`)), 1)) %>% 
      ungroup() %>% 
      mutate(episode_care_DT_1day = cumsum(DT_1day),
             episode_care_DT_1day_transfer = cumsum(DT_1day_transfer),
             episode_care_DT_1day_transfer_hosp = cumsum(DT_1day_transfer_hosp)) %>% 
      select(-c(DT_1day, DT_1day_transfer, DT_1day_transfer_hosp))
    
  }
  
  return(data)
  
}




