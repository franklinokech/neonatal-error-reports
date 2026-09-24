##########################################################
#
# Project         : Error Reporting - Site Machines
# Author          : Boniface Makone
# Date            : 12-02-2016
# Organization    : KEMRI Wellcome Trust
# Department      : HSRG (Data & Analytics Team)
# Updated         : 16-03-2018 (Abraham Lagat)
############################################################
options(warn=-1)
#sink(file=stdout(),type = "output")
# Load packages
cat2=function(...) cat(...,sep="\n")
cat("Checking and/or installing required packages...")
if(!'RedcapData' %in% installed.packages())
  #devtools::install_github('tjhubs/RedcapData', ref = 'v7fixed', dep = TRUE)
  stop("RedcapData is not installed in this image. Rebuild with the Dockerfile.")
suppressPackageStartupMessages({
  library(RedcapData) 
  library(splitstackshape)
  library(data.table)
  library(stringr)
  library(plyr)
  library(digest)
  library(magrittr)
  library(dplyr, warn.conflicts = F)
  library(parallel)
})

cat2("Packages attached!")

# Input the names of the required parameters
end_date <- Sys.Date()
interval <- 14
start_date <- end_date - interval


# ==============================================================================
# start_date = as.Date('2018-11-24');end_date = as.Date('2018-12-14')

start_time <- Sys.time()
cat2("Error reporting approximately started at ", format(start_time, "%I:%M %p [%A %e %B, %Y]"))

cat2("Setting and validating global options...")
Sys.setenv(LD_LIBRARY_PATH="/usr/lib/libreoffice/program")

# Set global options — secrets and endpoint come from .env
api_url <- Sys.getenv("REDCAP_API_URL", unset = "https://nbo.kemri-wellcome.org/api/")
token   <- Sys.getenv("REDCAP_API_TOKEN")
if (!nzchar(token))
  stop("REDCAP_API_TOKEN is not set. Add it to .env and restart the container.")
local <- TRUE
chunked <- TRUE
chunk_size <- 100
id_var <- "id" # Check for pumwani and CIN id names: id_var, id respectively
date_var <- "date_today"
hosp_var <- "hosp_id"
surrogate_id_var <- "ipno" # Check for pumwani and CIN ipno names: patient_ipno, ipno respectively
hosp_to_validate <- 71
# if(exists('hospital')) hosp_to_validate <-  ifelse(hospital>0, hospital, NA)
#hosp_to_validate <- c(51,52,53, 54, 55, 57,58, 62, 63, 64, 66, 68, 70, 71)
gui <- TRUE

if (chunked) stopifnot(is.numeric(chunk_size) && chunk_size > 0)
chunk_size <- as.integer(chunk_size)

cat2("Global options OK!")

# Set file paths

cat2("File system checking...")

# Locate the position of the running script
path_to_files <- Sys.getenv("PROJECT_ROOT", unset = getwd())
cat2("File location: ",path_to_files, "\n")

file_system_checks <- function(file_path, create = FALSE, verify = TRUE, dir = FALSE, ...) {
  if (create && !file.exists(file_path)) {
    if (!file.exists(basename(file_path)))
      stopifnot(dir.create(basename(file_path), recursive = T))
    stopifnot(if (dir) dir.create(file_path) else file.create(file_path))
  }
  if (verify) stopifnot(file.exists(file_path))
  normalizePath(file_path)
}
timescript_location <- file_system_checks(file.path(path_to_files, "time validation.R"), verify = FALSE)
updates_location <- file_system_checks(file.path(path_to_files, "upd.csv"), verify = FALSE) 
custom_code_location <- file_system_checks(file.path(path_to_files, "custom-neonatal.R"), verify = FALSE) # BOO
# custom_code_location <- file_system_checks(file.path(path_to_files, "custom-neonatal-update.R"), verify = FALSE) # BOO
exclusion_patterns_location <- file_system_checks(file.path(path_to_files, "exclude.R"), verify = FALSE)
report_location <- file.path(path_to_files, "tmp","NeonatalErrorReport.csv")


cat2("File system OK!")

# Running the application

cat2("Running the application...")

# populate the global namespace with the regular expressions matching the exclusion criteria
source(exclusion_patterns_location)
source(timescript_location)

# get ids and date of entry for data entered between the start and end period
ids_matching_dates <- tbl_df(get_redcap_data(
  api = api_url, 
  token = token, 
  content = "record", 
  local = local ,
  fields = c(id_var, date_var, hosp_var)) 
)
# Check connection to SQL
# if(grepl('combination.could.not.connect.to.the.MySQL.server.', names(ids_matching_dates))){
#     cat('Check connection to your SQL database!...')
#   }else{
names(ids_matching_dates) <- c("id", "hosp_id", "date")

ids_matching_dates %<>%
  mutate(date = as.Date(date)) %>%
  filter(date >= start_date & date <= end_date) 
if(!is.na(hosp_to_validate))  
  ids_matching_dates<- ids_matching_dates %>% filter(as.character(hosp_id)==as.character(hosp_to_validate))

ids_matching_dates <- ids_matching_dates$id

if ((data_size <- length(ids_matching_dates)) == 0)
  stop("No data in the selected period!")

cat2(sprintf("%d records to validate...", data_size))

# get data and metadata
cat2("Getting & cleaning metadata...")
meta_data <- tbl_df(get_redcap_data(
  api = api_url, 
  token = token, 
  content = "metadata", 
  local = local
))

# Clean metadata

meta_data =  meta_data %>%
  mutate(branching_logic = ifelse(grepl("\\band\\b", branching_logic) & grepl("hosp_id", branching_logic), sub("and.*", "",branching_logic), branching_logic)) %>%
  mutate(branching_logic =  ifelse(field_name =="in_bil_hi",  
                                   "[phototherapy] = '1' or [photo_therapy_on_any_other] = '1' " , branching_logic))



meta_data %<>%
  filter(
    field_type != "descriptive"
  ) %>%
  mutate(required_field = ifelse(field_type == "checkbox", TRUE, required_field)) %>%
  mutate_all(~gsub('[^ -~]', '', .)) %>%
  mutate_all(funs(remove_empties = ifelse((is.na(.) | str_trim(.) == ""), NA, .)))
#xxxx WHERE IS exclusion_patterns IMPORT FROM
# if (exists("exclusion_patterns") && length(na.omit(exclusion_patterns))) {
#   meta_data %<>%
#     mutate(`..to_exclude..` = grepl(paste0(str_trim(exclusion_patterns), collapse="|"), field_name)) %>%
#     filter(!`..to_exclude..`) %>%
#     select(-`..to_exclude..`)
# }

if (exists("exclusionPatterns") && length(na.omit(exclusionPatterns))) {
  meta_data %<>%
    mutate(`..to_exclude..` = grepl(paste0(str_trim(exclusionPatterns), collapse="|"), field_name)) %>%
    filter(!`..to_exclude..`) %>%
    select(-`..to_exclude..`)
}

cat2("Metadata in memory!")

cat2("Pulling data from REDCap...")
if (chunked) {
  data_to_validate <- get_redcap_data(
    api = api_url, 
    token = token, 
    content = "record", 
    local = local, 
    ids_to_pull = ids_matching_dates
  )
} else {
  get_chunked_redcap_data(
    dataset_name = "data_to_validate",
    metadataset_name = "..meta_tmp..",
    api = api_url, 
    token = token, 
    content = "record", 
    local = local, 
    ids_to_pull = ids_matching_dates
  )
}

data_to_validate <- tbl_df(data_to_validate)

cat2("Data in memory!")

data_to_validate <- tbl_df(data_to_validate)
data_to_validate <- as.data.table((data_to_validate))
meta_data<- as.data.table(meta_data)
metadata.formatted<- format_branching_logics(metadata = meta_data, data = data_to_validate)


cat2("Data in memory!")

# Generating validation function code
cat2("Generating data validation code...")
# getting custom logic
if (file.exists(custom_code_location)) {
  custom_code <- readLines(custom_code_location, warn = F)
  custom_code <- custom_code[!(is.na(custom_code) | custom_code == "")]
  custom_code <- if (0 == length(custom_code)) NA else custom_code
} else {
  custom_code <- NA
}

# getting updates
updates <- if (file.exists(updates_location)) 
  RedcapData:::load_updates(read.csv(updates_location, stringsAsFactors = F)) else 
    list()

# generate code and evaluate in current namespace
data_validation_code <- generate_data_validation_code(
  metadata.formatted, 
  date_var, 
  hosp_var, 
  custom_code = custom_code, 
  surrogate_id_var = surrogate_id_var,
  updates = "updates", 
  updates_envir_depth = 1
)
# if(any(grepl("weight =\"-1\"", data_validation_code)) | 
#    any(grepl("weight = \"-1\"", data_validation_code))){
#   data_validation_code<-gsub("weight = \"-1\"", "weight ==-1",data_validation_code)
#   data_validation_code<-gsub("weight =\"-1\"", "weight ==-1",data_validation_code)
# }
data_validation_code <- gsub("date_adm <= date_of_birth", "as.Date(date_of_birth)>=as.Date(date_adm)",data_validation_code)
data_validation_code <- gsub("datediff( [date_of_birth] ,[date_adm],\"d\") >=7", "(as.Date(date_adm)-as.Date(date_of_birth))>=7",data_validation_code)



eval(parse(text = data_validation_code))

cat2("Data validation code generated!")

# validate data entry

cat2("Validating data (This might take a while)...")
#Replace -1 with .1 for field values with checkboxes
#names(data_to_validate) <- gsub('____','___.',names(data_to_validate))
data_to_validate <- data.table(data_to_validate)
data_to_validate <- data_to_validate[, key:= .I]
setkey(data_to_validate, key)
# Running the error report where applicable
# if (1 < length(chunks <- get_chunks(seq_len(nrow(data_to_validate)), chunk_size)) && chunked) {
#   message("Validating in parallel (2 nodes per core)...")
# tryCatch({
#   message(sprintf("** %d nodes spawned **", detectCores() * 2))
#   cl <- makeCluster(min(length(chunks),detectCores()*2)) # initialize 2 slaves (nodes) per core
#   
#   invisible(clusterEvalQ(cl, {
#     # load relevant packages to cluster
#     library(data.table)
#     library(stringr)
#     library(RedcapData)
#     return(NULL)
#   }))
#   
#   clusterExport(cl, c("validate_data_entry", "data_to_validate", "hosp_to_validate", "updates")) # export variables to nodes
#   # Execute code in parallel
#   error_report <- clusterApplyLB(
#     cl,
#     chunks,
#     function(x) {
#       data_to_validate[x, validate_data_entry(.SD, hosp_to_validate = hosp_to_validate, updates = updates), by=key]
#     }
#   ) %>% rbindlist()
# }, error = function(e) {
#   stop(sprintf("An error has occured.\nDetails:\n%s", paste0(e$message, collapse = "\n")))
# }, finally = {
#   if (exists("cl")) {
#     stopCluster(cl)
#     rm(cl)
#   }
# })   
# message("DONE!")
# } else {
#   error_report <- data_to_validate[, validate_data_entry(.SD, hosp_to_validate = hosp_to_validate, updates = updates), by=key]
# }
# 
# # for(x in c(112:126)){
# # a=validate_data_entry(data_to_validate[x,], hosp_to_validate = hosp_to_validate, updates = updates); cat('--------------\n',x,"\n");cat(a,"\n")
# # }

#if (nrow(error_report)==0) stop("No errors in the specified period")



conditional_variables<-metadata.formatted[branching_logic!=""
                                          & field_type!='descriptive'
                                          & required_field!="y" 
                                          & (is.na(text_validation_max) | text_validation_max=='')
                                          & (text_validation_min=='' | is.na(text_validation_min))
                                          , (field_name)]

# validate groupwise (get variables by pattern)
admissionDiag<-conditional_variables[grepl('^adm_diag|^other_admission_diag\\_|admisn_diag_not_listed', conditional_variables)]
dischDiag<-conditional_variables[grepl('^disch_dia|other_discharge_diag|any_other_disch_diag|01
                                       other_discharge_diag_unlisted', conditional_variables)]
trtment<- conditional_variables[grepl('^other_drugs', conditional_variables)]
#dischargeTrt<- conditional_variables[grepl('^dsc\\_rx[0-9]{1,}|^dsc\\_rx\\_[a-z]', conditional_variables)]
nonGrouping<- conditional_variables[!(conditional_variables %in% c(admissionDiag
                                                                   , dischDiag,
                                                                   trtment
)) ]


if(dim(data_to_validate)[1]>0){
  cat("Validating batch 1\n")
  # Running the error report where applicable
  cl<-makeForkCluster(detectCores())
  
  error_report1<-parSapply(cl,1:nrow(data_to_validate),function(x){
    result<-tryCatch(validate_data_entry(data_to_validate[x,], hosp_to_validate = hosp_to_validate, updates = updates, metadata=metadata.formatted),
                     error=function(e) data.frame(RecordID=data_to_validate[x,]$id,Type="Error in script",Message=as.character(e)))
    return(result)
    
  },simplify = F) %>% bind_rows()
  # Manually remove updated variables
  if(nrow(error_report1) & nrow(read.csv(updates_location, header = T))){
    error_report1 = merge(as.data.frame(error_report1),
                          read.csv(updates_location, header = T) %>%
                            subset(., select = c(date, new_vars)) %>%
                            splitstackshape::cSplit(., "new_vars", ";",direction = "long") %>%
                            plyr::rename(.,c(new_vars ='Variable')) %>% as.data.frame(.), 
                          by = "Variable",all.x=T) %>%
      subset(., (is.na(as.Date(date, '%Y-%m-%d')) | as.Date(DateOfEntry, '%Y-%m-%d') > as.Date(date, '%Y-%m-%d')) &
               !duplicated(paste0(RecordID, Variable,Message)),
             select = c("RecordID","Identifier","DateOfEntry","Hospital","Form","Section","Variable","Type","Entry","Message")) %>%
      merge(., subset(meta_data, select =c('field_name','field_annotation')) %>%
              plyr::rename(., c(field_name='Variable')), all.x = T, by = 'Variable') %>%
      subset(., !grepl('@hidden', tolower(field_annotation)), select = -field_annotation) %>%
      # differentiate duplicated variables (more that one error in a variable!)
      mutate(., Variable = ifelse(duplicated(paste0(as.character(RecordID), as.character(Variable))), make.unique(as.character(Variable)), Variable)) %>%
      merge(., (subset(data_to_validate, select =c(id, t_sheet)) %>% plyr::rename(., c(id='RecordID'))), by = 'RecordID', all.x = TRUE) %>%
      subset(.,!(as.numeric(t_sheet)!=1 & Variable %in% c('pen','genta','amp','ceftr','other_treatment','other_treatment_2')),select = -t_sheet)
  }
  # differentiate duplicates
  
  
  
  # 
  #     
  # #=================================================================================================================================
  # # Identify IDs with concordant values for: date_adm,date_discharge,outcome,oxygen_sat,resp_rate,pulse_rate,temp
  # #=================================================================================================================================
  # 
  # cat2("Data validated!")
  # 
  # # Save error report and pop it open using default system app
  # cat2("\tSaving error report where applicable...!\n")
  # if(dim(error_report)[1]>0){
  #    report_location <- file.path(path_to_files, "tmp","CIN Paeds Hospital-ErrorReport.csv")
  #    write.csv(error_report, file=report_location, row.names = FALSE)
  # }
  # }
  
  #report_location <- output_file
  
  error_report1 = as.data.frame(error_report1)%>% 
    subset(., !Variable %in% c("other_fluid_2",'date_feeds_first_presc','total_volume_of_iv_fluids','other_fluid','duration_of_iv_fluid_presc',	
                               'intravenous_fluids_presc','date_fluid_presc', 'referred_from_which_facili', 'referred_from_which_facili_othr'))
  
  cat("Validating batch 2\n")
  
  #  
  
  error_report2<-parSapply(cl,1:nrow(data_to_validate),function(x){
    result<-tryCatch(validate_data_in_branching_logic(data_to_validate[x,], ipno_var =surrogate_id_var
                                                      ,dateOfEntry_var =date_var
                                                      ,recordID_var =id_var
                                                      ,hospitalID_var = hosp_var
                                                      ,individual.vars = 'nonGrouping'
                                                      ,metadataName = 'metadata.formatted'
                                                      ,dataName = 'data_to_validate'
                                                      ,validateTreatmentDates = T
                                                      ,treatmentFormName = "drug_treatment"
                                                      ,dischargeDateVar = "date_discharge"
                                                      ,admissionDateVar = 'date_adm'
                                                      ,n.groups = 3
                                                      ,group.names = c('admissionDiag', 'dischDiag','trtment')),
                     error=function(e) data.frame(RecordID=data_to_validate[x,]$id,Type="Error in script",Message=as.character(e)))
    return(result)
    
  },simplify = F) %>% bind_rows()
  #if(as.Date(error_report2$DateOfEntry) <= "2020-02-05"){
  error_report2 = as.data.frame(error_report2)%>% 
    subset(., !Variable %in% c('multiple_delivery','number_delivered','last_menstrual_period','rapture_of_membrane','abnormalities_1',
                               'date_feeds_only_presc','neo_std_monit_chart_pre','neo_intnsv_monit_chart_pre','oxygen_sat_monitored','no_of_times_oxy_monitored',
                               'cyanosis_assessed','no_times_cyanosis_assessed','ivf_type_day0_doc','ivf_vol_day0_doc','ivf_summ_tot_day0_doc','ivf_type_day1_doc',
                               'ivf_vol_day1_doc','ivf_summ_tot_day1_doc','time_feeds_started','feed_type_day0_doc','feed_vol_day0_doc','feed_summ_tot_day0_doc',
                               'feed_type_day1_doc','feed_vol_day1_doc','feed_summ_tot_day1_doc','vital_signs_monitored_in_t','post_weight_date1','postdischarge_weight_1',
                               'post_weight_date2','postdischarge_weight_2','post_weight_date4','postdischarge_weight_3', 
                               'post_weight_date3','postdischarge_weight_4','post_weight_date5','postdischarge_weight_5','post_weight_date6','postdischarge_weight_6', 
                               'fluids_presc_next_day','feeds_presc_next_day','primary_disch_diagnosis','pry_adm_diag', 'post_weight_unit1', 'post_weight_unit2', 
                               'post_weight_unit3', 'post_weight_unit4', 'post_weight_unit5', 'post_weight_unit6', 'post_discharge_weight_other',
                               'referred_from_which_facili_othr','phenobarb_start','phenobarb_stop','aminophylline_start','aminophylline_stop','caffeine_start',
                               'caffeine_stop','other_drugs_1_start','other_drugs_1_stop','other_drugs_2_start','other_drugs_2_stop','other_drugs_3_start','other_drugs_3_stop',
                               'other_drugs_4_start','other_drugs_4_stop','other_drugs_5_start','other_drugs_5_stop','drugs','drugs_1','drugs_1_start','drugs_1_stop','drugs_2','drugs_2_start',
                               'drugs_2_stop','drugs_3','drugs_3_start','drugs_3_stop','drugs_4','drugs_4_start','drugs_4_stop','drugs_5','drugs_5_start','drugs_5_stop','drugs_6','drugs_6_start',
                               'drugs_6_stop','drugs_7','drugs_7_start','drugs_7_stop','drugs_8','drugs_8_start','drugs_8_stop','drugs_9','drugs_9_start','drugs_9_stop','drugs_10','drugs_10_start',
                               'drugs_10_stop','other_post_admission_drugs','freq_of_administration_2','mechanical_ventilation2','referred_to_othr'))
  #}
  
  # Historically, hospital 70 kept 'crp_done' in the report. That hospital
  # no longer exists, so 'crp_done' is always filtered out.
  if (nrow(error_report1) > 0) {
    error_report1 <- error_report1 %>%
      filter(!Variable %in% c('crp_done'))
  }
  
  
  if (exists("error_report2") & dim(error_report2)[1] > 0 ){
    if(as.Date(error_report2$DateOfEntry) <= as.Date("2020-07-07") & as.numeric(error_report2$Hospital) %in% c(53,58,41)){
      error_report2 = error_report2 %>%
        filter(!Variable %in% c('total_input','d1_present','other_maternal_cond','baby_feeding_disch','nxtday_feed_rt_presc', 'referral_info_available','type_feed_presc_next_day','total_fluids_next_day','total_feeds_presc_next_day','is_other_maternal_cond','mother_anten_corti','mother_alive','baby_resusc_at_birth','in_gis_avail_ke','in_gis_doc_ke','in_gis_subc','in_gis_ward','in_gis_loc','in_gis_subloc','in_gis_school','in_gis_hosp','in_gis_add','weight_1_units','weight_2_units','weight_3_units','weight_4_units','weight_5_units','weight_6_units','weight_7_units','weight_8_units','weight_9_units','weight_10_units','weight_11_units','weight_12_units','weight_13_units','weight_14_units','weight_15_units','weight_16_units','weight_17_units','weight_18_units','weight_19_units','weight_20_units','transfer_form','mother_fever','mother_tbtreat','mother_diabetes','mother_htn','mother_preeclampsia','mother_eclampsia','in_othfac_nam','in_bil_hi','phenobarb','aminophylline','caffeine_citrate','oxygen_admin','date_feeds_first_presc','in_chloro','in_vitk', 'bilirubin_hi_unit'))
    } else{
      error_report2 = error_report2 %>%
        filter(!Variable %in% c('total_input','d1_present','other_maternal_cond','baby_feeding_disch','nxtday_feed_rt_presc', 'referral_info_available','type_feed_presc_next_day','total_fluids_next_day','total_feeds_presc_next_day','is_other_maternal_cond','mother_anten_corti','mother_alive','baby_resusc_at_birth','in_gis_avail_ke','in_gis_doc_ke','in_gis_subc','in_gis_ward','in_gis_loc','in_gis_subloc','in_gis_school','in_gis_hosp','in_gis_add','weight_1_units','weight_2_units','weight_3_units','weight_4_units','weight_5_units','weight_6_units','weight_7_units','weight_8_units','weight_9_units','weight_10_units','weight_11_units','weight_12_units','weight_13_units','weight_14_units','weight_15_units','weight_16_units','weight_17_units','weight_18_units','weight_19_units','weight_20_units','transfer_form','mother_fever','mother_tbtreat','mother_diabetes','mother_htn','mother_preeclampsia','mother_eclampsia','in_othfac_nam','in_bil_hi','phenobarb','aminophylline','caffeine_citrate','oxygen_admin','date_feeds_first_presc','in_chloro','in_vitk', 'bilirubin_hi_unit'))
    }
    
  }  
  
  error_report2<- as.data.table(error_report2)
  error_report1<- as.data.table(error_report1)
  
  error_report2[, key:=NULL]
  
  error_report2[, DateOfEntry:=as.character(DateOfEntry)]
  error_report1[, DateOfEntry:=as.character(DateOfEntry)]
  
  error_report=data.table::rbindlist(list(error_report1, error_report2), use.names = T, fill = T)
  error_report=unique(error_report
                      , by=c('Variable', 'RecordID'))
  
  
  write.csv(error_report, file=report_location, row.names = FALSE)
if (interactive() || nzchar(Sys.getenv("DISPLAY"))) {
  system(paste0('gio open "', report_location,'"'), intern = FALSE)
} else {
  cat2(sprintf("Report written to %s", report_location))
}
  cat2(sprintf("Error report located in \"%s\"!", report_location))
}

end_time <- Sys.time()
time_diff <-  difftime(end_time, start_time, units = "mins") %>%  as.numeric() %>% `*`(60) %>% round(0)
time_diff <- sprintf("%d minutes, %d seconds", (time_diff %/% 60), (time_diff %% 60))

cat2(sprintf("Error reporting approximately ended at %s!", format(end_time, "%I:%M %p [%A %e %B, %Y]")))
cat2(sprintf("The session took approximately %s", time_diff))

cat2("DONE!")