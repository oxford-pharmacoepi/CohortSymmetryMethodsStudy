print("Getting cohort sequence postive controls")
info(logger, "Getting cohort sequence postive controls")
########################
# positive controls
########################

cdm[["amiodarone"]] <- cdm[["amiodarone"]] %>% 
  dplyr::mutate(cohort_definition_id = 1, cohort_name = "Amiodarone") %>% 
  dplyr::compute()

cdm[["levothyroxine"]] <- cdm[["levothyroxine"]] %>% 
  dplyr::mutate(cohort_definition_id = 2, cohort_name = "Levothyroxine") %>% 
  dplyr::compute()


cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "amiodarone_levothyroxine",
                                         dateRange = c(starting_date, ending_date),
                                         indexTable = "amiodarone",
                                         markerTable = "levothyroxine",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

amiodarone_levothyroxin <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                             outcomeTable = "amiodarone_levothyroxine") %>% 
  mutate(Analysis = "Positive Control",
         Index_name = cdm[["amiodarone"]] %>%  distinct(cohort_name) %>% 
           pull(),
         Marker_name = cdm[["levothyroxine"]] %>%  distinct(cohort_name) %>% 
           pull()
  )


##############################
# negative controls
##############################
print("Getting cohort sequence negative controls")
info(logger, "Getting cohort sequence negative controls")

cdm[["allopurinol"]] <- cdm[["allopurinol"]] %>% 
  dplyr::mutate(cohort_definition_id = 5, cohort_name = "Allopurinol") %>% 
  dplyr::compute()

#Amiodarone	Allopurinol
cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "amiodarone_allopurinol",
                                         dateRange = c(starting_date, ending_date),
                                         indexTable = "amiodarone",
                                         markerTable = "allopurinol",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

amiodarone_allopurinol <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                             outcomeTable = "amiodarone_allopurinol") %>% 
  mutate(Analysis = "Negative Control" ,     
         Index_name = cdm[["amiodarone"]] %>%  distinct(cohort_name) %>% 
           pull(),
         Marker_name = cdm[["allopurinol"]] %>%  distinct(cohort_name) %>% 
           pull()
  )


##############################
# drugs to test
##############################
print("Getting test drugs into one table")
info(logger, "Getting test drugs into one table")
# put all drugs to be tested into one table

# cdm[["test_drugs"]] <- union_all(cdm[["beta_blockers"]] %>% dplyr::mutate(cohort_definition_id = 1, cohort_name = "Beta Blockers"), 
#                                               cdm[["bladder_anticholinergics"]] %>% dplyr::mutate(cohort_definition_id = 2, cohort_name = "Bladder Anticholinergics")) %>% 
#   dplyr::compute()


# putting it into one table is too large therefore doing per table
# beta blockers ###########################################################
cdm[["beta_blockers_com"]] <- cdm[["beta_blockers"]] %>% 
  dplyr::mutate(cohort_definition_id = 6, cohort_name = "Beta Blockers") %>% 
  dplyr::compute()

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "ache_inhibitors_com_beta_blockers",
                                         dateRange = c(starting_date, ending_date),
                                         indexTable = "ache_inhibitors_com",
                                         markerTable = "beta_blockers_com",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

ache_inhibitors_com_beta_blockers <- CohortSymmetry::getSequenceRatios(cdm = cdm,
                                                           outcomeTable = "ache_inhibitors_com_beta_blockers") %>% 
  mutate(Analysis = "Test Drugs",         
         Index_name = cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
           pull(),
         Marker_name = cdm[["beta_blockers_com"]] %>%  distinct(cohort_name) %>% 
           pull()) %>% 
           readr::write_csv(file = paste0(here::here(output.folder),"/", cdm_name(cdm) ,"_" , cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
                                     pull() , "_" , cdm[["beta_blockers_com"]] %>%  distinct(cohort_name) %>% 
                                     pull() ,
    
    ".csv")) 







