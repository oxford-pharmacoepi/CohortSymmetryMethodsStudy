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

tictoc::tic()
cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "amiodarone_levothyroxine",
                                         dateRange = c(starting_date, ending_date),
                                         indexTable = "amiodarone",
                                         markerTable = "levothyroxine",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

getCohortSeqtime <- tictoc::toc()$callback_msg

tictoc::tic()
amiodarone_levothyroxin <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                             outcomeTable = "amiodarone_levothyroxine") %>% 
  mutate(Analysis = "Positive Control",
         Index_name = cdm[["amiodarone"]] %>%  distinct(cohort_name) %>% 
           pull(),
         Marker_name = cdm[["levothyroxine"]] %>%  distinct(cohort_name) %>% 
           pull()
  )

getSeqRatioTime <- tictoc::toc()$callback_msg

# put the results into the object
amiodarone_levothyroxin <- amiodarone_levothyroxin %>% 
  mutate(CohortSequenceTime = getCohortSeqtime,
         SeqRatioTime = getSeqRatioTime)

##############################
# negative controls
##############################
print("Getting cohort sequence negative controls")
info(logger, "Getting cohort sequence negative controls")

cdm[["allopurinol"]] <- cdm[["allopurinol"]] %>% 
  dplyr::mutate(cohort_definition_id = 5, cohort_name = "Allopurinol") %>% 
  dplyr::compute()

#Amiodarone	Allopurinol
tictoc::tic()
cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "amiodarone_allopurinol",
                                         dateRange = c(starting_date, ending_date),
                                         indexTable = "amiodarone",
                                         markerTable = "allopurinol",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)
getCohortSeqtime <- tictoc::toc()$callback_msg

tictoc::tic()
amiodarone_allopurinol <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                             outcomeTable = "amiodarone_allopurinol") %>% 
  mutate(Analysis = "Negative Control" ,     
         Index_name = cdm[["amiodarone"]] %>%  distinct(cohort_name) %>% 
           pull(),
         Marker_name = cdm[["allopurinol"]] %>%  distinct(cohort_name) %>% 
           pull()
  )

getSeqRatioTime <- tictoc::toc()$callback_msg

amiodarone_allopurinol <- amiodarone_allopurinol %>% 
  mutate(CohortSequenceTime = getCohortSeqtime,
         SeqRatioTime = getSeqRatioTime)

benchmarkers <- bind_rows(
  amiodarone_levothyroxin,
  amiodarone_allopurinol
)

# save results
readr::write_csv(benchmarkers, 
                 paste0(here::here(output_folder),"/", cdm_name(cdm), "_21224_SSA_benchmarkers.csv"))

##############################
# drugs to test with gi
##############################
# dorzolamide # neg
# eszopiclone # neg
# fexofenadine # neg
# goserelin # neg
# simvastatin # neg
# aspirin # pos
# heparin # pos
# ibuprofen # pos
# indomethacin # pos
# prednisolone # pos

# put all drugs to be tested into one table
cdm[["test_drugs"]] <- union_all(cdm[["dorzolamide"]] %>% dplyr::mutate(cohort_definition_id = 1, cohort_name = "dorzolamide"),
                                              cdm[["eszopiclone"]] %>% dplyr::mutate(cohort_definition_id = 2, cohort_name = "eszopiclone")) %>%
  dplyr::compute()

cdm[["test_drugs"]] <- union_all(cdm[["test_drugs"]],
                                              cdm[["fexofenadine"]] %>% dplyr::mutate(cohort_definition_id = 3, cohort_name = "fexofenadine")) %>% 
  dplyr::compute()

cdm[["test_drugs"]] <- union_all(cdm[["test_drugs"]],
                                 cdm[["goserelin"]] %>% dplyr::mutate(cohort_definition_id = 4, cohort_name = "goserelin")) %>% 
  dplyr::compute()

cdm[["test_drugs"]] <- union_all(cdm[["test_drugs"]],
                                 cdm[["simvastatin"]] %>% dplyr::mutate(cohort_definition_id = 5, cohort_name = "simvastatin")) %>% 
  dplyr::compute()

cdm[["test_drugs"]] <- union_all(cdm[["test_drugs"]],
                                 cdm[["aspirin"]] %>% dplyr::mutate(cohort_definition_id = 6, cohort_name = "aspirin")) %>% 
  dplyr::compute()

cdm[["test_drugs"]] <- union_all(cdm[["test_drugs"]],
                                 cdm[["heparin"]] %>% dplyr::mutate(cohort_definition_id = 7, cohort_name = "heparin")) %>% 
  dplyr::compute()

cdm[["test_drugs"]] <- union_all(cdm[["test_drugs"]],
                                 cdm[["ibuprofen"]] %>% dplyr::mutate(cohort_definition_id = 8, cohort_name = "ibuprofen")) %>% 
  dplyr::compute()

cdm[["test_drugs"]] <- union_all(cdm[["test_drugs"]],
                                 cdm[["indomethacin"]] %>% dplyr::mutate(cohort_definition_id = 9, cohort_name = "indomethacin")) %>% 
  dplyr::compute()

cdm[["test_drugs"]] <- union_all(cdm[["test_drugs"]],
                                 cdm[["prednisolone"]] %>% dplyr::mutate(cohort_definition_id = 10, cohort_name = "prednisolone")) %>% 
  dplyr::compute()

cdm[["test_drugs"]] <- union_all(cdm[["test_drugs"]],
                                 cdm[["eszopiclone"]] %>% dplyr::mutate(cohort_definition_id = 11, cohort_name = "eszopiclone")) %>% 
  dplyr::compute()


cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "gi_benchmarkers",
                                         dateRange = c(starting_date, ending_date),
                                         indexTable = "test_drugs",
                                         markerTable = "upper_gi_ulcer",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

upper_gi_ulcer_benchmarkers <- CohortSymmetry::getSequenceRatios(cdm = cdm,
                                                                       outcomeTable = "gi_benchmarkers") %>% 
  mutate(Analysis = "Test Drugs",         
         Index_name = cdm[["test_drugs"]] %>%  distinct(cohort_name) %>% 
           pull(),
         Marker_name = cdm[["upper_gi_ulcer"]] %>%  distinct(cohort_name) %>% 
           pull()) 

# %>% 
#   readr::write_csv(file = paste0(here::here(output.folder),"/", cdm_name(cdm) ,"_" , cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
#                                    pull() , "_" , cdm[["beta_blockers_com"]] %>%  distinct(cohort_name) %>% 
#                                    pull() ,
#                                  
#                                  ".csv")) 

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







