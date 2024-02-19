print("Getting cohort sequence postive controls")
info(logger, "Getting cohort sequence postive controls")
########################
# positive controls
########################

cdm[["amiodarone"]] <- cdm[["amiodarone"]] %>% 
  dplyr::mutate(cohort_definition_id = 1, cohort_name = "Amiodarone") %>% 
  CDMConnector::computeQuery()

cdm[["levothyroxine"]] <- cdm[["levothyroxine"]] %>% 
  dplyr::mutate(cohort_definition_id = 2, cohort_name = "Levothyroxine") %>% 
  CDMConnector::computeQuery()


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


# all AChE inhibitors combined > Memantine
cdm[["ache_inhibitors_com"]] <- cdm[["ache_inhibitors"]] %>% 
  dplyr::mutate(cohort_definition_id = 3, cohort_name = "AChE Inhibitors") %>% 
  CDMConnector::computeQuery()

cdm[["memantine"]] <- cdm[["memantine"]] %>% 
  dplyr::mutate(cohort_definition_id = 4, cohort_name = "Memantine") %>% 
  CDMConnector::computeQuery()
  
cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "ache_inhibitors_com_memantine",
                                         dateRange = c(starting_date, ending_date),
                                         indexTable = "ache_inhibitors_com",
                                         markerTable = "memantine",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

ache_inhibitors_com_memantine <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                               outcomeTable = "ache_inhibitors_com_memantine") %>% 
  mutate(Analysis = "Positive Control",           
         Index_name = cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
           pull(),
         Marker_name = cdm[["memantine"]] %>%  distinct(cohort_name) %>% 
           pull()
  )

print("Got cohort sequence postive controls")
info(logger, "Got cohort sequence postive controls")

##############################
# negative controls
##############################
print("Getting cohort sequence negative controls")
info(logger, "Getting cohort sequence negative controls")

cdm[["allopurinol"]] <- cdm[["allopurinol"]] %>% 
  dplyr::mutate(cohort_definition_id = 5, cohort_name = "Allopurinol") %>% 
  CDMConnector::computeQuery()

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


# save results and output
controls <- bind_rows(
  amiodarone_levothyroxin ,
  ache_inhibitors_com_memantine ,
  amiodarone_allopurinol
) 
  
readr::write_csv(controls, paste0(here::here(output.folder),"/", cdm_name(cdm), "_control_estimates.csv"))


# #Thyroxine	Allopurinol
# cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
#                                          name = "levothyroxine_allopurinol",
#                                          dateRange = c(starting_date, ending_date),
#                                          indexTable = "levothyroxine",
#                                          markerTable = "allopurinol",
#                                          daysPriorObservation = 365,
#                                          indexWashout = 365,
#                                          markerWashout = 365,
#                                          timeGap = 365)
# 
# levothyroxine_allopurinol <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
#                                                             outcomeTable = "levothyroxine_allopurinol")

# not a negative control as evidence suggests thyroxine can cause gout in short term
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5388204/#:~:text=Current%20short%2Dterm%20treatment%20of,)%2C%20compared%20with%20no%20treatment.
print("Got cohort sequence negative controls")
info(logger, "Got cohort sequence negative controls")

##############################
# drugs to test
##############################
print("Getting test drugs into one table")
info(logger, "Getting test drugs into one table")
# put all drugs to be tested into one table

# cdm[["test_drugs"]] <- union_all(cdm[["beta_blockers"]] %>% dplyr::mutate(cohort_definition_id = 1, cohort_name = "Beta Blockers"), 
#                                               cdm[["bladder_anticholinergics"]] %>% dplyr::mutate(cohort_definition_id = 2, cohort_name = "Bladder Anticholinergics")) %>% 
#   CDMConnector::computeQuery()


###########################################
# run analysis
###########################################
print("Getting cohort sequence test drugs")
info(logger, "Getting cohort sequence test drugs")

# for ache_inhibitors > test drugs
# cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
#                                          name = "ache_inhibitors_com_test_drugs",
#                                          dateRange = c(starting_date, ending_date),
#                                          indexTable = "ache_inhibitors_com",
#                                          markerTable = "test_drugs",
#                                          daysPriorObservation = 365,
#                                          indexWashout = 365,
#                                          markerWashout = 365,
#                                          timeGap = 365)
# 
# ache_inhibitors_com_test_drugs <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
#                                                            outcomeTable = "ache_inhibitors_com_test_drugs")


# # for memantine > test drugs
# cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
#                                          name = "memantine_test_drugs",
#                                          dateRange = c(starting_date, ending_date),
#                                          indexTable = "memantine",
#                                          markerTable = "test_drugs",
#                                          daysPriorObservation = 365,
#                                          indexWashout = 365,
#                                          markerWashout = 365,
#                                          timeGap = 365)
# 
# memantine_test_drugs <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
#                                                                 outcomeTable = "memantine_test_drugs")

# putting it into one table is too large therefore doing per table
# beta blockers ###########################################################
cdm[["beta_blockers_com"]] <- cdm[["beta_blockers"]] %>% 
  dplyr::mutate(cohort_definition_id = 6, cohort_name = "Beta Blockers") %>% 
  CDMConnector::computeQuery()

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

# bladder anticholergics #########################################
cdm[["bladder_anticholinergics_com"]] <- cdm[["bladder_anticholinergics"]] %>% 
  dplyr::mutate(cohort_definition_id = 7, cohort_name = "Bladder anticholinergics") %>% 
  CDMConnector::computeQuery()

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "ache_inhib_bladder_anticholiner",
                                         dateRange = c(starting_date, ending_date),
                                         indexTable = "ache_inhibitors_com",
                                         markerTable = "bladder_anticholinergics_com",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

ache_inhib_bladder_anticholiner <- CohortSymmetry::getSequenceRatios(cdm = cdm,
                                                                       outcomeTable = "ache_inhib_bladder_anticholiner") %>% 
  mutate(Analysis = "Test Drugs",         
         Index_name = cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
           pull(),
         Marker_name = cdm[["bladder_anticholinergics_com"]] %>%  distinct(cohort_name) %>% 
           pull()
  ) %>% 
  readr::write_csv(file = paste0(here::here(output.folder),"/", cdm_name(cdm) ,"_" , cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() , "_" , cdm[["bladder_anticholinergics_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() ,
                                 
                                 ".csv")) 


# ccb  ##########################################
cdm[["ccb_com"]] <- cdm[["ccb"]] %>% 
  dplyr::mutate(cohort_definition_id = 8, cohort_name = "Calcium Channel Blockers") %>% 
  CDMConnector::computeQuery()

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "ache_inhib_bladder_ccb",
                                         dateRange = c(starting_date, ending_date),
                                         indexTable = "ache_inhibitors_com",
                                         markerTable = "ccb_com",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

ache_inhib_ccb <- CohortSymmetry::getSequenceRatios(cdm = cdm, outcomeTable = "ache_inhib_bladder_ccb") %>% 
  mutate(Analysis = "Test Drugs",         
         Index_name = cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
           pull(),
         Marker_name = cdm[["ccb_com"]] %>%  distinct(cohort_name) %>% 
           pull()
  ) %>% 
  readr::write_csv(file = paste0(here::here(output.folder),"/", cdm_name(cdm) ,"_" , cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() , "_" , cdm[["ccb_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() ,
                                 
                                 ".csv")) 


# diuretics #####################################
cdm[["diuretics_com"]] <- cdm[["diuretics"]] %>%
  dplyr::mutate(cohort_definition_id = 9, cohort_name = "Diuretics") %>% 
  CDMConnector::computeQuery()

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "ache_inhib_diuretics",
                                         dateRange = c(starting_date, ending_date),
                                         indexTable = "ache_inhibitors_com",
                                         markerTable = "diuretics_com",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

ache_inhib_diuretics <- CohortSymmetry::getSequenceRatios(cdm = cdm, outcomeTable = "ache_inhib_diuretics") %>% 
  mutate(Analysis = "Test Drugs",         
         Index_name = cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
           pull(),
         Marker_name = cdm[["diuretics_com"]] %>%  distinct(cohort_name) %>% 
           pull()
  ) %>% 
  readr::write_csv(file = paste0(here::here(output.folder),"/", cdm_name(cdm) ,"_" , cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() , "_" , cdm[["diuretics_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() ,
                                 
                                 ".csv")) 


# ace inhibitors #################################################
cdm[["ace_inhibitors_com"]] <- cdm[["ace_inhibitors"]] %>%
  dplyr::mutate(cohort_definition_id = 10, cohort_name = "ACE Inhibitors") %>% 
  CDMConnector::computeQuery()

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "ache_inhib_ace",
                                         dateRange = c(starting_date, ending_date),
                                         indexTable = "ache_inhibitors_com",
                                         markerTable = "ace_inhibitors_com",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

ache_inhib_ace <- CohortSymmetry::getSequenceRatios(cdm = cdm, outcomeTable = "ache_inhib_ace") %>% 
  mutate(Analysis = "Test Drugs",         
         Index_name = cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
           pull(),
         Marker_name = cdm[["ace_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
           pull()
  ) %>% 
  readr::write_csv(file = paste0(here::here(output.folder),"/", cdm_name(cdm) ,"_" , cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() , "_" , cdm[["ace_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() ,
                                 
                                 ".csv")) 


# arbs ##################################################
cdm[["arbs_com"]] <- cdm[["arbs"]] %>% dplyr::mutate(cohort_definition_id = 11, cohort_name = "ARBs") %>% 
  CDMConnector::computeQuery()

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "ache_inhib_arb",
                                         dateRange = c(starting_date, ending_date),
                                         indexTable = "ache_inhibitors_com",
                                         markerTable = "arbs_com",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

ache_inhib_arb <- CohortSymmetry::getSequenceRatios(cdm = cdm, outcomeTable = "ache_inhib_arb") %>% 
  mutate(Analysis = "Test Drugs",         
         Index_name = cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
           pull(),
         Marker_name = cdm[["arbs_com"]] %>%  distinct(cohort_name) %>% 
           pull()
  ) %>% 
  readr::write_csv(file = paste0(here::here(output.folder),"/", cdm_name(cdm) ,"_" , cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() , "_" , cdm[["arbs_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() ,
                                 
                                 ".csv")) 


#antipropulsives#######################################################
cdm[["antipropulsives_com"]] <- cdm[["antipropulsives"]] %>% 
  dplyr::mutate(cohort_definition_id = 12, cohort_name = "Antipropulsives") %>%
  CDMConnector::computeQuery()

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "ache_inhib_antipropulsives",
                                         dateRange = c(starting_date, ending_date),
                                         indexTable = "ache_inhibitors_com",
                                         markerTable = "antipropulsives_com",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

ache_inhib_antipropulsives <- CohortSymmetry::getSequenceRatios(cdm = cdm, outcomeTable = "ache_inhib_antipropulsives") %>% 
  mutate(Analysis = "Test Drugs",         
         Index_name = cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
           pull(),
         Marker_name = cdm[["antipropulsives_com"]] %>%  distinct(cohort_name) %>% 
           pull()
  ) %>% 
  readr::write_csv(file = paste0(here::here(output.folder),"/", cdm_name(cdm) ,"_" , cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() , "_" , cdm[["antipropulsives_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() ,
                                 
                                 ".csv")) 

#antiemetics_5ht3 ##############################################
cdm[["antiemetics_5ht3_com"]] <- cdm[["antiemetics_5ht3"]] %>%
  dplyr::mutate(cohort_definition_id = 13, cohort_name = "Antiemetics 5HT3 antagonists") %>%
  CDMConnector::computeQuery()

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "ache_inhib_antiemetics_5ht3",
                                         dateRange = c(starting_date, ending_date),
                                         indexTable = "ache_inhibitors_com",
                                         markerTable = "antiemetics_5ht3_com",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

ache_inhib_antiemetics_5ht3 <- CohortSymmetry::getSequenceRatios(cdm = cdm, outcomeTable = "ache_inhib_antiemetics_5ht3") %>% 
  mutate(Analysis = "Test Drugs",         
         Index_name = cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
           pull(),
         Marker_name = cdm[["antiemetics_5ht3_com"]] %>%  distinct(cohort_name) %>% 
           pull()
  ) %>% 
  readr::write_csv(file = paste0(here::here(output.folder),"/", cdm_name(cdm) ,"_" , cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() , "_" , cdm[["antiemetics_5ht3_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() ,
                                 
                                 ".csv")) 


# Antipsychotics Diphenylbutylpiperidine ##############################################
# cdm[["antipsych_diphenylbutylpipe_com"]] <- cdm[["antipsych_diphenylbutylpipe"]] %>%
#   dplyr::mutate(cohort_definition_id = 9, cohort_name = "Antipsychotics Diphenylbutylpiperidine") %>%
#   CDMConnector::computeQuery()
# 
# cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
#                                          name = "ache_inhib_antipsych_diphenylbutylpipe",
#                                          dateRange = c(starting_date, ending_date),
#                                          indexTable = "ache_inhibitors_com",
#                                          markerTable = "antipsych_diphenylbutylpipe_com",
#                                          daysPriorObservation = 365,
#                                          indexWashout = 365,
#                                          markerWashout = 365,
#                                          timeGap = 365)
# 
# ache_inhib_antipsych_diphenylbutylpipe <- CohortSymmetry::getSequenceRatios(cdm = cdm, outcomeTable = "ache_inhib_antipsych_diphenylbutylpipe")


# antipsych_thioxanthene ################################################
cdm[["antipsych_thioxanthene_com"]] <- cdm[["antipsych_thioxanthene"]] %>%
  dplyr::mutate(cohort_definition_id = 14, cohort_name = "Antipsychotics Thioxanthene") %>%
  CDMConnector::computeQuery()

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "ache_inhib_antipsych_thioxanthene",
                                         dateRange = c(starting_date, ending_date),
                                         indexTable = "ache_inhibitors_com",
                                         markerTable = "antipsych_thioxanthene_com",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

ache_inhib_thioxanthene <- CohortSymmetry::getSequenceRatios(cdm = cdm, outcomeTable = "ache_inhib_antipsych_thioxanthene") %>% 
  mutate(Analysis = "Test Drugs",         
         Index_name = cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
           pull(),
         Marker_name = cdm[["antipsych_thioxanthene_com"]] %>%  distinct(cohort_name) %>% 
           pull()
  ) %>% 
  readr::write_csv(file = paste0(here::here(output.folder),"/", cdm_name(cdm) ,"_" , cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() , "_" , cdm[["antipsych_thioxanthene_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() ,
                                 
                                 ".csv")) 


# diazepines ##############################################
cdm[["antipsych_diazepines_com"]] <- cdm[["antipsych_diazepines"]] %>%
  dplyr::mutate(cohort_definition_id = 15, cohort_name = "Antipsychotics Diazepines") %>%
  CDMConnector::computeQuery()

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "ache_inhib_diazepines",
                                         dateRange = c(starting_date, ending_date),
                                         indexTable = "ache_inhibitors_com",
                                         markerTable = "antipsych_diazepines_com",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

ache_inhib_diazepines <- CohortSymmetry::getSequenceRatios(cdm = cdm, outcomeTable = "ache_inhib_diazepines") %>% 
  mutate(Analysis = "Test Drugs",         
         Index_name = cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
           pull(),
         Marker_name = cdm[["antipsych_diazepines_com"]] %>%  distinct(cohort_name) %>% 
           pull()
  ) %>% 
  readr::write_csv(file = paste0(here::here(output.folder),"/", cdm_name(cdm) ,"_" , cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() , "_" , cdm[["antipsych_diazepines_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() ,
                                 
                                 ".csv")) 


# antipsych_benzamides
cdm[["antipsych_benzamides_com"]] <- cdm[["antipsych_benzamides"]] %>%
  dplyr::mutate(cohort_definition_id = 16, cohort_name = "Antipsychotics Benzamides") %>%
  CDMConnector::computeQuery()

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "ache_inhib_benzamides",
                                         dateRange = c(starting_date, ending_date),
                                         indexTable = "ache_inhibitors_com",
                                         markerTable = "antipsych_benzamides_com",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

ache_inhib_benzamides <- CohortSymmetry::getSequenceRatios(cdm = cdm, outcomeTable = "ache_inhib_benzamides") %>% 
  mutate(Analysis = "Test Drugs",         
         Index_name = cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
           pull(),
         Marker_name = cdm[["antipsych_benzamides_com"]] %>%  distinct(cohort_name) %>% 
           pull()
  ) %>% 
  readr::write_csv(file = paste0(here::here(output.folder),"/", cdm_name(cdm) ,"_" , cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() , "_" , cdm[["antipsych_benzamides_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() ,
                                 
                                 ".csv")) 


#### unable to run ####
# antipsych_lithium
cdm[["antipsych_lithium_com"]] <- cdm[["antipsych_lithium"]] %>%
  dplyr::mutate(cohort_definition_id = 17, cohort_name = "Antipsychotics Lithium") %>%
  CDMConnector::computeQuery()

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "ache_inhib_antipsych_lithium",
                                         dateRange = c(starting_date, ending_date),
                                         indexTable = "ache_inhibitors_com",
                                         markerTable = "antipsych_lithium_com",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

ache_inhib_antipsych_lithium <- CohortSymmetry::getSequenceRatios(cdm = cdm, outcomeTable = "ache_inhib_antipsych_lithium") %>% 
  mutate(Analysis = "Test Drugs",         
         Index_name = cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
           pull(),
         Marker_name = cdm[["antipsych_lithium_com"]] %>%  distinct(cohort_name) %>% 
           pull()
  ) %>% 
  readr::write_csv(file = paste0(here::here(output.folder),"/", cdm_name(cdm) ,"_" , cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() , "_" , cdm[["antipsych_lithium_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() ,
                                 
                                 ".csv")) 


# running
# antiepileptics_hydantoin
cdm[["antiepileptics_hydantoin_com"]] <- cdm[["antiepileptics_hydantoin"]] %>%
  dplyr::mutate(cohort_definition_id = 18, cohort_name = "Antiepileptics Hydantoins") %>%
  CDMConnector::computeQuery()

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "ache_inhib_antiepileptics_hydantoin",
                                         dateRange = c(starting_date, ending_date),
                                         indexTable = "ache_inhibitors_com",
                                         markerTable = "antiepileptics_hydantoin_com",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

ache_inhib_antiepileptics_hydantoin <- CohortSymmetry::getSequenceRatios(cdm = cdm, outcomeTable = "ache_inhib_antiepileptics_hydantoin") %>% 
  mutate(Analysis = "Test Drugs",         
         Index_name = cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
           pull(),
         Marker_name = cdm[["antiepileptics_hydantoin_com"]] %>%  distinct(cohort_name) %>% 
           pull()
  ) %>% 
  readr::write_csv(file = paste0(here::here(output.folder),"/", cdm_name(cdm) ,"_" , cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() , "_" , cdm[["antiepileptics_hydantoin_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() ,
                                 
                                 ".csv")) 



# antiepileptics_barbiturates
cdm[["antiepileptics_barbiturates_com"]] <- cdm[["antiepileptics_barbiturates"]] %>%
  dplyr::mutate(cohort_definition_id = 19, cohort_name = "Antiepileptics Barbiturates") %>%
  CDMConnector::computeQuery()

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "ache_inhib_antiepileptics_barbiturates",
                                         dateRange = c(starting_date, ending_date),
                                         indexTable = "ache_inhibitors_com",
                                         markerTable = "antiepileptics_barbiturates_com",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

ache_inhib_antiepileptics_barbiturates <- CohortSymmetry::getSequenceRatios(cdm = cdm, outcomeTable = "ache_inhib_antiepileptics_barbiturates") %>% 
  mutate(Analysis = "Test Drugs",         
         Index_name = cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
           pull(),
         Marker_name = cdm[["antiepileptics_barbiturates_com"]] %>%  distinct(cohort_name) %>% 
           pull()
  ) %>% 
  readr::write_csv(file = paste0(here::here(output.folder),"/", cdm_name(cdm) ,"_" , cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() , "_" , cdm[["antiepileptics_barbiturates_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() ,
                                 
                                 ".csv")) 



# antiepileptics_succinimide
cdm[["antiepileptics_succinimide_com"]] <- cdm[["antiepileptics_succinimide"]] %>%
  dplyr::mutate(cohort_definition_id = 20, cohort_name = "Antiepileptics Succinimides") %>%
  CDMConnector::computeQuery()

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "ache_inhib_antiepileptics_succinimide",
                                         dateRange = c(starting_date, ending_date),
                                         indexTable = "ache_inhibitors_com",
                                         markerTable = "antiepileptics_succinimide_com",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

ache_inhib_antiepileptics_succinimide <- CohortSymmetry::getSequenceRatios(cdm = cdm, outcomeTable = "ache_inhib_antiepileptics_succinimide") %>% 
  mutate(Analysis = "Test Drugs",         
         Index_name = cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
           pull(),
         Marker_name = cdm[["antiepileptics_succinimide_com"]] %>%  distinct(cohort_name) %>% 
           pull()
  ) %>% 
  readr::write_csv(file = paste0(here::here(output.folder),"/", cdm_name(cdm) ,"_" , cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() , "_" , cdm[["antiepileptics_succinimide_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() ,
                                 
                                 ".csv")) 



# antiepileptics_bzds
cdm[["antiepileptics_bzds_com"]] <- cdm[["antiepileptics_bzds"]] %>%
  dplyr::mutate(cohort_definition_id = 21, cohort_name = "Antiepileptics Benzodiazepines") %>%
  CDMConnector::computeQuery()

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "ache_inhib_antiepileptics_bzds",
                                         dateRange = c(starting_date, ending_date),
                                         indexTable = "ache_inhibitors_com",
                                         markerTable = "antiepileptics_bzds_com",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

ache_inhib_antiepileptics_bzds <- CohortSymmetry::getSequenceRatios(cdm = cdm, outcomeTable = "ache_inhib_antiepileptics_bzds") %>% 
  mutate(Analysis = "Test Drugs",         
         Index_name = cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
           pull(),
         Marker_name = cdm[["antiepileptics_bzds_com"]] %>%  distinct(cohort_name) %>% 
           pull()
  ) %>% 
  readr::write_csv(file = paste0(here::here(output.folder),"/", cdm_name(cdm) ,"_" , cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() , "_" , cdm[["antiepileptics_bzds_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() ,
                                 
                                 ".csv")) 



# antiepileptics_carboxamides
cdm[["antiepileptics_carboxamides_com"]] <- cdm[["antiepileptics_carboxamides"]] %>%
  dplyr::mutate(cohort_definition_id = 22, cohort_name = "Antiepileptics Carboxamides") %>%
  CDMConnector::computeQuery()

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "ache_inhib_antiepileptics_carboxamides",
                                         dateRange = c(starting_date, ending_date),
                                         indexTable = "ache_inhibitors_com",
                                         markerTable = "antiepileptics_carboxamides_com",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

ache_inhib_antiepileptics_carboxamides <- CohortSymmetry::getSequenceRatios(cdm = cdm, outcomeTable = "ache_inhib_antiepileptics_carboxamides") %>% 
  mutate(Analysis = "Test Drugs",         
         Index_name = cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
           pull(),
         Marker_name = cdm[["antiepileptics_carboxamides_com"]] %>%  distinct(cohort_name) %>% 
           pull()
  ) %>% 
  readr::write_csv(file = paste0(here::here(output.folder),"/", cdm_name(cdm) ,"_" , cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() , "_" , cdm[["antiepileptics_carboxamides_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() ,
                                 
                                 ".csv")) 



# antidep_moins
cdm[["antidep_moins_com"]] <- cdm[["antidep_moins"]] %>%
  dplyr::mutate(cohort_definition_id = 23, cohort_name = "Antidepressants NS Monoamine oxidase inhibitors") %>%
  CDMConnector::computeQuery()

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "ache_inhib_antidep_moins",
                                         dateRange = c(starting_date, ending_date),
                                         indexTable = "ache_inhibitors_com",
                                         markerTable = "antidep_moins_com",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

ache_inhib_antidep_moins <- CohortSymmetry::getSequenceRatios(cdm = cdm, outcomeTable = "ache_inhib_antidep_moins") %>% 
  mutate(Analysis = "Test Drugs",         
         Index_name = cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
           pull(),
         Marker_name = cdm[["antidep_moins_com"]] %>%  distinct(cohort_name) %>% 
           pull()
  ) %>% 
  readr::write_csv(file = paste0(here::here(output.folder),"/", cdm_name(cdm) ,"_" , cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() , "_" , cdm[["antidep_moins_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() ,
                                 
                                 ".csv")) 



# antidep_moai
cdm[["antidep_moai_com"]] <- cdm[["antidep_moai"]] %>%
  dplyr::mutate(cohort_definition_id = 24, cohort_name = "Antidepressants Monoamine oxidase A inhibitors") %>%
  CDMConnector::computeQuery()

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "ache_inhib_antidep_moai",
                                         dateRange = c(starting_date, ending_date),
                                         indexTable = "ache_inhibitors_com",
                                         markerTable = "antidep_moai_com",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

ache_inhib_antidep_moai <- CohortSymmetry::getSequenceRatios(cdm = cdm, outcomeTable = "ache_inhib_antidep_moai") %>% 
  mutate(Analysis = "Test Drugs",         
         Index_name = cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
           pull(),
         Marker_name = cdm[["antidep_moai_com"]] %>%  distinct(cohort_name) %>% 
           pull()
  ) %>% 
  readr::write_csv(file = paste0(here::here(output.folder),"/", cdm_name(cdm) ,"_" , cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() , "_" , cdm[["antidep_moai_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() ,
                                 
                                 ".csv")) 



# antiemetics_other
cdm[["antiemetics_other_com"]] <- cdm[["antiemetics_other"]] %>%
  dplyr::mutate(cohort_definition_id = 25, cohort_name = "Other Antiemetics") %>%
  CDMConnector::computeQuery()

cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "ache_inhib_antiemetics_other",
                                         dateRange = c(starting_date, ending_date),
                                         indexTable = "ache_inhibitors_com",
                                         markerTable = "antiemetics_other_com",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

ache_inhib_antiemetics_other <- CohortSymmetry::getSequenceRatios(cdm = cdm, outcomeTable = "ache_inhib_antiemetics_other") %>% 
  mutate(Analysis = "Test Drugs",         
         Index_name = cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
           pull(),
         Marker_name = cdm[["antiemetics_other_com"]] %>%  distinct(cohort_name) %>% 
           pull()
  ) %>% 
  readr::write_csv(file = paste0(here::here(output.folder),"/", cdm_name(cdm) ,"_" , cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() , "_" , cdm[["antiemetics_other_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() ,
                                 
                                 ".csv")) 



print("Got cohort sequence test drugs")
info(logger, "Got cohort sequence test drugs")
#############################################################

print("Getting cohort sequence bzd > test drugs")
info(logger, "Getting cohort sequence bzd > test drugs")

# Benzodiazepines hypnotics and sedatives
cdm[["bzd_hs_com"]] <- cdm[["bzd_hs"]] %>%
  dplyr::mutate(cohort_definition_id = 26, cohort_name = "bzd_hs") %>%
  CDMConnector::computeQuery()

# for benzodiazepines > ache_inhibitors
cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "bzd_hs_ache_inhibitors",
                                         dateRange = c(starting_date, ending_date),
                                         indexTable = "bzd_hs_com",
                                         markerTable = "ache_inhibitors_com",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

bzd_hs_ache_inhibitors <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                                outcomeTable = "bzd_hs_ache_inhibitors") %>% 
  mutate(Analysis = "Test Drugs",         
         Index_name = cdm[["bzd_hs_com"]] %>%  distinct(cohort_name) %>% 
           pull(),
         Marker_name = cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
           pull()
  ) %>% 
  readr::write_csv(file = paste0(here::here(output.folder),"/", cdm_name(cdm) ,"_" , cdm[["bzd_hs_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() , "_" , cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() ,
                                 
                                 ".csv")) 


# bzd_anxiolytics
cdm[["bzd_anxiolytics_com"]] <- cdm[["bzd_anxiolytics"]] %>%
  dplyr::mutate(cohort_definition_id = 27, cohort_name = "bzd_anxiolytics") %>%
  CDMConnector::computeQuery()

# for bzd_anxiolytics > ache_inhibitors
cdm <- CohortSymmetry::getCohortSequence(cdm = cdm,
                                         name = "bzd_anxiolytics_ache_inhibitors",
                                         dateRange = c(starting_date, ending_date),
                                         indexTable = "bzd_anxiolytics_com",
                                         markerTable = "ache_inhibitors_com",
                                         daysPriorObservation = 365,
                                         indexWashout = 365,
                                         markerWashout = 365,
                                         timeGap = 365)

bzd_hs_ache_inhibitors <- CohortSymmetry::getSequenceRatios(cdm = cdm, 
                                                            outcomeTable = "bzd_anxiolytics_ache_inhibitors") %>% 
  mutate(Analysis = "Test Drugs",         
         Index_name = cdm[["bzd_anxiolytics_com"]] %>%  distinct(cohort_name) %>% 
           pull(),
         Marker_name = cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
           pull()
  ) %>% 
  readr::write_csv(file = paste0(here::here(output.folder),"/", cdm_name(cdm) ,"_" , cdm[["bzd_anxiolytics_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() , "_" , cdm[["ache_inhibitors_com"]] %>%  distinct(cohort_name) %>% 
                                   pull() ,
                                 
                                 ".csv")) 


print("Got cohort sequence bzd > test drugs")
info(logger, "Got cohort sequence bzd > test drugs")






