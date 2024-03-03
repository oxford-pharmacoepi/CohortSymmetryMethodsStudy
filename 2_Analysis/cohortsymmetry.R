########################
# positive controls
########################

tictoc::tic()
cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                         name = "amiodarone_levothyroxine",
                                         cohortDateRange = c(starting_date, ending_date),
                                         indexTable = "amiodarone",
                                         markerTable = "levothyroxine",
                                         daysPriorObservation = 365,
                                         washoutWindow = 365,
                                         indexMarkerGap = NULL, # if null it uses the second argument of the combinationWindow
                                         combinationWindow = c(0, 365))

getCohortSeqtime <- tictoc::toc()$callback_msg

tictoc::tic()
amiodarone_levothyroxin <- CohortSymmetry::summariseSequenceRatio(cdm = cdm, 
                                                                  sequenceCohortSet = "amiodarone_levothyroxine")

amiodarone_levothyroxin_results <- CohortSymmetry::tidySequenceSymmetry(result = amiodarone_levothyroxin)

getSeqRatioTime <- tictoc::toc()$callback_msg

##############################
# negative controls
##############################
print("Getting cohort sequence negative controls")
info(logger, "Getting cohort sequence negative controls")

#Amiodarone	Allopurinol
tictoc::tic()
cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                         name = "amiodarone_allopurinol",
                                         cohortDateRange = c(starting_date, ending_date),
                                         indexTable = "amiodarone",
                                         markerTable = "allopurinol",
                                         daysPriorObservation = 365,
                                         washoutWindow = 365,
                                         indexMarkerGap = NULL, # if null it uses the second argument of the combinationWindow
                                         combinationWindow = c(0, 365))
getCohortSeqtime <- tictoc::toc()$callback_msg

tictoc::tic()
amiodarone_allopurinol <- CohortSymmetry::summariseSequenceRatio(cdm = cdm, sequenceCohortSet = "amiodarone_allopurinol")
amiodarone_allopurinol_results <- CohortSymmetry::tidySequenceSymmetry(result = amiodarone_allopurinol)


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


#########################
# # Aplastic Anemia
#########################
# put all drugs in one table
cdm <- omopgenerics::bind(cdm$desloratadine, cdm$fluvastatin, cdm$irbesartan, cdm$latanoprost, cdm$timolol,
                          cdm$allopurinol, cdm$captopril, cdm$carbamazepine, cdm$methimazole, cdm$ticlopidine,
                          name = "test_drugs1"
)

# Aplastic Anemia
cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 name = "anemia_benchmarkers",
                                                 cohortDateRange = c(starting_date, ending_date),
                                                 indexTable = "test_drugs",
                                                 markerTable = "aplastic_anemia",
                                                 daysPriorObservation = 365,
                                                 washoutWindow = 365,
                                                 indexMarkerGap = NULL, # if null it uses the second argument of the combinationWindow
                                                 combinationWindow = c(0, 365))
#summarise sequence ratio
anemia_benchmarkers <- CohortSymmetry::summariseSequenceRatio(cdm = cdm, sequenceCohortSet = "anemia_benchmarkers")
# extract results
anemia_benchmarkers_results <- CohortSymmetry::tidySequenceSymmetry(result = anemia_benchmarkers)

#######################
# Acute Renal Failure
#######################

#########################
# # Acute Liver Failure
#########################

################################
# # Acute Myocardial Infarction
################################


##############################
# drugs to test with gi
##############################

# put all drugs in one table
cdm <- omopgenerics::bind(cdm$dorzolamide, cdm$eszopiclone, cdm$fexofenadine, cdm$goserelin, cdm$simvastatin,
  cdm$aspirin, cdm$heparin, cdm$ibuprofen, cdm$indomethacin, cdm$prednisolone,
  name = "test_drugs"
)

# test gi ulcer
cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                         name = "gi_benchmarkers",
                                         cohortDateRange = c(starting_date, ending_date),
                                         indexTable = "test_drugs",
                                         markerTable = "upper_gi_ulcer",
                                         daysPriorObservation = 365,
                                         washoutWindow = 365,
                                         indexMarkerGap = NULL, # if null it uses the second argument of the combinationWindow
                                         combinationWindow = c(0, 365))
#summarise sequence ratio
upper_gi_ulcer_benchmarkers <- CohortSymmetry::summariseSequenceRatio(cdm = cdm, sequenceCohortSet = "gi_benchmarkers")

# extract results
upper_gi_ulcer_benchmarkers_results <- CohortSymmetry::tidySequenceSymmetry(result = upper_gi_ulcer_benchmarkers)


####################
# #  Anaphylaxis 
###################

##################################
# # Stevens-Johnson Syndrome 
##################################


################
# # Neutropenia 
#################


####################
# # Rhabdomyolysis
####################

############################
# # Cardiac Valve Fibrosis
############################

###########
# # cough
###########







