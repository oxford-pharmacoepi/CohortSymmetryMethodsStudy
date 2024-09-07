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
amiodarone_levothyroxine <- CohortSymmetry::summariseSequenceRatios(cdm$amiodarone_levothyroxine)

amiodarone_levothyroxine_results <- CohortSymmetry::tableSequenceRatios(result = amiodarone_levothyroxine)

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
amiodarone_allopurinol <- CohortSymmetry::summariseSequenceRatios(cdm$amiodarone_allopurinol)
amiodarone_allopurinol_results <- CohortSymmetry::tableSequenceRatios(result = amiodarone_allopurinol)

getSeqRatioTime <- tictoc::toc()$callback_msg

amiodarone_allopurinol <- amiodarone_allopurinol %>% 
  mutate(CohortSequenceTime = getCohortSeqtime,
         SeqRatioTime = getSeqRatioTime)

benchmarkers <- bind_rows(
  amiodarone_levothyroxine,
  amiodarone_allopurinol
)

# save results
readr::write_csv(benchmarkers, 
                 paste0(here::here(output_folder),"/", cdm_name(cdm), "_21224_SSA_benchmarkers.csv"))

bm_conditions_test <- bm_conditions[[1]]


#########################
# # Aplastic Anemia
#########################
# put all drugs in one table
cdm <- omopgenerics::bind(cdm$desloratadine, cdm$fluvastatin, cdm$irbesartan, cdm$latanoprost, cdm$timolol,
                          cdm$allopurinol, cdm$captopril, cdm$carbamazepine, cdm$methimazole, cdm$ticlopidine,
                          name = "test_drugs")

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
anemia_benchmarkers <- CohortSymmetry::summariseSequenceRatio(cdm = cdm, sequenceTable = "anemia_benchmarkers")
# extract results
#anemia_benchmarkers_results <- CohortSymmetry::tableSequenceRatios(result = anemia_benchmarkers)

#########################
# # Acute Liver Failure
#########################
# put all drugs in one table
cdm <- omopgenerics::bind(cdm$carteolol, cdm$formoterol, cdm$levodopa, cdm$nitroglycerin, cdm$terazosin,
                          cdm$amoxicillin, cdm$carbamazepine, cdm$sulfasalazine, cdm$valproate, cdm$ticlopidine,
                          name = "test_drugs1")

cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 name = "liver_benchmarkers",
                                                 cohortDateRange = c(starting_date, ending_date),
                                                 indexTable = "test_drugs1",
                                                 markerTable = "liver_failure",
                                                 daysPriorObservation = 365,
                                                 washoutWindow = 365,
                                                 indexMarkerGap = NULL, # if null it uses the second argument of the combinationWindow
                                                 combinationWindow = c(0, 365))
#summarise sequence ratio
liver_benchmarkers <- CohortSymmetry::summariseSequenceRatio(cdm = cdm, sequenceTable = "liver_benchmarkers")
# extract results
#liver_benchmarkers_results <- CohortSymmetry::tableSequenceRatios(result = liver_benchmarkers)


#######################
# Acute Renal Failure
#######################
# put all drugs in one table
cdm <- omopgenerics::bind(cdm$`ferrous sulfate`, cdm$fexofenadine, cdm$levodopa, cdm$mometasone, cdm$levothyroxine,
                          cdm$acetaminophen, cdm$captopril, cdm$ciprofloxacin, cdm$ibuprofen, cdm$`lithium carbonate`,
                          name = "test_drugs2"
)

# renal failure
cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 name = "renal_benchmarkers",
                                                 cohortDateRange = c(starting_date, ending_date),
                                                 indexTable = "test_drugs2",
                                                 markerTable = "renal_failure",
                                                 daysPriorObservation = 365,
                                                 washoutWindow = 365,
                                                 indexMarkerGap = NULL, # if null it uses the second argument of the combinationWindow
                                                 combinationWindow = c(0, 365))
#summarise sequence ratio
renal_benchmarkers <- CohortSymmetry::summariseSequenceRatio(cdm = cdm, sequenceTable = "renal_benchmarkers")
# extract results
#renal_benchmarkers_results <- CohortSymmetry::tableSequenceRatios(result = renal_benchmarkers)


################################
# # Acute Myocardial Infarction
################################
# put all drugs in one table
cdm <- omopgenerics::bind(cdm$`ferrous sulfate`, cdm$amoxicillin, cdm$`insulin, regular, human`, cdm$gemfibrozil, cdm$valacyclovir,
                          cdm$levonorgestrel, cdm$rofecoxib, cdm$rosiglitazone, cdm$sumatriptan, cdm$valdecoxib,
                          name = "test_drugs3"
)

cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 name = "mci_benchmarkers",
                                                 cohortDateRange = c(starting_date, ending_date),
                                                 indexTable = "test_drugs3",
                                                 markerTable = "myocardial_infarction",
                                                 daysPriorObservation = 365,
                                                 washoutWindow = 365,
                                                 indexMarkerGap = NULL, # if null it uses the second argument of the combinationWindow
                                                 combinationWindow = c(0, 365))
#summarise sequence ratio
mci_benchmarkers <- CohortSymmetry::summariseSequenceRatio(cdm = cdm, sequenceTable = "mci_benchmarkers")
# extract results
#mci_benchmarkers_results <- CohortSymmetry::tableSequenceRatios(result = mci_benchmarkers)

##############################
# gi ulcers
##############################

# put all drugs in one table
cdm <- omopgenerics::bind(cdm$dorzolamide, cdm$eszopiclone, cdm$fexofenadine, cdm$goserelin, cdm$simvastatin,
  cdm$aspirin, cdm$heparin, cdm$ibuprofen, cdm$indomethacin, cdm$prednisolone,
  name = "test_drugs4"
)

# test gi ulcer
cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                         name = "gi_benchmarkers",
                                         cohortDateRange = c(starting_date, ending_date),
                                         indexTable = "test_drugs4",
                                         markerTable = "upper_gi_ulcer",
                                         daysPriorObservation = 365,
                                         washoutWindow = 365,
                                         indexMarkerGap = NULL, # if null it uses the second argument of the combinationWindow
                                         combinationWindow = c(0, 365))
#summarise sequence ratio
upper_gi_ulcer_benchmarkers <- CohortSymmetry::summariseSequenceRatio(cdm = cdm, sequenceTable = "gi_benchmarkers")

# extract results
#upper_gi_ulcer_benchmarkers_results <- CohortSymmetry::tableSequenceRatios(result = upper_gi_ulcer_benchmarkers)


####################
# #  Anaphylaxis 
###################
# put all drugs in one table
cdm <- omopgenerics::bind(cdm$clonidine, cdm$doxazosin, cdm$mirtazapine, cdm$oxazepam, cdm$levothyroxine,
                          
                          cdm$acetaminophen, cdm$amoxicillin, cdm$aspirin, cdm$ciprofloxacin, cdm$diclofenac,
                          name = "test_drugs5"
)

# Anaphylaxis 
cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 name = "anaphylaxis_benchmarkers",
                                                 cohortDateRange = c(starting_date, ending_date),
                                                 indexTable = "test_drugs4",
                                                 markerTable = "anaphylaxis",
                                                 daysPriorObservation = 365,
                                                 washoutWindow = 365,
                                                 indexMarkerGap = NULL, # if null it uses the second argument of the combinationWindow
                                                 combinationWindow = c(0, 365))
#summarise sequence ratio
anaphylaxis_benchmarkers <- CohortSymmetry::summariseSequenceRatio(cdm = cdm, sequenceTable = "anaphylaxis_benchmarkers")

# extract results
#anaphylaxis_benchmarkers_benchmarkers_results <- CohortSymmetry::tableSequenceRatios(result = anaphylaxis_benchmarkers)



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







