cli::cli_alert_info("- Getting benchmarker definitions drug - drug")

# positive controls -------
cli::cli_alert_info("- Getting benchmarker definitions drug - drug positive controls")

cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("amiodarone", "ingredient")),
                           table_name = "amiodarone",
                           start_date = starting_date,
                           end_date = ending_date)

cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("levothyroxine", "ingredient")),
                           table_name = "levothyroxine",
                           start_date = starting_date,
                           end_date = ending_date)


cli::cli_alert_success("- Got benchmarker definitions drug - drug")

# negative controls -------
cli::cli_alert_info("- Getting benchmarker definitions drug - drug negative controls")

cdm <- getSingleDrugCohort(cdm = cdm,
                           drug = list(c("allopurinol", "ingredient")),
                           table_name = "allopurinol",
                           start_date = starting_date,
                           end_date = ending_date)

cli::cli_alert_success("- Got benchmarker definitions drug - drug")


cli::cli_alert_info("- Getting benchmarker definitions drug - condition")
# from EUADR method evaluation package
# Acute Renal Failure
renal_failure <- c(197320, 432961)
cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                              conceptSet = list("renal_failure" = renal_failure),
                                              end = "observation_period_end_date",
                                              limit = "all",
                                              name = "renal_failure",
                                              overwrite = TRUE)

# Acute Liver Failure
liver_failure <- c(438878 , 200451, 194984,
                   197917, 434887, 436238, 
                   4029488, 194417, 194087, 
                   200449, 4159144, 196455, 
                   194990, 137977)

cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                              conceptSet = list("liver_failure" = liver_failure),
                                              end = "observation_period_end_date",
                                              limit = "all",
                                              name = "liver_failure",
                                              overwrite = TRUE)

# Aplastic Anemia
aplastic_anemia <- c(138723, 137829, 140065)
cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                              conceptSet = list("aplastic_anemia" = aplastic_anemia),
                                              end = "observation_period_end_date",
                                              limit = "all",
                                              name = "aplastic_anemia",
                                              overwrite = TRUE)

# Acute Myocardial Infarction 
myocardial_infarction <- c(434376, 438438, 438170, 
                           438447, 441579, 436706, 
                           444406, 321318, 315296, 
                           439693 )

cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                              conceptSet = list("myocardial_infarction" = myocardial_infarction),
                                              end = "observation_period_end_date",
                                              limit = "all",
                                              name = "myocardial_infarction",
                                              overwrite = TRUE)

# Upper GastrointestinaI Ulcer 1
upper_gi_ulcer <- c(28779, 4114486, 26441,
              437326, 24076, 22665,
              30770, 24397, 195309,
              23245, 26727, 437027,
              201059, 316457, 193809,
              194158 )

cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                              conceptSet = list("upper_gi_ulcer" = upper_gi_ulcer),
                                              end = "observation_period_end_date",
                                              limit = "all",
                                              name = "upper_gi_ulcer",
                                              overwrite = TRUE)

#  Anaphylaxis 1
anaphylaxis <- c(441202)

cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                              conceptSet = list("anaphylaxis" = anaphylaxis),
                                              end = "observation_period_end_date",
                                              limit = "all",
                                              name = "anaphylaxis",
                                              overwrite = TRUE)

# Stevens-Johnson Syndrome 2
sj_syndrome <- c(439414, 443754,
                 141651, 132702)

cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                              conceptSet = list("sj_syndrome" = sj_syndrome),
                                              end = "observation_period_end_date",
                                              limit = "all",
                                              name = "sj_syndrome",
                                              overwrite = TRUE)

# Neutropenia 1
neutropenia <- c(314617,434895,432297,
                 443909,432289,316376,
                 436957,435775,4121123,
                 321533,320074,432283,
                 438398,435224,441541,
                 434910,320073,432589,
                 440367,434008,320682)

cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                              conceptSet = list("neutropenia" = neutropenia),
                                              end = "observation_period_end_date",
                                              limit = "all",
                                              name = "neutropenia",
                                              overwrite = TRUE)

# Rhabdomyolysis
rhabdomyolysis <- c(4345578)
cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                              conceptSet = list("rhabdomyolysis" = rhabdomyolysis),
                                              end = "observation_period_end_date",
                                              limit = "all",
                                              name = "rhabdomyolysis",
                                              overwrite = TRUE)

# Cardiac Valve Fibrosis 
cardiac_valve_fibrosis <- c(320116, 314054, 315564,
                            321041, 319843, 319845,
                            4175807)
cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                              conceptSet = list("cardiac_valve_fibrosis" = cardiac_valve_fibrosis),
                                              end = "observation_period_end_date",
                                              limit = "all",
                                              name = "cardiac_valve_fibrosis",
                                              overwrite = TRUE)


