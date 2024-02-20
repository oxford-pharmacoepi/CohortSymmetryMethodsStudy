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
cli::cli_alert_info("- Getting benchmarker definitions conditions")
# Acute Renal Failure
renal_failure <- c(197320, 432961)
cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                              conceptSet = list("renal_failure" = renal_failure),
                                              end = "observation_period_end_date",
                                              limit = "all",
                                              name = "renal_failure",
                                              overwrite = TRUE)
cdm$renal_failure <- cdm$renal_failure %>% 
  filter(cohort_start_date >= starting_date & cohort_start_date <= ending_date)

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
cdm$liver_failure <- cdm$liver_failure %>% 
  filter(cohort_start_date >= starting_date & cohort_start_date <= ending_date)

# Aplastic Anemia
aplastic_anemia <- c(138723, 137829, 140065)
cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                              conceptSet = list("aplastic_anemia" = aplastic_anemia),
                                              end = "observation_period_end_date",
                                              limit = "all",
                                              name = "aplastic_anemia",
                                              overwrite = TRUE)
cdm$aplastic_anemia <- cdm$aplastic_anemia %>% 
  filter(cohort_start_date >= starting_date & cohort_start_date <= ending_date)

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
cdm$myocardial_infarction <- cdm$myocardial_infarction %>% 
  filter(cohort_start_date >= starting_date & cohort_start_date <= ending_date)

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
cdm$upper_gi_ulcer <- cdm$upper_gi_ulcer %>% 
  filter(cohort_start_date >= starting_date & cohort_start_date <= ending_date)

#  Anaphylaxis 1
anaphylaxis <- c(441202)

cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                              conceptSet = list("anaphylaxis" = anaphylaxis),
                                              end = "observation_period_end_date",
                                              limit = "all",
                                              name = "anaphylaxis",
                                              overwrite = TRUE)
cdm$anaphylaxis <- cdm$anaphylaxis %>% 
  filter(cohort_start_date >= starting_date & cohort_start_date <= ending_date)

# Stevens-Johnson Syndrome 2
sj_syndrome <- c(439414, 443754,
                 141651, 132702)

cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                              conceptSet = list("sj_syndrome" = sj_syndrome),
                                              end = "observation_period_end_date",
                                              limit = "all",
                                              name = "sj_syndrome",
                                              overwrite = TRUE)
cdm$sj_syndrome <- cdm$sj_syndrome %>% 
  filter(cohort_start_date >= starting_date & cohort_start_date <= ending_date)

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
cdm$neutropenia <- cdm$neutropenia %>% 
  filter(cohort_start_date >= starting_date & cohort_start_date <= ending_date)

# Rhabdomyolysis
rhabdomyolysis <- c(4345578)
cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                              conceptSet = list("rhabdomyolysis" = rhabdomyolysis),
                                              end = "observation_period_end_date",
                                              limit = "all",
                                              name = "rhabdomyolysis",
                                              overwrite = TRUE)
cdm$rhabdomyolysis <- cdm$rhabdomyolysis %>% 
  filter(cohort_start_date >= starting_date & cohort_start_date <= ending_date)

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
cdm$cardiac_valve_fibrosis <- cdm$cardiac_valve_fibrosis %>% 
  filter(cohort_start_date >= starting_date & cohort_start_date <= ending_date)

# get drug list for benchmarkers
data(euadrReferenceSet)
euadrReferenceSet$exposureId
drugs <- unique(euadrReferenceSet$exposureName) %>% 
  tolower() %>% 
  as.character()


get_cohorts <- function() {

  # create a loop that instanstiates each drug cohort
  cli_progress_bar("Instanstiating cohorts", total = length(drugs))
  
  for (i in 1:length(drugs)) {

    Sys.sleep(10/100)
    
    cdm <- getSingleDrugCohort(cdm = cdm,
                               drug = list(
                                 c(drugs[i],"ingredient")),
                               table_name = drugs[i],
                               start_date = starting_date,
                               end_date = ending_date)
    
    cli_progress_update()
    
    success_message <- paste("- Benchmarker Cohorts generated for CohortSymmetry for", drugs[i])
    
    # Print the success message
    cli::cli_alert_success(success_message)
  }
  
  cli_progress_done()
  
}

get_cohorts()



# cdm <- getSingleDrugCohort(cdm = cdm,
#                            drug = list(
#                              c("amiodarone","ingredient"), #ok
#                                         c("desloratadine","ingredient"), 
#                                        c("fluvastatin","ingredient"),
#                                         c("irbesartan","ingredient"),        
#                                         c("latanoprost","ingredient"),  
#                                         c("timolol","ingredient"),  #ok
#                                         c("allopurinol","ingredient"),
#                                         c("captopril","ingredient"),
#                                         c("carbamazepine", "ingredient"),
#                                         c("methimazole", "ingredient"),
#                                         c("ticlopidine", "ingredient"),
#                                         c("carteolol", "ingredient"),
#                                         c("formoterol","ingredient"),
#                                         c("levodopa","ingredient"),
#                                         c("nitroglycerin","ingredient"),
#                                         c("terazosin", "ingredient"),
#                                         c("amoxicillin", "ingredient"),
#                                         c("sulfasalazine", "ingredient"),
#                                         c("valproate", "ingredient"), # ok
#                                         c("ferrous sulfate", "ingredient"),
#                                         c("fexofenadine",  "ingredient"),
#                                         c("mometasone",  "ingredient"),
#                                         c("levothyroxine", "ingredient"), 
#                                         c("acetaminophen", "ingredient"), 
#                                        c("ciprofloxacin" , "ingredient"), 
#                                        c("ibuprofen", "ingredient"), 
#                                         c("lithium carbonate", "ingredient"), # ok
#                                         c("gemfibrozil",   "ingredient"), 
#                                         c("insulin, regular, human" ,   "ingredient") ,
#                                         c("valacyclovir" ,  "ingredient"),
#                                         c("levonorgestrel" , "ingredient"),
#                                         c("rofecoxib", "ingredient"),
#                                         c("rosiglitazone", "ingredient"),
#                                         c("sumatriptan", "ingredient"),
#                                         c("valdecoxib" , "ingredient"),
#                                         c("dorzolamide", "ingredient"),
#                                         c("eszopiclone", "ingredient"),
#                                         c("goserelin", "ingredient"),
#                                         c("simvastatin", "ingredient"),
#                                         c("aspirin", "ingredient"),
#                                         c("heparin" ,"ingredient"),
#                                         c("indomethacin" , "ingredient"),
#                                         c("prednisolone","ingredient"),
#                                         c("clonidine", "ingredient"),
#                                         c("doxazosin", "ingredient"),
#                                         c("mirtazapine", "ingredient"),
#                                         c("oxazepam","ingredient"),
#                                         c("diclofenac","ingredient"),
#                                         c("atenolol", "ingredient"),
#                                         c("felodipine" , "ingredient"),
#                                         c("ipratropium", "ingredient"),
#                                         c("propafenone", "ingredient"), #ok
#                                         c("tiotropium" , "ingredient"),
#                                         c("furosemide" , "ingredient"),
#                                         c("lamotrigine","ingredient"),
#                                         c("sulfamethoxazole","ingredient"),
#                                         c("atorvastatin" , "ingredient"),
#                                         c("isosorbide","ingredient"),
#                                         c("sotalol", "ingredient"),
#                                         c("tamsulosin",  "ingredient"),
#                                         c("estradiol", "ingredient"),
#                                         c("glimepiride", "ingredient"), #ok
#                                         c("pravastatin", "ingredient"),
#                                         c("rosuvastatin" , "ingredient"),
#                                         c("fluvoxamine","ingredient"),
#                                        c("methotrexate", "ingredient")),
#                            table_name = "benchmarkers",
#                            start_date = starting_date,
#                            end_date = ending_date)
