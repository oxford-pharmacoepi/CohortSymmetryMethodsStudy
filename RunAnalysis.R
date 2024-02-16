#start the clock
start<-Sys.time()

# create logger
log_file <- paste0(output.folder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

# if you have already instantiated cohorts you can get them back
instantiatedCohorts <- TRUE

if (instantiatedCohorts == TRUE) {
  
  cdm <- CDMConnector::cdm_from_con(con = db, 
                                    cdm_schema = cdm_database_schema,
                                    write_schema = c("schema" = results_database_schema, 
                                                     "prefix" = table_stem),
                                    cdm_name = db.name, 
                                    cohort_tables = c(
                                      #"amiodarone",
                                      #"levothyroxine",
                                      #"allopurinol",
                                      "ache_inhibitors",
                                      #"memantine",
                                      #"loperamide" ,
                                      #"diuretics", # older version
                                      #"beta_blockers", # older version
                                      #"ccb", # older version
                                      #"ace_inhibitors", # older version
                                      #"arbs",
                                      #"antipropulsives",
                                      #"antipsych_diphenylbutylpipe",
                                      #"antiemetics_5ht3",
                                      #"bladder_anticholinergics,
                                      "ache_inhib_antipsych_lithium",
                                      "antipsych_thioxanthene",
                                      "antipsych_diazepines",
                                      "antipsych_benzamides",
                                      #"antipsych_lithium",
                                      "antiepileptics_hydantoin",
                                      "antiepileptics_barbiturates",
                                      "antiepileptics_succinimide",
                                      "antiepileptics_bzds",
                                      "antiepileptics_carboxamides",
                                      "antidep_moins",
                                      "antidep_moai" ,
                                      "antiemetics_other"))
  
  
}


###################
# get functions
###################
source(here("Functions.R"))

##############################
# Hypothesis driven approach
##############################
# generating cohorts for PSSA
info(logger, "1 GENERATING COHORTS FOR PSSA")
print(paste0("1 Generating cohorts for PSSA at ", Sys.time()))
source(here("1_InstantiateCohorts", "CohortPSSA.R"))
info(logger, "1 GENERATING COHORTS FOR PSSA IS DONE")
print(paste0("1 Generating cohorts for PSSA is done at ", Sys.time()))

# carrying out PSSA
info(logger, "2 CARRYING OUT PSSA")
print(paste0("2 Carrying out for PSSA at ", Sys.time()))
source(here("2_Analysis", "RunPSSA.R"))
info(logger, "2 COMPLETED FOR PSSA")
print(paste0("2 Completed PSSA at ", Sys.time()))

# tidy up results
print(paste0("Saving PSSA results at ", Sys.time()))
info(logger, "SAVING RESULTS")
readr::write_csv(amiodarone_levothyroxin, paste0(here::here(output.folder),"/", cdm_name(cdm), "PSSA_amiodarone_levothyroxin.csv"))
readr::write_csv(ache_inhibitors_memantine, paste0(here::here(output.folder),"/", cdm_name(cdm), "PSSA_ache_inhibitors_memantine.csv"))
readr::write_csv(ache_inhibitors_com_memantine, paste0(here::here(output.folder),"/", cdm_name(cdm), "PSSA_ache_inhibitors_com_memantine.csv"))
readr::write_csv(amiodarone_allopurinol, paste0(here::here(output.folder),"/", cdm_name(cdm), "PSSA_amiodarone_allopurinol.csv"))
readr::write_csv(levothyroxine_allopurinol, paste0(here::here(output.folder),"/", cdm_name(cdm), "PSSA_levothyroxine_allopurinol.csv"))
readr::write_csv(ache_inhibitors_com_test_drugs, paste0(here::here(output.folder),"/", cdm_name(cdm), "PSSA_ache_inhibitors_com_test_drugs.csv"))
readr::write_csv(memantine_test_drugs, paste0(here::here(output.folder),"/", cdm_name(cdm), "PSSA_memantine_com_test_drugs.csv"))

#benzodiazepines_ache_inhibitors
#benzodiazepines_memantine
print(paste0("SAVED RESULTS")) 


##############################
# data driven approach
##############################
# generating cohorts for PSSA
# info(logger, "3 GENERATING COHORTS FOR PSSA")
# print(paste0("3 Generating cohorts for PSSA at ", Sys.time()))
# source(here("1_InstantiateCohorts", "CohortPSSA_datadriven.R"))
# info(logger, "3 GENERATING COHORTS FOR PSSA IS DONE")
# print(paste0("3 Generating cohorts for PSSA is done at ", Sys.time()))
# 
# # carrying out PSSA
# info(logger, "4 CARRYING OUT PSSA")
# print(paste0("4 Carrying out for PSSA at ", Sys.time()))
# source(here("2_Analysis", "RunPSSA_datadriven.R"))
# info(logger, "4 COMPLETED FOR PSSA")
# print(paste0("4 Completed PSSA at ", Sys.time()))

# getting results


# zip results
print("Zipping results to output folder")

zip::zip(
  zipfile = here::here(output.folder, paste0("Results_", cdmName(cdm), ".zip")),
  files = list.files(output.folder),
  root = output.folder)

print("Study done!")
print(paste0("Study took: ",
             sprintf("%02d:%02d:%02d:%02d",
                     x %/% 86400,  x %% 86400 %/% 3600, x %% 3600 %/%
                       60,  x %% 60 %/% 1)))
print("-- If all has worked, there should now be a zip folder with your results in the Results folder to share")
print("-- Thank you for running the study! :)")

Sys.time()-start

readLines(log_file)