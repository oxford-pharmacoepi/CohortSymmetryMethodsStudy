# get functions for study
source(here("Functions.R"))

# Set output folder location -----
# the path to a folder where the results from this analysis will be saved
output_folder <- here("Results", db_name)

# output files ---- 
if (!file.exists(output_folder)){
  dir.create(output_folder, recursive = TRUE)}

# get cdm snapshot
cli::cli_alert_info("- Getting cdm snapshot")
write_csv(snapshot(cdm), here("Results", paste0(db_name,
                                                "/", cdmName(cdm), "_cdm_snapshot_.csv"
)))


# if you have already instantiated cohorts you can get them back
instantiatedCohorts <- FALSE

if (instantiatedCohorts == TRUE) {
  
  cdm <- CDMConnector::cdm_from_con(con = db, 
                                    cdm_schema = cdm_database_schema,
                                    write_schema = c("schema" = results_database_schema, 
                                                     "prefix" = table_stem),
                                    cdm_name = db.name, 
                                    cohort_tables = c(
                                      "amiodarone",
                                      "levothyroxine",
                                      "allopurinol"
                                     ))
  
  
} else {
  
  cli::cli_alert_info("- Cohort generation for CohortSymmetry")
  source(here("1_InstantiateCohorts","instantiatecohorts.R"))
  cli::cli_alert_success("- Cohorts generated for CohortSymmetry")
}


# run main analysis ------------
if(isTRUE(run_symmetry)){
cli::cli_alert_info("- Running cohort symmetry")
tryCatch({
  source(here("2_Analysis", "cohortsymmetry.R"))
}, error = function(e) {
  writeLines(as.character(e),
             here("Results", paste0(db_name,
                                    "/", cdmName(cdm),
                                    
                                    "_error_cohortsymmetry.txt")))
})
}

# characterisation analysis -----
if(isTRUE(run_characterisation)){
cli::cli_alert_info("- Running Characterisation")
  tryCatch({
    source(here("2_Analysis", "characterisation.R"))
  }, error = function(e) {
    writeLines(as.character(e),
               here("Results", paste0(db_name,
                                      "/", cdmName(cdm),
                                      
                                      "_error_characterisation.txt")))
  })
}


# zip results ----
cli::cli_alert_info("- Zipping Results")
# zip all results
zip::zip(
  zipfile = file.path(here("Results", db_name,
                           paste0("Results_", db_name, ".zip"))),
  files = list.files(here("Results", db_name)),
  root = output_folder)

cli::cli_alert_success("- Study Done!")
cli::cli_alert_success("- If all has worked, there should now be a zip folder with your results in the Results folder to share")
cli::cli_alert_success("- Thank you for running the study! :)")
