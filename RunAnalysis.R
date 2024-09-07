# create logger
log_file <- paste0(output_folder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

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

if (instantiatedCohorts == TRUE) {
  
  cdm <- CDMConnector::cdm_from_con(con = db, 
                                    cdm_schema = "public_100k",
                                    write_schema = c("schema" = results_database_schema, 
                                                     "prefix" = table_stem),
                                    cohort_tables = c("amiodarone",
                                                      "levothyroxine",
                                                      "allopurinol",
                                                      bm_conditions,
                                                      ingredient_events,
                                                      atc_event_name))
  
} else {
  
  cli::cli_alert_info("- Cohort generation for CohortSymmetry")
  source(here("1_InstantiateCohorts","InstantiateCohorts.R"))
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
