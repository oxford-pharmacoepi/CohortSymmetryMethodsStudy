source(here("2_Analysis", "helpers.R"))

# Set output folder location -----
# the path to a folder where the results from this analysis will be saved
output_folder <- here("Results", db_name)

# output files ---- 
if (!file.exists(output_folder)){
  dir.create(output_folder, recursive = TRUE)}

createLogger(output_folder, db_name)

# get cdm snapshot
log(" - Getting cdm snapshot")
OmopSketch::exportSummarisedResult(
  OmopSketch::summariseOmopSnapshot(cdm),
  fileName = here(output_folder, paste0("/", db_name, "_cdm_snapshot_.csv")),
  path = output_folder
)

if (instantiatedCohorts == TRUE) {
  log(" - Retrieving instantiated cohorts")
  cdm <- CDMConnector::cdm_from_con(con = db, 
                                    cdm_schema = cdm_database_schema,
                                    write_schema = c("schema" = results_database_schema, 
                                                     "prefix" = table_stem),
                                    cohort_tables = c("amiodarone",
                                                      "levothyroxine",
                                                      "allopurinol",
                                                      bm_conditions,
                                                      ingredient_events,
                                                      atc_event_name)
  )
  
} else {
  
  log("- Cohort generation for CohortSymmetry")
  source(here("1_InstantiateCohorts","InstantiateCohorts.R"))
  log("- Cohorts generated for CohortSymmetry")
  
}

# run main analysis ------------
if(isTRUE(run_symmetry)){
log("- Running cohort symmetry")
tryCatch({
  source(here("2_Analysis", "CohortSymmetry.R"))
}, error = function(e) {
  writeLines(as.character(e),
             here(output_folder, paste0("/", db_name,
                                    
                                    "_error_cohortsymmetry.txt")))
})
}

# varying parameters ------------
if(isTRUE(run_symmetry_vary_parameter)){
  log("- Running PSSA whilst varying parameters")
  tryCatch({
    source(here("2_Analysis", "ParameterVariations.R"))
  }, error = function(e) {
    writeLines(as.character(e),
               here(output_folder, paste0("/", db_name,
                                      
                                      "_error_parameter_variation.txt")))
  })
}

# characterisation analysis -----
if(isTRUE(run_characterisation)){
log("- Running Characterisation")
  tryCatch({
    source(here("2_Analysis", "characterisation.R"))
  }, error = function(e) {
    writeLines(as.character(e),
               here(output_folder, paste0("/", db_name,
                                      
                                      "_error_characterisation.txt")))
  })
}


# zip results ----
log("- Zipping Results")
# zip all results
zip::zip(
  zipfile = file.path(here(output_folder,
                           paste0("Results_", db_name, ".zip"))),
  files = list.files(here(output_folder)),
  root = output_folder)

cli::cli_alert_success("- Study Done!")
cli::cli_alert_success("- If all has worked, there should now be a zip folder with your results in the Results folder to share")
cli::cli_alert_success("- Thank you for running the study! :)")
